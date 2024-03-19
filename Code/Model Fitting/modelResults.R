# packages ----

library(tidyverse)
library(sf)
library(INLA)

# directories ----

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, '/')[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-3)], collapse = '/')
code.dir <- paste0(home.dir, '/Code')
data.dir <- paste0(home.dir, '/Data')
res.dir <- paste0(home.dir, '/Results')
data.spatial.dir <- paste0(data.dir, '/shapeFiles')
res.model.fit.dir <- paste0(res.dir, '/Model Fit')
res.main.analysis.dir <- paste0(res.dir, '/Main Analysis')

# saving ----

height <- width <- 10
text.size <- 20
save.image <- TRUE

# import ----

## functions ----

source(paste0(code.dir, '/functions.R'))

## spatial ----

### raw ----

# spatial polygons at lsoa and lad levels
poly.lsoa <- sf::st_read(dsn = data.spatial.dir, layer = 'ONS11_LSOA')
poly.lad <- sf::st_read(dsn = data.spatial.dir, layer = 'ONS22_LAD')
poly.nat <- sf::st_read(dsn = data.spatial.dir, layer = 'ONS21_NAT')

### organised ----

poly.lsoa.england <- poly.lsoa %>%  dplyr::filter(str_detect(LSOA11CD, '^E'))
poly.lad.england <- poly.lad %>%  dplyr::filter(str_detect(LAD22CD, '^E'))
poly.nat.england <- poly.nat %>%  dplyr::filter(str_detect(CTRY21CD, '^E'))

spatialPlotSimple <- FALSE
if(spatialPlotSimple){
  # less detailed but quicker plots
  poly.lsoa.england <-
    poly.lsoa.england %>%
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)
  poly.lad.england <-
    poly.lad.england %>%
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)
  poly.nat.england <-
    poly.nat.england %>%
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)
}

## main results ----

setwd(res.model.fit.dir)
data.final <- readRDS(file = 'modelData.rds')
fit <- readRDS(file = 'modelFit.rds')
theta.predictor <- readRDS(file = 'linearPredictorSamples.rds')
theta.observation <- readRDS(file = 'observationSamples.rds')
theta.parameter <- readRDS(file = 'parameterSamples.rds')
theta.space <- readRDS(file = 'spatialRandomEffectSamples.rds')
theta.time <- readRDS(file = 'temporalRandomEffectSamples.rds')

## additional data ----

setwd(data.dir)
load('Organised Data/LAD22_2001_2021_IMD.rda')
load('Organised Data/LSOA11_2001_2021_IMD.rda')

# linear predictor based results ----

setwd(res.main.analysis.dir)

## Before-after ----

### temporal ----

before.after.temporal.data <-
  theta.predictor %>%
  dplyr::mutate(time = (lubridate::interval(start = ucStartDate, end = interviewDate) %/% months(3))) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'time')) %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:'))

before.after.temporal.plot <-
  ggplot2::ggplot(data = before.after.temporal.data, aes(x = time/4, y = median, group = exposed, colour = exposed, fill = exposed)) +
  ggplot2::geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, colour = NA) +
  ggplot2::scale_colour_manual(values = c('blue3', 'red3')) +
  ggplot2::scale_fill_manual(values = c('blue3', 'red3')) +
  # ggplot2::scale_y_continuous(limits = c(0, 36), breaks = seq(0, 36, by = 6)) +
  ggplot2::scale_y_continuous(limits = c(10, 25), breaks = seq(10, 25, by = 5)) +
  ggplot2::scale_x_continuous(limits = c(-11, 4), breaks = -11:4, labels = c(-11:-1, 'Start', 1:4)) +
  ggplot2::labs(x = 'Time since awareness to Universal Credit (years)', 
                y = 'Self reported mental ill health (GHQ-12)') +
  my.theme(legend.title = element_blank(),
           legend.position = 'bottom',
           text = element_text(size = text.size)); before.after.temporal.plot

if(save.image){
  ggplot2::ggsave(filename = 'beforeAfter_temporalPlot.png',
                  plot = before.after.temporal.plot,
                  height = height, width = width)
}

### spatial ----

#### overall ---- 

before.after.overall.control.spatial.lad.data <- 
  # overall before-after by exposure group
  theta.predictor %>%
  # filter out control
  dplyr::filter(exposed == 'Control') %>% 
  # average score by exposure, treatment and ltla
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment', 'LAD22CD')) %>% 
  # exposed-treatment label
  dplyr::mutate(exposed.treatment = interaction(exposed, treatment, sep = '')) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::reframe(difference = value[exposed.treatment == 'Control1'] - value[exposed.treatment == 'Control0'],
                 .by = c('LAD22CD', 'theta')) %>% 
  # revert theta to columns
  tidyr::pivot_wider(names_from = 'theta', values_from = 'difference', names_prefix = 'theta:', names_sep = '') %>%
  # group before and after UC
  dplyr::mutate(exposed = 'Control',
                timeFrame = 'Overall')

before.after.overall.exposed.spatial.lad.data <- 
  # overall before-after by exposure group
  theta.predictor %>%
  # filter out exposed
  dplyr::filter(exposed == 'Exposed') %>% 
  # average score by exposure, treatment and ltla
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment', 'LAD22CD')) %>% 
  # exposed-treatment label
  dplyr::mutate(exposed.treatment = interaction(exposed, treatment, sep = '')) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::reframe(difference = value[exposed.treatment == 'Exposed1'] - value[exposed.treatment == 'Exposed0'],
                 .by = c('LAD22CD', 'theta')) %>% 
  # revert theta to columns
  tidyr::pivot_wider(names_from = 'theta', values_from = 'difference', names_prefix = 'theta:', names_sep = '') %>%
  # group before and after UC
  dplyr::mutate(exposed = 'Exposed',
                timeFrame = 'Overall')

#### immediate ----

before.after.immediate.control.spatial.lad.data <- 
  # immediate before-after by exposure group
  theta.predictor %>%
  # filter out control and year before-after
  dplyr::filter(time %in% -12:12,
                exposed == 'Control') %>% 
  # average score by exposure, treatment and ltla
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment', 'LAD22CD')) %>% 
  # exposed-treatment label
  dplyr::mutate(exposed.treatment = interaction(exposed, treatment, sep = '')) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::reframe(difference = value[exposed.treatment == 'Control1'] - value[exposed.treatment == 'Control0'],
                 .by = c('LAD22CD', 'theta')) %>% 
  # revert theta to columns
  tidyr::pivot_wider(names_from = 'theta', values_from = 'difference', names_prefix = 'theta:', names_sep = '') %>%
  # group before and after UC
  dplyr::mutate(exposed = 'Control',
                timeFrame = 'Immediate')

before.after.immediate.exposed.spatial.lad.data <- 
  # immediate before-after by exposure group
  theta.predictor %>%
  # filter out exposed and year before-after
  dplyr::filter(time %in% -12:12,
                exposed == 'Exposed') %>% 
  # average score by exposure, treatment and ltla
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment', 'LAD22CD')) %>% 
  # exposed-treatment label
  dplyr::mutate(exposed.treatment = interaction(exposed, treatment, sep = '')) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::reframe(difference = value[exposed.treatment == 'Exposed1'] - value[exposed.treatment == 'Exposed0'],
                 .by = c('LAD22CD', 'theta')) %>% 
  # revert theta to columns
  tidyr::pivot_wider(names_from = 'theta', values_from = 'difference', names_prefix = 'theta:', names_sep = '') %>%
  # group before and after UC
  dplyr::mutate(exposed = 'Exposed',
                timeFrame = 'Immediate')

#### overall and immediate together ----

before.after.spatial.lad.data <-
  dplyr::bind_rows(before.after.overall.control.spatial.lad.data,
                   before.after.overall.exposed.spatial.lad.data,
                   before.after.immediate.control.spatial.lad.data,
                   before.after.immediate.exposed.spatial.lad.data) %>% 
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                timeFrame = timeFrame %>% factor(., levels = c('Overall', 'Immediate'), labels = c('All years', 'One year')),
                exposed = exposed %>% factor(., levels = c('Exposed', 'Control'))) %>%
  dplyr::select(-starts_with('theta:')) %>%
  dplyr::left_join(., poly.lad.england, by = 'LAD22CD') %>%
  sf::st_as_sf()

#### plot ----

before.after.spatial.lad.plot <- 
  ggplot2::ggplot() +
  # England outline filled in with white - must be first
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  # estimate
  ggplot2::geom_sf(data = before.after.spatial.lad.data, aes(fill = median), colour = 'black') +
  # facet by group and frame
  ggh4x::facet_nested(exposed ~ timeFrame, switch = 'y') + 
  # set colour
  ggplot2::scale_fill_gradient2(name = 'Change in\nself reported\nmental ill health\n(GHQ-12)',
                                low = 'blue3', mid = 'grey', high = 'red3',
                                midpoint = 0) +
  my.map.theme(text = element_text(size = text.size),
               legend.position = 'bottom',
               legend.key.width = unit(2, 'cm'))
if(spatialPlotSimple){before.after.spatial.lad.plot}

if(save.image){
  ggplot2::ggsave(filename = 'beforeAfter_spatialPlot_LAD22.png',
                  plot = before.after.spatial.lad.plot,
                  height = height, width = width)
}

# standardised change ----

setwd(res.main.analysis.dir)

## spatial ----

### lad ----

imd.01.21.average.lad <-
  imd.01.21.lad %>%
  dplyr::filter(YEAR %in% 2009:2021) %>%
  dplyr::summarise(imdScore = imdScore %>% mean(),
                   .by = 'LAD22CD') %>%
  # define rank
  dplyr::arrange(dplyr::desc(imdScore)) %>%
  dplyr::mutate(imdRank = 1:n(),
                DEPRIVATION = dplyr::ntile(x = imdRank, n = 10) %>%
                  factor(levels = 1:10, labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)')))

standardised.change.spatial.lad.data <-
  theta.predictor %>%
  # average over exposure-treatment-ltla
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment', 'LAD22NM')) %>%
  # exposed-treatment label
  dplyr::mutate(exposed.treatment = interaction(exposed, treatment, sep = '')) %>%
  # need theta to be one colum not rows
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::summarise(rho.EA = value[exposed.treatment == 'Exposed1'],
                   rho.tilde.EB = value[exposed.treatment == 'Exposed0'] * value[exposed.treatment == 'Control1'] / value[exposed.treatment == 'Control0'],
                   rho = (rho.EA - rho.tilde.EB) / rho.tilde.EB * 100,
                   .by = c('LAD22NM', 'theta')) %>% 
  dplyr::select(theta, LAD22NM, rho) %>% 
  # revert theta to columns
  tidyr::pivot_wider(names_from = 'theta', values_from = 'rho', names_prefix = 'theta:', names_sep = '') %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:')) %>%
  dplyr::left_join(., poly.lad.england, by = 'LAD22NM') %>%
  dplyr::left_join(., imd.01.21.average.lad %>% dplyr::select(LAD22CD, DEPRIVATION), by = 'LAD22CD') %>% 
  dplyr::arrange(median) %>%
  dplyr::mutate(space = LAD22NM %>% haven::as_factor(),
                space_id = space %>% as.numeric()) %>%
  sf::st_as_sf()

standardised.change.spatial.lad.data.largest <-
  standardised.change.spatial.lad.data %>% 
  dplyr::filter(median == max(median)) %>% 
  dplyr::mutate(label = paste0('Increase:\n', LAD22NM)) 

standardised.change.spatial.lad.data.smallest <- 
  standardised.change.spatial.lad.data %>% 
  dplyr::filter(median == min(median)) %>% 
  dplyr::mutate(label = paste0('Decrease:\n', LAD22NM))

### national ----

standardised.change.nat.data <- 
  theta.predictor %>%
  # average over exposure-treatment
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment')) %>%
  # exposed-treatment label
  dplyr::mutate(exposed.treatment = interaction(exposed, treatment, sep = '')) %>%
  # need theta to be in one column not rows
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::summarise(rho.EA = value[exposed.treatment == 'Exposed1'],
                   rho.tilde.EB = value[exposed.treatment == 'Exposed0'] * value[exposed.treatment == 'Control1'] / value[exposed.treatment == 'Control0'],
                   rho = (rho.EA - rho.tilde.EB) / rho.tilde.EB * 100,
                   .by = c('theta')) %>% 
  dplyr::select(theta, rho) %>% 
  # revert theta to columns
  tidyr::pivot_wider(names_from = 'theta', values_from = 'rho', names_prefix = 'theta:', names_sep = '') %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:'))

### plot ----

standardised.change.spatial.lad.plot.map <- 
  ggplot2::ggplot() +
  # England outline filled in with white - must be first
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  # estimate and CI width
  ggplot2::geom_sf(data = standardised.change.spatial.lad.data, aes(fill = median), colour = 'black') +
  # largest and smallest StandChange
  ## largest 
  ggrepel::geom_text_repel(data = standardised.change.spatial.lad.data.largest,
                           aes(label = label, geometry = geometry),
                           stat = 'sf_coordinates',
                           size = 8,
                           nudge_x = 210000 - standardised.change.spatial.lad.data.largest$BNG_E,
                           nudge_y = 450000 - standardised.change.spatial.lad.data.largest$BNG_N,
                           min.segment.length = 0) +
  ## smallest 
  ggrepel::geom_text_repel(data = standardised.change.spatial.lad.data.smallest,
                           aes(label = label, geometry = geometry),
                           stat = 'sf_coordinates', 
                           size = 8,
                           nudge_x = 210000 - standardised.change.spatial.lad.data.smallest$BNG_E,
                           nudge_y = 210000 - standardised.change.spatial.lad.data.smallest$BNG_N,
                           min.segment.length = 0) +
  # set colour
  ggplot2::scale_fill_gradient2(name = 'Standardised \nchange (%)',
                                low = 'blue3', mid = 'grey', high = 'red3',
                                midpoint = 0, n.breaks = 6) +
  my.map.theme(text = element_text(size = text.size),
               legend.position = 'bottom',
               legend.key.width = unit(2, 'cm'))
if(spatialPlotSimple){standardised.change.spatial.lad.plot.map}

standardised.change.spatial.lad.plot.line <-
  ggplot2::ggplot(data = 
                    standardised.change.spatial.lad.data %>% 
                    dplyr::mutate(DEPRIVATION2 = dplyr::case_when(DEPRIVATION %in% c('1 (Most Deprived)', 2) ~ '1 (Most Deprived) and 2',
                                                                  DEPRIVATION %in% 3:8 ~ '3 - 8',
                                                                  DEPRIVATION %in% c(9, '10 (Least Deprived)') ~ '9 and 10 (Least Deprived)',
                                                                  TRUE ~ NA)), 
                  aes(x = space_id, y = median, color = DEPRIVATION2)) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), width = .05) +
  ggplot2::geom_hline(yintercept = 0, color = 'black', linetype = 'dashed') +
  # set colour
  ggplot2::scale_color_manual(name = 'Deprivation', values = c('red3', 'grey', 'blue3')) +
  ggplot2::labs(x = 'Lower Tier Local Authority', y = 'Standardised change (%)') +
  # largest and smallest StandChange
  ## largest 
  ggrepel::geom_label_repel(data = standardised.change.spatial.lad.data.largest,
                            aes(label = label), colour = 'black',
                            nudge_x = -75, nudge_y = 0, size = 8) +
  ## smallest 
  ggrepel::geom_label_repel(data = standardised.change.spatial.lad.data.smallest,
                            aes(label = label), colour = 'black',
                            nudge_x = 75, nudge_y = -0, size = 8) +
  # national results
  ggplot2::geom_hline(yintercept = standardised.change.nat.data$median, color = 'black', linetype = 'solid') +
  ggplot2::annotate(geom = 'text',
                    label = paste0('National: ', round(standardised.change.nat.data$median, 2), 
                                   '% \n(', round(standardised.change.nat.data$lower, 2), '% - ', round(standardised.change.nat.data$upper, 2), '%)'),
                    x = nrow(standardised.change.spatial.lad.data)/4, 
                    y = 10,
                    color = 'black',
                    size = 8) +
  my.theme(axis.ticks.x = element_blank(),
           axis.text.x = element_blank(),
           legend.position = 'bottom',
           text = element_text(size = text.size)); standardised.change.spatial.lad.plot.line

if(save.image){
  ggplot2::ggsave(filename = 'standardisedChange_spatialPlot_LAD22_map.png',
                  plot = standardised.change.spatial.lad.plot.map,
                  height = height, width = width) 
  ggplot2::ggsave(filename = 'standardisedChange_spatialPlot_LAD22_line.png',
                  plot = standardised.change.spatial.lad.plot.line,
                  height = height, width = width) 
}

## all confounders ----

age.level.order <- c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)')
edu.level.order <- c('Degree or higher', 'GCSE, A-level or equivalent', 'Below GCSE and other')
eth.level.order <- c('Asian', 'Black', 'Mixed', 'Other', 'White')
rela.level.order <- c('Single', 'Relationship')
sex.level.order <- c('Male', 'Female')

standardised.change.confounder.data <- 
  theta.predictor %>%
  # turn the columns for each confounder into one column
  tidyr::pivot_longer(cols = c('age', 'edu', 'ethn', 'rela', 'sex', 'deprivation', 'diversity'), names_to = 'parameter', values_to = 'parameter.level') %>% 
  # average over exposure-treatment-confouder
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment', 'parameter', 'parameter.level')) %>%
  # label for exposed.treament
  dplyr::mutate(exposed.treatment = interaction(exposed, treatment, sep = '')) %>%
  # need thetas to be rows not columns
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::summarise(rho.EA = value[exposed.treatment == 'Exposed1'],
                   rho.tilde.EB = value[exposed.treatment == 'Exposed0'] * value[exposed.treatment == 'Control1'] / value[exposed.treatment == 'Control0'],
                   rho = (rho.EA - rho.tilde.EB) / rho.tilde.EB * 100,
                   .by = c('parameter', 'parameter.level', 'theta')) %>% 
  dplyr::select(theta, parameter, parameter.level, rho) %>% 
  # thetas back to columns 
  tidyr::pivot_wider(names_from = 'theta', values_from = 'rho', names_prefix = 'theta:', names_sep = '') %>%
  # summarise
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                parameter = parameter %>% factor(., levels = c('age', 'edu', 'ethn', 'rela', 'sex', 'deprivation', 'diversity'), labels = c('Age', 'Education', 'Ethnicity', 'Relationship', 'Sex', 'Deprivation', 'Diversity')), 
                parameter.level = parameter.level %>% factor(., levels = c(age.level.order, edu.level.order, eth.level.order, rela.level.order, sex.level.order, 1:10))) %>%
  dplyr::select(-starts_with('theta:')) %>% 
  dplyr::arrange(parameter, parameter.level) %>% 
  dplyr::mutate(index = 1:n())

standardised.change.confounder.plot<- 
  ggplot2::ggplot(standardised.change.confounder.data, aes(y = index, x = median)) +
  ggplot2::geom_vline(xintercept = 0, colour = 'red3', linetype = 'dashed') +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(aes(xmin = lower, xmax = upper), width = .05) +
  ggplot2::scale_y_continuous(labels = standardised.change.confounder.data$parameter.level,
                              breaks = 1:31,
                              trans = 'reverse') +
  ggplot2::labs(x = 'Standardised change (%)', y = '') +
  ggplot2::geom_segment(data = 
                          data.frame(x = rep(-Inf, times = 5), xend = rep(9, times = 5), 
                                     y = c(5.5, 8.5, 13.5, 15.5, 27.5), 
                                     yend = c(5.5, 8.5, 13.5, 15.5, 27.5)),
                        aes(x = x, xend = xend, y = y, yend = yend), colour = 'grey') + 
  ggplot2::annotate("text",
                    x = rep(-1, lenght.out = 7),
                    # angle = 270,
                    y = c(1, 6, 9, 14, 16, 18, 28),
                    label = c('Age', 'Eduation', 'Ethnicity', 'Relationship', 'Sex', 'Deprivation', 'Diversity'),
                    fontface = 'bold',
                    colour = 'grey',
                    size = 4) +
  ggplot2::geom_hline(yintercept = c(17.5)) +
  ggplot2::annotate("text",
                    x = rep(9.5, lenght.out = 2),
                    angle = 270,
                    y = c(8.5, 24),
                    label = c('Individual level\nconfounders', 'Community level\nconfounders'),
                    fontface = 'bold',
                    size = 4) +
  my.theme(legend.title = element_blank(),
           text = element_text(size = text.size),
           legend.position = 'bottom'); standardised.change.confounder.plot

if(save.image){
  ggplot2::ggsave(filename = 'standardisedChange_confounderPlot.png',
                  plot = standardised.change.confounder.plot,
                  height = height, width = 1.25*width)  
}

## joint individual-community confounder ----

age.level.order <- c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)')
eth.level.order <- c('Degree or higher', 'GCSE, A-level or equivalent', 'Below GCSE and other')
edu.level.order <- c('Asian', 'Black', 'Mixed', 'Other', 'White')
rela.level.order <- c('Single', 'Relationship')
sex.level.order <- c('Male', 'Female')

standardised.change.joint.confounder.data <- 
  theta.predictor %>%
  # turn the columns for each confounder into one for indivdual level and one for community level
  tidyr::pivot_longer(cols = c('age', 'edu', 'ethn', 'rela', 'sex'), names_to = 'individual.parameter', values_to = 'individual.parameter.level') %>% 
  tidyr::pivot_longer(cols = c('deprivation', 'diversity'), names_to = 'community.parameter', values_to = 'community.parameter.level') %>% 
  # interactions between parameters and their levels
  dplyr::mutate(joint.parameter = interaction(individual.parameter, community.parameter),
                joint.parameter.level = interaction(individual.parameter.level, community.parameter.level)) %>%
  # average over exposure-treatment-joint confouders
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment', 'joint.parameter', 'joint.parameter.level')) %>% 
  # label for exposed.treament
  dplyr::mutate(exposed.treatment = interaction(exposed, treatment, sep = '')) %>%
  # need thetas to be rows not columns
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::summarise(rho.EA = value[exposed.treatment == 'Exposed1'],
                   rho.tilde.EB = value[exposed.treatment == 'Exposed0'] * value[exposed.treatment == 'Control1'] / value[exposed.treatment == 'Control0'],
                   rho = (rho.EA - rho.tilde.EB) / rho.tilde.EB * 100,
                   .by = c('joint.parameter', 'joint.parameter.level', 'theta')) %>% 
  dplyr::select(theta, joint.parameter, joint.parameter.level, rho) %>% 
  # thetas back to columns 
  tidyr::pivot_wider(names_from = 'theta', values_from = 'rho', names_prefix = 'theta:', names_sep = '') %>%
  # summarise
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:')) %>% 
  # seperate out the columns defined by interaction
  tidyr::separate_wider_delim(joint.parameter, delim = '.', names = c('individual.parameter', 'community.parameter')) %>% 
  tidyr::separate_wider_delim(joint.parameter.level, delim = '.', names = c('individual.parameter.level', 'community.parameter.level')) %>% 
  # naming for plot
  dplyr::mutate(individual.parameter = 
                  individual.parameter %>% factor(., levels = c('age', 'edu', 'ethn', 'rela', 'sex'), labels = c('Age', 'Education', 'Ethnicity', 'Relationship', 'Sex')),
                individual.parameter.level = 
                  individual.parameter.level %>% factor(., levels = c(age.level.order, eth.level.order, edu.level.order, rela.level.order, sex.level.order)),
                community.parameter = community.parameter %>% factor(., levels = c('deprivation', 'diversity'), labels = c('Deprivation', 'Diversity')),
                community.parameter.level = community.parameter.level %>% factor(., levels = 1:10))

standardised.change.joint.confounder.plot <-
  ggplot2::ggplot(data = standardised.change.joint.confounder.data, aes(x = community.parameter.level, y = individual.parameter.level, group = individual.parameter)) +
  ggplot2::geom_tile(aes(fill = median), colour = 'black') +
  ggplot2::scale_fill_gradient2(name = 'Standardised \nchange (%)',
                                low = 'blue3', mid = 'grey', high = 'red3',
                                midpoint = 0, n.breaks = 4, na.value = 'white') +
  ggplot2::facet_grid(individual.parameter ~ community.parameter, scales = 'free') +
  ggplot2::scale_y_discrete(limits = rev) + 
  ggplot2::labs(x = '', y = '') + 
  my.theme(text = element_text(size = text.size),
           legend.position = 'bottom',
           legend.key.width = unit(2, 'cm')); standardised.change.joint.confounder.plot

if(save.image){
  ggplot2::ggsave(filename = 'standardisedChange_jointConfounderPlot.png',
                  plot = standardised.change.joint.confounder.plot,
                  height = height, width = width)
}

# parameter based results ----

setwd(res.main.analysis.dir)

## all parameters table ----

all.parameter.data <-
  data.frame(
    covariate =
      c(rep('Control', times = 4),
        rep('Difference', times = 4),
        rep('Age', times = 4),
        rep('Education', times = 2),
        rep('Ethnicity', times = 4),
        rep('Relationship', times = 1),
        rep('Sex', times = 1),
        rep('Deprivation', times = 9),
        rep('Diversity', times = 3),
        rep('Random effect', times = 6)),
    covariateLevel =
      c('Intercept', 'Time', 'Intervention', 'Time+',
        'Intercept', 'Time', 'Intervention', 'Time+',
        '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)',
        'GCSE, A-level or equivalent', 'Degree or higher',
        'Asian', 'Black', 'Mixed', 'Other',
        'Relationship',
        'Female',
        '1 (Most Deprived)', '2', '3', '4', '5', '6', '7', '8', '9',
        '2', '3', '4 (Most Diverse)',
        'Gaussian precision', 'Strata precision', 'Cluster precision', 'Temporal precision', 'Spatial precision', 'Spatial mixing'),
    value = c(paste0(fit$summary.fixed[,4] %>% round(., digits = 4), ' (', 
                     fit$summary.fixed[,3] %>% round(., digits = 4), ', ', 
                     fit$summary.fixed[,5] %>% round(., digits = 4), ')'),
              paste0(fit$summary.hyperpar[,4] %>% round(., digits = 4), ' (', 
                     fit$summary.hyperpar[,3] %>% round(., digits = 4), ', ', 
                     fit$summary.hyperpar[,5] %>% round(., digits = 4), ')')))

print(x = xtable::xtable(all.parameter.data,
                         digits = 4), 
      include.rownames = FALSE,
      file = 'parameterTable_allParametes.tex')

## fixed parameters plot ----

fixed.parameter.reference.data <- 
  data.frame(level = c('age_id[16, 25):1', 'edu_idBelow GCSE and other:1', 'eth_idWhite:1', 'rela_idSingle:1', 'sex_idMale:1', 'deprivation_id10:1', 'diversity_id1:1'),
             mean = rep(0, times = 7),
             variance = rep(0, times = 7),
             lower = rep(0, times = 7),
             median = rep(0, times = 7),
             upper = rep(0, times = 7))

fixed.parameter.data <-
  theta.parameter %>% 
  dplyr::filter(!startsWith(level, 'strata_id'),
                !startsWith(level, 'psu_id'),
                !startsWith(level, 'time.month_id'),
                !startsWith(level, 'space_id')) %>%  
  # relative risk and then summarise
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:')) %>% 
  dplyr::bind_rows(., fixed.parameter.reference.data) %>% 
  cbind(., 
        covariateLevel = 
          c('Intercept', 'Time', 'Intervention', 'Time+',
            'Intercept', 'Time', 'Intervention', 'Time+',
            '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)',
            'GCSE, A-level or equivalent', 'Degree or higher',
            'Asian', 'Black', 'Mixed', 'Other',
            'Relationship',
            'Female',
            '1 (Most Deprived)', '2', '3', '4', '5', '6', '7', '8', '9',
            '2', '3', '4 (Most Diverse)',
            'Ref. [16, 25)', 'Ref. Below GCSE and other', 'Ref. White', 'Ref. Single', 'Ref. Male', 'Ref. 10 (Least Deprived)', 'Ref. 1 (Least Diverse)'),
        covariate = c(rep('Control', times = 4),
                      rep('Difference', times = 4),
                      rep('Age', times = 4),
                      rep('Education', times = 2),
                      rep('Ethnicity', times = 4),
                      rep('Relationship', times = 1),
                      rep('Sex', times = 1),
                      rep('Deprivation', times = 9),
                      rep('Diversity', times = 3),
                      'Age', 'Education', 'Ethnicity', 'Relationship', 'Sex', 'Deprivation', 'Diversity')) %>%
  dplyr::mutate(index = c(1:8, 10:13, 15:16, 17:20, 23, 25, 26:34, 37:39, 9, 14, 21, 22, 24, 35, 36)) %>% 
  dplyr::arrange(index)

fixed.parameter.table <- 
  fixed.parameter.data %>% 
  dplyr::mutate(value = paste0(round(median, digits = 4), ' (', round(lower, digits = 4), ', ', round(upper, digits = 4), ')')) %>% 
  dplyr::select(covariate, covariateLevel, value)

fixed.parameter.plot.axis.names <- 
  fixed.parameter.data %>%
  dplyr::select(covariate, covariateLevel, index) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(label = dplyr::case_when(covariate == 'Control' & covariateLevel == 'Intercept' ~ r'(Intercept, $\beta_0$)',
                                         covariate == 'Control' & covariateLevel == 'Time' ~ r'(Time, $\beta_1$)',
                                         covariate == 'Control' & covariateLevel == 'Intervention' ~ r'(Intervention, $\beta_2$)',
                                         covariate == 'Control' & covariateLevel == 'Time+' ~ r'(Time$^{+}$, $\beta_3$)',
                                         covariate == 'Difference' & covariateLevel == 'Intercept' ~ r'(Intercept, $\beta_4$)',
                                         covariate == 'Difference' & covariateLevel == 'Time' ~ r'(Time, $\beta_5$)',
                                         covariate == 'Difference' & covariateLevel == 'Intervention' ~ r'(Intervention, $\beta_6$)',
                                         covariate == 'Difference' & covariateLevel == 'Time+' ~ r'(Time$^{+}$, $\beta_7$)',
                                         TRUE ~ covariateLevel))

fixed.parameter.plot <-
  ggplot2::ggplot(fixed.parameter.data, aes(y = index, x = median)) +
  ggplot2::geom_vline(xintercept = 0, colour = 'red3', linetype = 'dashed') +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(aes(xmin = lower, xmax = upper), width = .05) +
  ggplot2::scale_y_continuous(labels = unname(c(latex2exp::TeX(fixed.parameter.plot.axis.names %>% dplyr::filter(covariate %in% c('Control', 'Difference')) %>% dplyr::pull(label)),
                                                fixed.parameter.plot.axis.names %>% dplyr::filter(!(covariate %in% c('Control', 'Difference'))) %>% dplyr::pull(label))),
                              breaks = 1:nrow(fixed.parameter.plot.axis.names),
                              trans = 'reverse') +
  ggplot2::labs(x = 'Parameter', y = '') +
  ggplot2::geom_segment(data = 
                          data.frame(x = rep(-Inf, times = 6), xend = rep(11.5, times = 6), 
                                     y = c(4.5, 13.5, 16.5, 21.5, 23.5, 35.5), 
                                     yend = c(4.5, 13.5, 16.5, 21.5, 23.5, 35.5)),
                        aes(x = x, xend = xend, y = y, yend = yend), colour = 'grey') + 
  ggplot2::annotate('text',
                    x = rep(-2.5, times = 9),
                    y = c(1, 5, 9, 14, 17, 22, 24, 26, 36),
                    label = c('Control', 'Difference',
                              'Age', 'Eduation', 'Ethnicity', 'Relationship', 'Sex', 
                              'Deprivation', 'Diversity'),
                    fontface = 'bold',
                    colour = 'grey',
                    size = 4,
                    hjust = 0) +
  ggplot2::geom_hline(yintercept = c(8.5, 25.5)) +
  ggplot2::annotate("text",
                    x = rep(12.5, lenght.out = 3),
                    angle = 270,
                    y = c(4, 17, 32.5),
                    label = c('Interrupted time\nseries', 'Individual level\nconfounders', 'Community level\nconfounders'),
                    fontface = 'bold',
                    size = 4) +
  my.theme(legend.title = element_blank(),
           text = element_text(size = text.size),
           legend.position = 'bottom'); fixed.parameter.plot

fixed.parameter.data.non.its <-
  fixed.parameter.data %>% 
  dplyr::filter(!(covariate %in% c('Control', 'Difference'))) %>% 
  dplyr::mutate(index = 1:n())

fixed.parameter.plot.non.its <-
  ggplot2::ggplot(fixed.parameter.data.non.its, aes(y = index, x = median)) +
  ggplot2::geom_vline(xintercept = 0, colour = 'red3', linetype = 'dashed') +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(aes(xmin = lower, xmax = upper), width = .05) + 
  ggplot2::scale_y_continuous(labels = fixed.parameter.data.non.its$covariateLevel,
                              breaks = 1:nrow(fixed.parameter.data.non.its),
                              trans = 'reverse') +
  ggplot2::labs(x = 'Parameter', y = '') +
  ggplot2::geom_segment(data = 
                          data.frame(x = rep(-Inf, times = 6), xend = rep(1.4, times = 6), 
                                     y = c(5.5, 8.5, 13.5, 15.5, 17.5, 27.5), 
                                     yend = c(5.5, 8.5, 13.5, 15.5, 17.5, 27.5)),
                        aes(x = x, xend = xend, y = y, yend = yend), colour = 'grey') + 
  ggplot2::annotate('text',
                    x = rep(-1.5, times = 7),
                    y = c(1, 6, 9, 14, 16, 18, 28),
                    label = c('Age', 'Eduation', 'Ethnicity', 'Relationship', 'Sex', 
                              'Deprivation', 'Diversity'),
                    fontface = 'bold',
                    colour = 'grey',
                    size = 4,
                    hjust = 0) +
  ggplot2::geom_hline(yintercept = 17.5) +
  ggplot2::annotate('text',
                    x = rep(1.5, lenght.out = 2),
                    angle = 270,
                    y = c(9, 24.5),
                    label = c('Individual level\nconfounders', 'Community level\nconfounders'),
                    fontface = 'bold',
                    size = 4) +
  my.theme(legend.title = element_blank(),
           text = element_text(size = text.size),
           legend.position = 'bottom'); fixed.parameter.plot.non.its

print(x = xtable::xtable(fixed.parameter.table,
                         digits = 4), 
      include.rownames = FALSE,
      file = 'parameterTable_fixedParametes.tex')

if(save.image){
  ggplot2::ggsave(filename = 'parameterPlot_fixedParamters.png',
                  plot = fixed.parameter.plot,
                  height = height, width = 1.25*width) 
}

if(save.image){
  ggplot2::ggsave(filename = 'parameterPlot_fixedParamters_nonITS.png',
                  plot = fixed.parameter.plot.non.its,
                  height = height, width = 1.25*width) 
}

## its parameter table ----

its.control.parameter.data <-
  theta.parameter %>% 
  dplyr::filter(level %in% c('(Intercept):1', 'time_id:1', 'treatment_id:1', 'timeSinceTreatment_id:1')) %>% 
  dplyr::mutate(level = level %>% factor(., 
                                         level = c('(Intercept):1', 'time_id:1', 'treatment_id:1', 'timeSinceTreatment_id:1'),
                                         label = c('beta0', 'beta1', 'beta2', 'beta3')),
                itsTerm = dplyr::case_when(level == 'beta0' ~ 'Intercept',
                                           level == 'beta1'~ 'Time',
                                           level == 'beta2' ~ 'Intervention',
                                           level == 'beta3' ~ 'Time+',
                                           TRUE ~ NA),
                type = 'Control')

its.differece.parameter.data <-
  theta.parameter %>% 
  dplyr::filter(level %in% c('exposed_id2:1', 'time.exposed_id:1', 'treatment.exposed_id:1', 'timeSinceTreatment.exposed_id:1')) %>% 
  dplyr::mutate(level = level %>% factor(., 
                                         level = c('exposed_id2:1', 'time.exposed_id:1', 'treatment.exposed_id:1', 'timeSinceTreatment.exposed_id:1'),
                                         label = c('beta4', 'beta5', 'beta6', 'beta7')),
                itsTerm = dplyr::case_when(level == 'beta4' ~ 'Intercept',
                                           level == 'beta5'~ 'Time',
                                           level == 'beta6' ~ 'Intervention',
                                           level == 'beta7' ~ 'Time+',
                                           TRUE ~ NA),
                type = 'Difference')

its.exposed.parameter.data <-
  dplyr::bind_rows(its.control.parameter.data,
                   its.differece.parameter.data) %>% 
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), sum),
                   .by = 'itsTerm') %>% 
  dplyr::mutate(level = c('beta0 + beta4', 'beta1 + beta5', 'beta2 + beta6', 'beta3 + beta7') %>% factor(),
                type = 'Exposed')

its.parameter.data <- 
  dplyr::bind_rows(its.control.parameter.data,
                   its.differece.parameter.data,
                   its.exposed.parameter.data) %>% 
  dplyr::relocate(c('type', 'itsTerm'), .before = level) %>% 
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, function(x) { 100*data.frame(exceedence = mean(x > 0))}) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                type = type %>% factor(., levels = c('Control', 'Difference', 'Exposed'))) %>%
  dplyr::select(-starts_with('theta:')) %>% 
  dplyr::mutate(parameter = paste0(itsTerm, ', ', level),
                value = paste0(round(median, digits = 4), ' (', round(lower, digits = 4), ', ', round(upper, digits = 4), ')')) %>% 
  dplyr::select(parameter, value, exceedence)

print(x = xtable::xtable(its.parameter.data, digits = 4), 
      include.rownames = FALSE,
      file = 'parameterTable_itsParamters.tex')

# residuals ----

setwd(res.main.analysis.dir)

## temporal ----

residual.temporal.data <- 
  theta.time %>% 
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                year = time / 12) %>%
  dplyr::select(-starts_with('theta:'))

residual.temporal.plot <-
  ggplot2::ggplot(data = residual.temporal.data, aes(x = year, y = median)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, colour = NA) +
  ggplot2::scale_x_continuous(limits = c(-11, 5), breaks = -11:5, labels = c(-11:-1, 'Start', 1:5)) +
  ggplot2::geom_vline(xintercept = 0, colour = 'red3', linetype = 'dashed') +
  ggplot2::labs(x = 'Time since awareness to Universal Credit (years)', 
                y = 'Residual variation in time') +
  my.theme(legend.title = element_blank(),
           legend.position = 'bottom',
           text = element_text(size = text.size))
residual.temporal.plot

if(save.image){
  ggplot2::ggsave(filename = 'residual_temporalPlot.png',
                  plot = residual.temporal.plot,
                  height = height, width = width)
}


## spatial ----

residual.spatial.data <-
  theta.space %>% 
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:')) %>%
  dplyr::left_join(., poly.lsoa.england, by = 'LSOA11CD') %>%
  sf::st_as_sf()

residual.spatial.plot <-
  ggplot2::ggplot() +
  # England outline filled in with white - must be first
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  # estimate and CI width
  ggplot2::geom_sf(data = residual.spatial.data, aes(fill = median), colour = NA) +
  # set colour
  ggplot2::scale_fill_gradient2(name = 'Residual varaition\nin space',
                                low = 'blue3', mid = 'grey', high = 'red3',
                                midpoint = 0, n.breaks = 4) +
  my.map.theme(text = element_text(size = text.size),
               legend.position = 'bottom',
               legend.key.width = unit(2, 'cm'))
if(spatialPlotSimple){residual.spatial.plot}

if(save.image){
  ggplot2::ggsave(filename = 'residual_spatialPlot.png',
                  plot = residual.spatial.plot,
                  height = height, width = width)
}

## starta ----

residual.strata.data <-
  theta.parameter %>% 
  dplyr::filter(startsWith(level, 'strata_id')) %>%  
  # relative risk and then summarise
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                index = 1:n()) %>%
  dplyr::select(-starts_with('theta:'))

residual.strata.plot <-
  ggplot2::ggplot(data = residual.strata.data, aes(x = index, y = median)) +
  ggplot2::geom_hline(aes(yintercept = 0), color = 'red3', linetype = 'dashed') +
  ggplot2::geom_point() +
  my.theme(); residual.strata.plot

if(save.image){
  ggplot2::ggsave(filename = 'residual_strataPlot.png',
                  plot = residual.strata.plot,
                  height = height, width = width)
}

## cluster/psu ----

residual.cluster.data <-
  theta.parameter %>% 
  dplyr::filter(startsWith(level, 'psu_id')) %>%  
  # relative risk and then summarise
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                index = 1:n()) %>%
  dplyr::select(-starts_with('theta:'))

residual.cluster.plot <-
  ggplot2::ggplot(data = residual.cluster.data, aes(x = index, y = median)) +
  ggplot2::geom_hline(aes(yintercept = 0), color = 'red3', linetype = 'dashed') +
  ggplot2::geom_point() +
  my.theme(); residual.cluster.plot

if(save.image){
  ggplot2::ggsave(filename = 'residual_clusterPlot.png',
                  plot = residual.cluster.plot,
                  height = height, width = width)
}

# additional results ----

setwd(res.main.analysis.dir)

## sample characteristics ----

sample.characteristic.indivudal <-
  data.frame(characteristic = 'Individual',
             group = 'Individual',
             n = data.final$individual %>% unique() %>% length(),
             freq = 100)

sample.characteristic.employed <-
  data.final %>% 
  dplyr::select(individual, exposed) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'exposed') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                group = exposed %>% factor(., levels = c('Exposed', 'Control'), labels = c('Unemployed', 'Not unemployed')),
                characteristic = 'Employment') %>% 
  dplyr::select(characteristic, group, n, freq) %>% 
  dplyr::arrange(group)

sample.characteristic.age <-
  data.final %>% 
  dplyr::select(individual, age) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'age') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                group = age %>% factor(., levels = c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)')),
                characteristic = 'Age') %>% 
  dplyr::select(characteristic, group, n, freq) %>% 
  dplyr::arrange(group)

sample.characteristic.education <-
  data.final %>% 
  dplyr::select(individual, edu) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'edu') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                group = edu %>% factor(., levels = c('GCSE, A-level or equivalent', 'Degree or higher', 'Below GCSE and other')),
                characteristic = 'Education') %>% 
  dplyr::select(characteristic, group, n, freq) %>% 
  dplyr::arrange(group)

sample.characteristic.ethnicity <-
  data.final %>% 
  dplyr::select(individual, ethn) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'ethn') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                group = ethn %>% factor(., levels = c('Asian', 'Black', 'Mixed', 'Other', 'White')),
                characteristic = 'Ethnicity') %>% 
  dplyr::select(characteristic, group, n, freq) %>% 
  dplyr::arrange(group)


sample.characteristic.relationship <-
  data.final %>% 
  dplyr::select(individual, rela) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'rela') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                group = rela %>% factor(., levels = c('Single', 'Relationship')),
                characteristic = 'Relationship') %>% 
  dplyr::select(characteristic, group, n, freq) %>% 
  dplyr::arrange(group)

sample.characteristic.sex <-
  data.final %>% 
  dplyr::select(individual, sex) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'sex') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                group = sex %>% factor(., levels = c('Male', 'Female')),
                characteristic = 'Sex') %>% 
  dplyr::select(characteristic, group, n, freq) %>% 
  dplyr::arrange(group)

sample.characteristic.deprivation <-
  data.final %>% 
  dplyr::select(individual, DEPRIVATION) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'DEPRIVATION') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                group = DEPRIVATION %>% factor(., levels = 1:10),
                characteristic = 'Deprivation') %>% 
  dplyr::select(characteristic, group, n, freq) %>% 
  dplyr::arrange(group)

sample.characteristic.diversity <-
  data.final %>% 
  dplyr::select(individual, DIVERSITY) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'DIVERSITY') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                group = DIVERSITY %>% factor(., levels = 1:4),
                characteristic = 'Diversity') %>% 
  dplyr::select(characteristic, group, n, freq) %>% 
  dplyr::arrange(group)

sample.characteristic.all <- 
  dplyr::bind_rows(sample.characteristic.indivudal,
                   sample.characteristic.employed,
                   sample.characteristic.age,
                   sample.characteristic.education,
                   sample.characteristic.ethnicity,
                   sample.characteristic.relationship,
                   sample.characteristic.sex,
                   sample.characteristic.deprivation,
                   sample.characteristic.diversity)


print(x = xtable::xtable(sample.characteristic.all, digits = 4), 
      include.rownames = FALSE,
      file = 'sampleCharacteristic.text')

## missing ltla ----

ltlaMissing.EB <-
  theta.predictor %>% 
  dplyr::select(exposed, treatment, LAD22NM) %>%  
  dplyr::filter(exposed == 'Exposed', treatment == 0) %>% 
  dplyr::distinct() %>% 
  dplyr::pull(LAD22NM) %>% 
  sort()

ltlaMissing.EA <-
  theta.predictor %>% 
  dplyr::select(exposed, treatment, LAD22NM) %>%  
  dplyr::filter(exposed == 'Exposed', treatment == 1) %>% 
  dplyr::distinct() %>% 
  dplyr::pull(LAD22NM) %>% 
  sort()

ltlaMissing.CB <-
  theta.predictor %>% 
  dplyr::select(exposed, treatment, LAD22NM) %>%  
  dplyr::filter(exposed == 'Control', treatment == 0) %>% 
  dplyr::distinct() %>% 
  dplyr::pull(LAD22NM) %>% 
  sort()

ltlaMissing.CA <-
  theta.predictor %>% 
  dplyr::select(exposed, treatment, LAD22NM) %>%  
  dplyr::filter(exposed == 'Control', treatment == 1) %>% 
  dplyr::distinct() %>% 
  dplyr::pull(LAD22NM) %>% 
  sort()

setdiff(poly.lad.england$LAD22NM, ltlaMissing.EB) %>% length() +
  setdiff(poly.lad.england$LAD22NM, ltlaMissing.EA) %>% length() +
  setdiff(poly.lad.england$LAD22NM, ltlaMissing.CB) %>% length() +
  setdiff(poly.lad.england$LAD22NM, ltlaMissing.CA) %>% length()

setdiff(poly.lad.england$LAD22NM, ltlaMissing.EB) %>% length()
setdiff(poly.lad.england$LAD22NM, ltlaMissing.EA) %>% length()
setdiff(poly.lad.england$LAD22NM, ltlaMissing.CB) %>% length()
setdiff(poly.lad.england$LAD22NM, ltlaMissing.CA) %>% length()

## ranked ltla ----

standardised.change.spatial.lad.data.ranked <- 
  standardised.change.spatial.lad.data %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LAD22CD, LAD22NM, lower, median, upper) %>% 
  left_join(.,
            imd.01.21.average.lad,
            by = 'LAD22CD') %>% 
  dplyr::mutate(index = 1:n(),
                mostDeprived = dplyr::if_else(DEPRIVATION %in% c('1 (Most Deprived)', '2'), 1, 0),
                leastDeprived = dplyr::if_else(DEPRIVATION %in% c('9', '10 (Least Deprived)'), 1, 0),
                increaseDecrease = dplyr::if_else(median > 0, 'Increase', 'Decrease'))

total.ltla.with.score <- nrow(standardised.change.spatial.lad.data.ranked)

standardised.change.spatial.lad.data.ranked %>% 
  dplyr::filter(index %in% 1:floor(total.ltla.with.score*0.10)) %>% 
  dplyr::summarise(leastDeprived = mean(leastDeprived)*100,
                   mostDeprived = mean(mostDeprived)*100)

standardised.change.spatial.lad.data.ranked %>% 
  dplyr::filter(index %in% floor(total.ltla.with.score*0.90):total.ltla.with.score) %>% 
  dplyr::summarise(leastDeprived = mean(leastDeprived)*100,
                   mostDeprived = mean(mostDeprived)*100)

table(standardised.change.spatial.lad.data.ranked$increaseDecrease)/nrow(standardised.change.spatial.lad.data.ranked)*100
