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
res.figures.dir <- paste0(res.dir, '/Figures')

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
poly.lad <- sf::st_read(dsn = data.spatial.dir, layer = 'ONS22_LAD')
poly.nat <- sf::st_read(dsn = data.spatial.dir, layer = 'ONS21_NAT')

### organised ----

poly.lad.england <- poly.lad %>%  dplyr::filter(str_detect(LAD22CD, '^E'))
poly.nat.england <- poly.nat %>%  dplyr::filter(str_detect(CTRY21CD, '^E'))

spatialPlotSimple <- FALSE
if(spatialPlotSimple){
  # less detailed but quicker plots
  poly.lad.england <-
    poly.lad.england %>%
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)
  poly.nat.england <-
    poly.nat.england %>%
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)
}

## results ----

setwd(res.model.fit.dir)
data.final <- readRDS(file = 'modelData.rds')
fit <- readRDS(file = 'modelFit.rds')
theta.predictor <- readRDS(file = 'linearPredictorSamples.rds')
theta.observation <- readRDS(file = 'observationSamples.rds')
theta.parameter <- readRDS(file = 'parameterSamples.rds')
theta.space <- readRDS(file = 'spatialRandomEffectSamples.rds')
theta.time <- readRDS(file = 'temporalRandomEffectSamples.rds')

# plots ----

setwd(res.figures.dir)

## centered years ----

centeredPlotData.national <-
  theta.predictor %>%
  dplyr::mutate(time =
                  (lubridate::interval(start = ucStartDate, end = interviewDate) %/% months(12)) %>%
                  factor(.,
                         labels = c(-10:-1, 'Start', 1:4))) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'time')) %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  expit() %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:'))

centeredPlot.national <-
  ggplot2::ggplot(data = centeredPlotData.national, aes(x = time, y = median, group = exposed, colour = exposed, fill = exposed)) +
  ggplot2::geom_vline(xintercept = 'Start', colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, colour = NA) +
  ggplot2::scale_colour_manual(values = c('blue3', 'red3')) +
  ggplot2::scale_fill_manual(values = c('blue3', 'red3')) +
  ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  ggplot2::labs(x = 'Time since awareness to Universal Credit (years)', y = 'Psychological distress prevalence (%)') +
  my.theme(legend.title = element_blank(),
           legend.position = 'bottom',
           text = element_text(size = text.size))
centeredPlot.national

if(save.image){
  ggplot2::ggsave(filename = 'centeredPlot.national.png',
                  plot = centeredPlot.national,
                  height = height, width = width)
}

## standardised change ----

### spatial profile ----

standChangePlotData.national <- 
  theta.predictor %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment')) %>%
  dplyr::mutate(grouping = interaction(exposed, treatment, sep = '')) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::summarise(summary = 
                     (expit(value[grouping == 'Exposed1']) - 
                        expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) /
                     (expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) * 100,
                   .by = c('theta')) %>%
  tidyr::pivot_wider(names_from = 'theta', values_from = 'summary', names_prefix = 'theta:', names_sep = '') %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:'))

standChangePlotData.lad <-
  theta.predictor %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment', 'LAD22NM')) %>%
  dplyr::mutate(grouping = interaction(exposed, treatment, sep = '')) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::summarise(summary = 
                     (expit(value[grouping == 'Exposed1']) - 
                        expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) /
                     (expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) * 100,
                   .by = c('LAD22NM', 'theta')) %>%
  tidyr::pivot_wider(names_from = 'theta', values_from = 'summary', names_prefix = 'theta:', names_sep = '') %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:')) %>%
  dplyr::left_join(., poly.lad.england, by = 'LAD22NM') %>%
  dplyr::arrange(median) %>%
  dplyr::mutate(space = LAD22NM %>% haven::as_factor(),
                space_id = space %>% as.numeric()) %>%
  sf::st_as_sf()

largestStandChange.lad <-
  standChangePlotData.lad %>% 
  dplyr::filter(median == max(median)) %>% 
  dplyr::mutate(label = paste0('Increase:\n', LAD22NM)) 

smallestStandChange.lad <- 
  standChangePlotData.lad %>% 
  dplyr::filter(median == min(median)) %>% 
  dplyr::mutate(label = paste0('Decrease:\n', LAD22NM))

standChangePlot.lad_prev <- 
  ggplot2::ggplot() +
  # England outline filled in with white - must be first
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  # estimate and CI width
  ggplot2::geom_sf(data = standChangePlotData.lad, aes(fill = median), colour = 'black') +
  # largest and smallest StandChange
  ## largest 
  ggrepel::geom_text_repel(data = largestStandChange.lad,
                           aes(label = label, geometry = geometry),
                           stat = 'sf_coordinates',
                           size = 8,
                           nudge_x = 210000 - largestStandChange.lad$BNG_E,
                           nudge_y = 450000 - largestStandChange.lad$BNG_N,
                           min.segment.length = 0) +
  ## smallest 
  ggrepel::geom_text_repel(data = smallestStandChange.lad,
                           aes(label = label, geometry = geometry),
                           stat = 'sf_coordinates', 
                           size = 8,
                           nudge_x = 210000 - smallestStandChange.lad$BNG_E,
                           nudge_y = 210000 - smallestStandChange.lad$BNG_N,
                           min.segment.length = 0) +
  # set colour
  ggplot2::scale_fill_gradient2(name = 'Standardised \nchange (%)',
                                low = 'blue3', mid = 'grey', high = 'red3',
                                midpoint = 0, n.breaks = 4) +
  my.map.theme(text = element_text(size = text.size),
               legend.position = 'bottom')
if(spatialPlotSimple){standChangePlot.lad_prev}

standChangePlot.lad_uncer <-
  ggplot2::ggplot(data = standChangePlotData.lad, 
                  aes(x = space_id, y = median, color = space_id)) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), width = .05) +
  ggplot2::scale_colour_gradient2(low = 'blue3',
                                  mid = 'grey',
                                  high = 'red3',
                                  midpoint = which.max(standChangePlotData.lad$median > 0)) + 
  ggplot2::geom_hline(yintercept = 0, color = 'black', linetype = 'dashed') +
  ggplot2::labs(x = 'Lower Tier Local Authority', y = 'Standardised change (%)') +
  # largest and smallest StandChange
  ## largest 
  ggrepel::geom_label_repel(data = largestStandChange.lad,
                            aes(label = label), colour = 'black',
                            nudge_x = -30, nudge_y = 10, size = 8) +
  ## smallest 
  ggrepel::geom_label_repel(data = smallestStandChange.lad,
                            aes(label = label), colour = 'black',
                            nudge_x = 15, nudge_y = -20, size = 8) +
  # national results
  ggplot2::geom_hline(yintercept = standChangePlotData.national$median, color = 'black', linetype = 'solid') +
  ggplot2::annotate(geom = 'text',
                    label = paste0('National: ', round(standChangePlotData.national$median, 2), 
                                   '% \n(', round(standChangePlotData.national$lower, 2), '% - ', round(standChangePlotData.national$upper, 2), '%)'),
                    x = nrow(standChangePlotData.lad)/4, 
                    y = 30,
                    color = 'black',
                    size = 8) +
  my.theme(axis.ticks.x = element_blank(),
           axis.text.x = element_blank(),
           legend.position = 'none',
           text = element_text(size = text.size))
standChangePlot.lad_uncer

if(save.image){
  ggplot2::ggsave(filename = 'standChangePlot.lad_prev.png',
                  plot = standChangePlot.lad_prev,
                  height = height, width = width) 
  ggplot2::ggsave(filename = 'standChangePlot.lad_uncer.png',
                  plot = standChangePlot.lad_uncer,
                  height = height, width = width) 
}


### confounder profiles individually ----

standChangePlotData.allConfounders <- 
  theta.predictor %>%
  tidyr::pivot_longer(cols = c('age', 'edu', 'ethn', 'marStat', 'sex', 'deprivation', 'diversity'), names_to = 'confounder', values_to = 'category') %>% 
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment', 'confounder', 'category')) %>% 
  dplyr::mutate(grouping = interaction(exposed, treatment, sep = '')) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::summarise(summary = 
                     (expit(value[grouping == 'Exposed1']) - 
                        expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) /
                     (expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) * 100,
                   .by = c('confounder', 'category', 'theta')) %>%
  tidyr::pivot_wider(names_from = 'theta', values_from = 'summary', names_prefix = 'theta:', names_sep = '') %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                confounder = confounder %>% factor(., 
                                                   levels = c('age', 'edu', 'ethn', 'marStat', 'sex', 'deprivation', 'diversity'),
                                                   labels = c('Age', 'Education', 'Ethnicity', 
                                                              'Marital Status', 'Sex', 
                                                              'Deprivation', 'Diversity')), 
                category = category %>% factor(.,
                                               levels = c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)', 
                                                          'Degree or higher', 'GCSE, A-level or equivalent', 'Below GCSE and other',
                                                          'Asian', 'Black', 'Mixed', 'Other', 'White', 
                                                          'Married or civil partnership', 'Unmarried',
                                                          'Male', 'Female',
                                                          '1', '2', '3', '4', '5', '6', '7', '8', '9', '10'),
                                               labels = c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)', 
                                                          '[Degree or higher]', '[GCSE, A-level or equivalent]', '[Below GCSE and other]',
                                                          '[Asian]', '[Black]', '[Mixed]', '[Other]', '[White]', 
                                                          '[Married or civil partnership]', '[Unmarried]',
                                                          '[Male]', '[Female]',
                                                          '[1]', '[2]', '[3]', '[4]', '[5]', '[6]', '[7]', '[8]', '[9]', '[10]'))) %>%
  dplyr::select(-starts_with('theta:')) %>% 
  dplyr::arrange(confounder, category)

standChangePlot.allConfounders <-
  ggplot2::ggplot(data = standChangePlotData.allConfounders, aes(x = category, y = median, group = confounder)) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), width = .05) +
  ggplot2::geom_hline(yintercept = 0, color = 'black', linetype = 'dashed') +
  ggplot2::facet_wrap(~confounder, scales = 'free_x', ncol = 2) +
  ggplot2::labs(x = '', y = 'Standardised change (%)') + 
  my.theme(legend.position = 'none',
           text = element_text(size = text.size)); standChangePlot.allConfounders


if(save.image){
  ggplot2::ggsave(filename = 'standChangePlot.allConfounders.png',
                  plot = standChangePlot.allConfounders,
                  height = height, width = 2*width)  
}

### individual x community confounder profiles ----

standChangePlotData.jointConfounders <- 
  theta.predictor %>%
  tidyr::pivot_longer(cols = c('age', 'edu', 'ethn', 'marStat', 'sex'), names_to = 'indConfounder', values_to = 'indCategory') %>% 
  tidyr::pivot_longer(cols = c('deprivation', 'diversity'), names_to = 'comConfounder', values_to = 'comCategory') %>% 
  dplyr::mutate(confounder = interaction(indConfounder, comConfounder),
                category = interaction(indCategory, comCategory)) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment', 'confounder', 'category')) %>% 
  dplyr::mutate(grouping = interaction(exposed, treatment, sep = '')) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::summarise(summary = 
                     (expit(value[grouping == 'Exposed1']) - 
                        expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) /
                     (expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) * 100,
                   .by = c('confounder', 'category', 'theta')) %>%
  tidyr::pivot_wider(names_from = 'theta', values_from = 'summary', names_prefix = 'theta:', names_sep = '') %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                label = paste0(round(median, 2), '%\n(', round(lower, 2), '%-',  round(upper, 2), '%)')) %>%
  dplyr::select(-starts_with('theta:')) %>% 
  tidyr::separate_wider_delim(confounder, delim = '.', names = c('indConfounder', 'comConfounder')) %>% 
  tidyr::separate_wider_delim(category, delim = '.', names = c('indCategory', 'comCategory')) %>% 
  dplyr::mutate(indConfounder = indConfounder %>% factor(., 
                                                         levels = c('age', 'edu', 'ethn', 'marStat', 'sex'),
                                                         labels = c('Age', 'Education', 'Ethnicity', 
                                                                    'Marital Status', 'Sex')),
                indCategory = indCategory %>% factor(., 
                                                     levels = c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)', 
                                                                'Degree or higher', 'GCSE, A-level or equivalent', 'Below GCSE and other',
                                                                'Asian', 'Black', 'Mixed', 'Other', 'White', 
                                                                'Married or civil partnership', 'Unmarried',
                                                                'Male', 'Female'),
                                                     labels = c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)', 
                                                                '[Degree or higher]', '[GCSE, A-level or equivalent]', '[Below GCSE and other]',
                                                                '[Asian]', '[Black]', '[Mixed]', '[Other]', '[White]', 
                                                                '[Married or civil partnership]', '[Unmarried]',
                                                                '[Male]', '[Female]')),
                comConfounder = comConfounder %>% factor(., 
                                                         levels = c('deprivation', 'diversity'),
                                                         labels = c('Deprivation', 'Diversity')),
                comCategory = comCategory %>% factor(., levels = 1:10,
                                                     labels = c('[1]', '[2]', '[3]', '[4]', '[5]', '[6]', '[7]', '[8]', '[9]', '[10]'))) %>% 
  dplyr::arrange(comConfounder, comCategory, indConfounder, indCategory)

missingComb <-
  expand.grid(indConfounder  = 'Ethnicity',
              comConfounder = 'Deprivation',
              indCategory = c('[Asian]', '[Black]', '[Mixed]', '[Other]', '[White]'), 
              comCategory = 1:10) %>% 
  dplyr::mutate(indConfounder = indConfounder %>% factor(., 
                                                         levels = c('Age', 'Education', 'Ethnicity', 
                                                                    'Marital Status', 'Sex')),
                indCategory = indCategory %>% factor(., 
                                                     levels = c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)', 
                                                                '[Degree or higher]', '[GCSE, A-level or equivalent]', '[Below GCSE and other]',
                                                                '[Asian]', '[Black]', '[Mixed]', '[Other]', '[White]', 
                                                                '[Married or civil partnership]', '[Unmarried]',
                                                                '[Male]', '[Female]')),
                comConfounder = comConfounder %>% factor(., 
                                                         levels = c('Deprivation', 'Diversity')),
                comCategory = comCategory %>% factor(., 
                                                     levels = 1:10,
                                                     labels = c('[1]', '[2]', '[3]', '[4]', '[5]', '[6]', '[7]', '[8]', '[9]', '[10]')))

standChangePlot.jointConfounders <-
  ggplot2::ggplot(data = 
                    standChangePlotData.jointConfounders %>% 
                    dplyr::full_join(., missingComb, by = c('indConfounder', 'comConfounder', 'indCategory', 'comCategory')), 
                  aes(x = comCategory, y = indCategory, group = indConfounder)) +
  ggplot2::geom_tile(aes(fill = median), colour = 'black') +
  ggplot2::scale_fill_gradient2(name = 'Standardised \nchange (%)',
                                low = 'blue3', mid = 'grey', high = 'red3',
                                midpoint = 0, n.breaks = 4, na.value = 'white') +
  ggplot2::facet_grid(indConfounder ~ comConfounder, scales = 'free') +
  ggplot2::scale_y_discrete(limits = rev) + 
  ggplot2::labs(x = '', y = '') + 
  my.theme(text = element_text(size = text.size),
           legend.position = 'bottom',
           legend.title.align = 0.5); standChangePlot.jointConfounders

if(save.image){
  ggplot2::ggsave(filename = 'standChangePlot.jointConfounders.png',
                  plot = standChangePlot.jointConfounders,
                  height = height, width = width)
}

# tables  ----

## temporal profiles ---- 

### exposed & control ----

allYear.exposure.temporalProfile <- 
  theta.predictor %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed')) %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  expit() %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                time = 'All') %>%
  dplyr::select(-starts_with('theta:'), -mean , -variance) %>% 
  pivot_wider(names_from = 'exposed', values_from = c('lower', 'median', 'upper')) %>% 
  dplyr::relocate(c('lower_Exposed', 'median_Exposed', 'upper_Exposed',
                    'lower_Control', 'median_Control', 'upper_Control'), .after = 'time')


beforeAfter.exposure.temporalProfile <-
  theta.predictor %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment')) %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  expit() %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                time = dplyr::if_else(treatment == 0, 'Before', 'After')) %>%
  dplyr::select(-starts_with('theta:'), -mean, -variance, -treatment) %>% 
  pivot_wider(names_from = 'exposed', values_from = c('lower', 'median', 'upper')) %>% 
  dplyr::relocate(c('lower_Exposed', 'median_Exposed', 'upper_Exposed',
                    'lower_Control', 'median_Control', 'upper_Control'), .after = 'time')

seperateTimes.exposure.temporalProfile <-
  theta.predictor %>%
  dplyr::mutate(time = (lubridate::year(interviewDate) - lubridate::year(ucStartDate))) %>%
  dplyr::filter(time != 5) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'time')) %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  expit() %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:'), -mean, -variance) %>% 
  pivot_wider(names_from = 'exposed', values_from = c('lower', 'median', 'upper')) %>% 
  dplyr::relocate(c('lower_Exposed', 'median_Exposed', 'upper_Exposed',
                    'lower_Control', 'median_Control', 'upper_Control'), .after = 'time')

### exposed versus control ----

allYear.versus.temporalProfile <- 
  theta.predictor %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed')) %>%
  dplyr::mutate(grouping = exposed) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>% 
  dplyr::summarise(value1 = (expit(value[grouping == 'Exposed']) / expit(value[grouping == 'Control'])),
                   .by = c('theta')) %>%
  tidyr::pivot_wider(names_from = 'theta', values_from = 'value1', names_prefix = 'theta:', names_sep = '') %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                time = 'All') %>%
  dplyr::select(-starts_with('theta:'), -mean, -variance) %>% 
  dplyr::rename(diff_lower = lower, diff_median = median, diff_upper = upper) %>% 
  dplyr::relocate('time', .before = 'diff_lower')

beforeAfter.versus.temporalProfile <-
  theta.predictor %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment')) %>%
  dplyr::mutate(grouping = exposed) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>% 
  dplyr::summarise(value1 = (expit(value[grouping == 'Exposed']) / expit(value[grouping == 'Control'])),
                   .by = c('theta', 'treatment')) %>%
  tidyr::pivot_wider(names_from = 'theta', values_from = 'value1', names_prefix = 'theta:', names_sep = '') %>% 
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                time = dplyr::if_else(treatment == 0, 'Before', 'After')) %>%
  dplyr::select(-starts_with('theta:'), -mean, -variance, -treatment) %>% 
  dplyr::rename(diff_lower = lower, diff_median = median, diff_upper = upper)  %>% 
  dplyr::relocate('time', .before = 'diff_lower')

seperateTimes.versus.temporalProfile <- 
  theta.predictor %>%
  dplyr::mutate(time = (lubridate::year(interviewDate) - lubridate::year(ucStartDate))) %>%
  dplyr::filter(time != 5) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'time')) %>%
  dplyr::mutate(grouping = exposed) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>% 
  dplyr::summarise(value1 = (expit(value[grouping == 'Exposed']) / expit(value[grouping == 'Control'])),
                   .by = c('theta', 'time')) %>%
  tidyr::pivot_wider(names_from = 'theta', values_from = 'value1', names_prefix = 'theta:', names_sep = '') %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::rename(diff_lower = lower, diff_median = median, diff_upper = upper) %>% 
  dplyr::select(-starts_with('theta:'), -mean, -variance)


### combine ---- 

allYear.temporalProfile <-
  dplyr::left_join(allYear.exposure.temporalProfile, allYear.versus.temporalProfile, by = 'time')

beforeAfter.temporalProfile <-
  dplyr::left_join(beforeAfter.exposure.temporalProfile, beforeAfter.versus.temporalProfile, by = 'time')

seperateTimes.temporalProfile <-
  dplyr::left_join(seperateTimes.exposure.temporalProfile, seperateTimes.versus.temporalProfile, by = 'time')

temporalProfile <-
  rbind(seperateTimes.temporalProfile,
        beforeAfter.temporalProfile,
        allYear.temporalProfile) %>% 
  dplyr::mutate(across(-c('time', starts_with('diff_')), ~ round(.x*100, digits = 2)))

print(x = xtable::xtable(temporalProfile), 
      include.rownames = FALSE,
      file = 'temporalProfile.tex')

## confounder profiles ----

confounderProfile.all <-
  theta.predictor %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment', 'age', 'edu', 'ethn', 'marStat', 'sex')) %>%
  dplyr::mutate(grouping = interaction(exposed, treatment, sep = '')) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::summarise(summary = 
                     (expit(value[grouping == 'Exposed1']) - 
                        expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) /
                     (expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) * 100,
                   .by = c('theta', 'age', 'edu', 'ethn', 'marStat', 'sex')) %>%
  tidyr::pivot_wider(names_from = 'theta', values_from = 'summary', names_prefix = 'theta:', names_sep = '') %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:')) %>% 
  dplyr::arrange(dplyr::desc(median)) %>% 
  select(-mean, - variance)


confounderProfile.topBottom5 <-
  rbind(head(confounderProfile.all, n = 5),
        tail(confounderProfile.all, n = 5)) %>% 
  as.data.frame() %>% 
  dplyr::mutate(age = age %>% factor(., 
                                     levels = c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)'),
                                     labels = c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)')),
                edu = edu %>% factor(., 
                                     levels = c('Degree or higher', 'GCSE, A-level or equivalent', 'Below GCSE and other'),
                                     labels = c('[Degree or higher]', '[GCSE, A-level or equivalent]', '[Below GCSE and other]')),
                ethn = ethn %>% factor(., 
                                       levels = c('Asian', 'Black', 'Mixed', 'Other', 'White'),
                                       labels = c('[Asian]', '[Black]', '[Mixed]', '[Other]', '[White]')),
                marStat = marStat %>% factor(., 
                                             levels = c('Married or civil partnership', 'Unmarried'),
                                             labels = c('[Married or civil partnership]', '[Unmarried]')),
                sex = sex %>% factor(., 
                                     levels = c('Male', 'Female'),
                                     labels = c('[Male]', '[Female]')))

print(x = xtable::xtable(confounderProfile.topBottom5), 
      include.rownames = FALSE,
      file = 'confounderProfileTopBottom5.tex')


# additional results ----

## sample characteristics ----

percentInd <-
  data.frame(id = 'id',
             Freq = data.final$individual %>% unique() %>% length()); percentInd

percentEmployed <-
  data.final %>% 
  dplyr::select(individual, exposed) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'exposed') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                exposed = exposed %>% factor(., levels = c('Exposed', 'Control'), labels = c('Unemployed', 'Not unemployed'))) %>% 
  dplyr::arrange(exposed); percentEmployed

percentAge <-
  data.final %>% 
  dplyr::select(individual, age) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'age') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                age = age %>% factor(., levels = c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)'))) %>% 
  dplyr::arrange(age); percentAge

percentEdu <-
  data.final %>% 
  dplyr::select(individual, edu) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'edu') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                edu = edu %>% factor(., levels = c('Degree or higher', 'GCSE, A-level or equivalent', 'Below GCSE and other'))) %>% 
  dplyr::arrange(edu); percentEdu

percentEthn <-
  data.final %>% 
  dplyr::select(individual, ethn) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'ethn') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                ethn = ethn %>% factor(., levels = c('Asian', 'Black', 'Mixed', 'Other', 'White'))) %>% 
  dplyr::arrange(ethn); percentEthn


percentMarStat <-
  data.final %>% 
  dplyr::select(individual, marStat) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'marStat') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                marStat = marStat %>% factor(., levels = c('Married or civil partnership', 'Unmarried'))) %>% 
  dplyr::arrange(marStat); percentMarStat


percentSex <-
  data.final %>% 
  dplyr::select(individual, sex) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'sex') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                sex = sex %>% factor(., levels = c('Male', 'Female'))) %>% 
  dplyr::arrange(sex); percentSex

percentDep <-
  data.final %>% 
  dplyr::select(individual, DEPRIVATION) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'DEPRIVATION') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                deprivation = DEPRIVATION %>% factor(., levels = 1:10)) %>% 
  dplyr::arrange(deprivation); percentDep

percentDiv <-
  data.final %>% 
  dplyr::select(individual, DIVERSITY) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'DIVERSITY') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                diversity = DIVERSITY %>% factor(., levels = 1:6)) %>% 
  dplyr::arrange(diversity); percentDiv

## model parameters ----

### table ----

fitSummary <- 
  data.frame(
    group = 
      c(rep('Control ITS terms', times = 4),
        rep('Exposed ITS terms', times = 4),
        rep('Relative to [16, 25)', times = 4),
        rep('Relative to [Degree or higher]', times = 2),
        rep('Relative to [White]', times = 4),
        rep('Relative to [Married or civil partnership]', times = 1),
        rep('Relative to [Male]', times = 1),
        rep('Deprivation relative to [1]', times = 9),
        rep('Diversity relative to [1]', times = 3),
        rep('Random effect', times = 3)),
    parameter = 
      c('Intercept', 'Time', 'Intervention', 'Time+',
        'Intercept', 'Time', 'Intervention', 'Time+',
        '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)',
        '[Below GCSE and other]', '[GCSE, A-level or equivalent]',
        '[Asian]', '[Black]', '[Mixed]', '[Other]',
        '[Unmarried]',
        '[Female]',
        '[2]', '[3]', '[4]', '[5]', '[6]', '[7]', '[8]', '[9]', '[10]',
        '[2]', '[3]', '[4]',
        'Temporal precision', 'Spatial precision', 'Spatial mixing'),
    lower = c(fit$summary.fixed[,3], fit$summary.hyperpar[,3]),
    median = c(fit$summary.fixed[,4], fit$summary.hyperpar[,4]),
    upper = c(fit$summary.fixed[,5], fit$summary.hyperpar[,5]))

print(x = xtable::xtable(fitSummary,
                         digits = 4), 
      include.rownames = FALSE,
      file = 'fitSummary.tex')

### plot ----

fitSummaryPlotData <- 
  data.frame(parameter = 
               c('Intercept', 'Time', 'Intervention', 'Time+',
                 'Intercept', 'Time', 'Intervention', 'Time+',
                 '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)',
                 '[Below GCSE and other]', '[GCSE, A-level or equivalent]',
                 '[Asian]', '[Black]', '[Mixed]', '[Other]',
                 '[Unmarried]',
                 '[Female]',
                 '[2]', '[3]', '[4]', '[5]', '[6]', '[7]', '[8]', '[9]', '[10]',
                 '[2]', '[3]', '[4]') %>% 
               factor(., levels = c('Intercept', 'Time', 'Intervention', 'Time+',
                                    '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)',
                                    '[Below GCSE and other]', '[GCSE, A-level or equivalent]',
                                    '[Asian]', '[Black]', '[Mixed]', '[Other]',
                                    '[Unmarried]',
                                    '[Female]',
                                    '[2]', '[3]', '[4]', '[5]', '[6]', '[7]', '[8]', '[9]', '[10]')),
             group = c(rep('Control ITS terms', times = 4),
                       rep('Exposed ITS terms', times = 4),
                       rep('Relative to [16, 25)', times = 4),
                       rep('Relative to [Degree or higher]', times = 2),
                       rep('Relative to [White]', times = 4),
                       rep('Relative to [Married or civil partnership]', times = 1),
                       rep('Relative to [Male]', times = 1),
                       rep('Deprivation relative to [1]', times = 9),
                       rep('Diversity relative to [1]', times = 3)) %>%
               factor(., 
                      levels = c('Control ITS terms',
                                 'Exposed ITS terms',
                                 'Relative to [16, 25)',
                                 'Relative to [Degree or higher]',
                                 'Relative to [White]',
                                 'Relative to [Married or civil partnership]',
                                 'Relative to [Male]',
                                 'Deprivation relative to [1]',
                                 'Diversity relative to [1]')),
             lower = fit$summary.fixed[,3],
             median = fit$summary.fixed[,4],
             upper = fit$summary.fixed[,5])

fitSummaryPlot <-
  ggplot2::ggplot(fitSummaryPlotData,
                  aes(x = parameter, y = median, group = parameter)) +
  ggplot2::geom_hline(yintercept = 0, colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), width = .05) +
  ggplot2::labs(x = '', y = '') +
  ggplot2::facet_wrap(~ group, scales = 'free') +
  my.theme(legend.title = element_blank(),
           text = element_text(size = text.size),
           legend.position = 'bottom')
fitSummaryPlot

if(save.image){
  ggplot2::ggsave(filename = 'fitSummaryPlot.png',
                  plot = fitSummaryPlot,
                  height = height, width = 2.5*width) 
}

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

## odds ratio ----

### all fixed parameters ----

fixed.parametere.odds.ratio.data <-
  theta.parameter %>% 
  dplyr::filter(!startsWith(level, 'time.month_id'),
                !startsWith(level, 'space_id')) %>%  
  # relative risk and then summarise
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  exp() %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:')) %>% 
  cbind(., 
        covariateLevel = 
          c('Intercept', 'Time', 'Intervention', 'Time+',
            'Intercept', 'Time', 'Intervention', 'Time+',
            '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)',
            '[Below GCSE and other]', '[GCSE, A-level or equivalent]',
            '[Asian]', '[Black]', '[Mixed]', '[Other]',
            '[Unmarried]',
            '[Female]',
            '[2]', '[3]', '[4]', '[5]', '[6]', '[7]', '[8]', '[9]', '[10]',
            '[2]', '[3]', '[4]') %>% 
          factor(., levels = c('Intercept', 'Time', 'Intervention', 'Time+',
                               '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)',
                               '[Below GCSE and other]', '[GCSE, A-level or equivalent]',
                               '[Asian]', '[Black]', '[Mixed]', '[Other]',
                               '[Unmarried]',
                               '[Female]',
                               '[2]', '[3]', '[4]', '[5]', '[6]', '[7]', '[8]', '[9]', '[10]')),
        covariate = c(rep('Control ITS terms', times = 4),
                      rep('Exposed ITS terms', times = 4),
                      rep('Relative to [16, 25)', times = 4),
                      rep('Relative to [Degree or higher]', times = 2),
                      rep('Relative to [White]', times = 4),
                      rep('Relative to [Married or civil partnership]', times = 1),
                      rep('Relative to [Male]', times = 1),
                      rep('Deprivation relative to [1]', times = 9),
                      rep('Diversity relative to [1]', times = 3)) %>%
          factor(., 
                 levels = c('Control ITS terms',
                            'Exposed ITS terms',
                            'Relative to [16, 25)',
                            'Relative to [Degree or higher]',
                            'Relative to [White]',
                            'Relative to [Married or civil partnership]',
                            'Relative to [Male]',
                            'Deprivation relative to [1]',
                            'Diversity relative to [1]')))

fixed.parametere.odds.ratio.plot <-
  ggplot2::ggplot(fixed.parametere.odds.ratio.data, aes(x = covariateLevel, y = median)) +
  # ggplot2::geom_hline(yintercept = 1, colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), width = .05) +
  ggplot2::labs(x = '', y = '') +
  ggplot2::facet_wrap(~ covariate, scales = 'free') +
  my.theme(legend.title = element_blank(),
           text = element_text(size = text.size),
           legend.position = 'bottom')
fixed.parametere.odds.ratio.plot

if(save.image){
  ggplot2::ggsave(filename = 'fitParameters_OddsRatio.png',
                  plot = fixed.parametere.odds.ratio.plot,
                  height = height, width = 2.5*width) 
}

### causal terms ----

casual.parameters.odds.ratio <-
  theta.parameter %>% 
  dplyr::filter(grepl(pattern = 'exposed', x = level)) %>% 
  dplyr::mutate(parameter = 'Exposed',
                parameterLabel = 
                  level %>% 
                  factor(., 
                         levels = paste0(c('exposed_id2', 'time.exposed_id', 'treatment.exposed_id', 'timeSinceTreatment.exposed_id'), ':1'),
                         labels = c('Baseline', 'Time', 'Intercention', 'Time+'))) %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  exp() %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(parameterLabel, lower, median, upper)

print(x = xtable::xtable(casual.parameters.odds.ratio, digits = 4), 
      include.rownames = FALSE,
      file = 'causalParametersOddsRatio.tex')

