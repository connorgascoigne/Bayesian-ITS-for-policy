# packages ----

library(tidyverse)
library(sf)
library(INLA)

# directories ----

# extract file location of this script
codePath <- rstudioapi::getActiveDocumentContext()$path
codePathSplitted <- strsplit(codePath, "/")[[1]]

# retrieve directories
homeDir <- paste(codePathSplitted[1: (length(codePathSplitted)-3)], collapse = "/")
codeDir <- paste0(homeDir, '/Code')
dataDir <- paste0(homeDir, '/Data')
resDir <- paste0(homeDir, '/Results')
resDir_modelFit <- paste0(resDir, '/Model Fit')
resDir_figures <- paste0(resDir, '/Figures')

# results folder
if(!dir.exists(paths = resDir)) {
  dir.create(path = resDir)
}

# model fit folder
if(!dir.exists(paths = resDir_figures)) {
  dir.create(path = resDir_figures)
}

# saving ----

height <- width <- 10
textSize <- 20
saveImage <- TRUE

# import ----

## functions ----

source(paste0(codeDir, '/functions.R'))

## spatial ----

### raw ----

onsShapePath <- 'Data/shapeFiles_ons2/'
ltlaPoly <- sf::st_read(paste0(onsShapePath, 'ons21_GBR_LTLA_shp/ons21_GBR_LTLA.shp'))
countryPoly <- sf::st_read(paste0(onsShapePath, 'ons21_GBR_country_shp/ons21_GBR_country.shp'))

### organised ----

ltlaPoly_England <-
  ltlaPoly %>%
  dplyr::filter(str_detect(LAD21CD, "^E"))
countryPoly_England <-
  countryPoly %>%
  dplyr::filter(str_detect(CTRY21CD, "^E"))

spatialPlotSimple <- FALSE
if(spatialPlotSimple){
  # less detailed but quicker plots
  ltlaPoly_England_simple <-
    ltlaPoly_England %>%
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)
  countryPoly_England_simple <-
    countryPoly_England %>%
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)
} else{
  # full detail for FINAL plots
  ltlaPoly_England_simple <-
    ltlaPoly_England
  countryPoly_England_simple <-
    countryPoly_England
}

## results ----

dataFinal <- readRDS(file = paste0(resDir_modelFit, '/modelData.rds'))
fit <- readRDS(file = paste0(resDir_modelFit, '/modelFit.rds'))
theta.predictor <- readRDS(file = paste0(resDir_modelFit, '/linearPredictorSamples.rds'))
theta.observation <- readRDS(file = paste0(resDir_modelFit, '/observationSamples.rds'))
theta.space <- readRDS(file = paste0(resDir_modelFit, '/spatialRandomEffectSamples.rds'))
theta.time <- readRDS(file = paste0(resDir_modelFit, '/temporalRandomEffectSamples.rds'))

# plots ----

## centered years ----

centeredPlotData.national <-
  theta.predictor %>% 
  dplyr::mutate(time = time %>% factor(., labels = c(-10:-1, 'Start', 1:5))) %>%
  dplyr::filter(time != 5) %>%
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
  ggplot2::scale_colour_manual(values = c('red3', 'blue3')) +
  ggplot2::scale_fill_manual(values = c('red3', 'blue3')) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::labs(x = 'Time since awareness to Universal Credit (years)', y = 'Psychological distress prevalence (%)') +
  my.theme(legend.title = element_blank(),
           legend.position = 'bottom',
           text = element_text(size = textSize))
centeredPlot.national

if(saveImage){
  ggplot2::ggsave(filename = paste0(resDir_figures, '/centeredPlot.national.png'),
                  plot = centeredPlot.national,
                  height = height, width = width)
}

## standardised change ----

### spatial profile ----

standChangePlotData.national <- 
  theta.predictor %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment', 'country')) %>%
  dplyr::mutate(grouping = interaction(exposed, treatment, sep = '')) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::summarise(summary = 
                     (expit(value[grouping == 'Exposed1']) - 
                        expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) /
                     (expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) * 100,
                   .by = c('theta', 'country')) %>%
  tidyr::pivot_wider(names_from = 'theta', values_from = 'summary', names_prefix = 'theta:', names_sep = '') %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:'))

standChangePlotData.localAuthority <-
  theta.predictor %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'treatment', 'localAuthority')) %>%
  dplyr::mutate(grouping = interaction(exposed, treatment, sep = '')) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>%
  dplyr::summarise(summary = 
                     (expit(value[grouping == 'Exposed1']) - 
                        expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) /
                     (expit(value[grouping == 'Exposed0'] * (value[grouping == 'Control1']/value[grouping == 'Control0']))) * 100,
                   .by = c('localAuthority', 'theta')) %>%
  tidyr::pivot_wider(names_from = 'theta', values_from = 'summary', names_prefix = 'theta:', names_sep = '') %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:')) %>%
  dplyr::left_join(., ltlaPoly_England_simple, by = c('localAuthority' = 'LAD21NM')) %>%
  dplyr::arrange(median) %>%
  dplyr::mutate(localAuthority = localAuthority %>% haven::as_factor(),
                localAuthority_id = localAuthority %>% as.numeric()) %>%
  sf::st_as_sf()

largestStandChange.localAuthority <-
  standChangePlotData.localAuthority %>% 
  dplyr::filter(median == max(median)) %>% 
  dplyr::mutate(label = paste0('Increase:\n', localAuthority)) 

smallestStandChange.localAuthority <- 
  standChangePlotData.localAuthority %>% 
  dplyr::filter(median == min(median)) %>% 
  dplyr::mutate(label = paste0('Decrease:\n', localAuthority))

standChangePlot.localAuthority_prev <- 
  ggplot2::ggplot() +
  # England outline filled in with white - must be first
  ggplot2::geom_sf(data = countryPoly_England_simple, fill = 'white') +
  # estimate and CI width
  ggplot2::geom_sf(data = standChangePlotData.localAuthority, aes(fill = median), colour = 'black') +
  # largest and smallest StandChange
  ## largest 
  ggrepel::geom_text_repel(data = largestStandChange.localAuthority,
                           aes(label = label, geometry = geometry),
                           stat = 'sf_coordinates',
                           size = 8,
                           nudge_x = 210000 - largestStandChange.localAuthority$BNG_E,
                           nudge_y = 450000 - largestStandChange.localAuthority$BNG_N,
                           min.segment.length = 0) +
  ## smallest 
  ggrepel::geom_text_repel(data = smallestStandChange.localAuthority,
                           aes(label = label, geometry = geometry),
                           stat = 'sf_coordinates', 
                           size = 8,
                           nudge_x = 210000 - smallestStandChange.localAuthority$BNG_E,
                           nudge_y = 210000 - smallestStandChange.localAuthority$BNG_N,
                           min.segment.length = 0) +
  # set colour
  ggplot2::scale_fill_gradient2(name = 'Standardised \nchange (%)',
                                low = 'blue3', mid = 'grey', high = 'red3',
                                midpoint = 0, n.breaks = 4) +
  my.map.theme(text = element_text(size = textSize),
               legend.position = 'bottom')
if(spatialPlotSimple){standChangePlot.localAuthority_prev}


standChangePlot.localAuthority_uncer <-
  ggplot2::ggplot(data = standChangePlotData.localAuthority, 
                  aes(x = localAuthority, y = median, color = localAuthority_id)) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), width = .05) +
  ggplot2::scale_colour_gradient2(low = 'blue3',
                                  mid = 'grey',
                                  high = 'red3',
                                  midpoint = which.max(standChangePlotData.localAuthority$median > 0)) + 
  ggplot2::geom_hline(yintercept = 0, color = 'black', linetype = 'dashed') +
  ggplot2::labs(x = 'Lower Tier Local Authority', y = 'Standardised change (%)') +
  # largest and smallest StandChange
  ## largest 
  ggrepel::geom_label_repel(data = largestStandChange.localAuthority,
                            aes(label = label), colour = 'black',
                            nudge_x = -30, nudge_y = 10, size = 8) +
  ## smallest 
  ggrepel::geom_label_repel(data = smallestStandChange.localAuthority,
                            aes(label = label), colour = 'black',
                            nudge_x = 15, nudge_y = -20, size = 8) +
  # national results
  ggplot2::geom_hline(yintercept = standChangePlotData.national$median, color = 'black', linetype = 'solid') +
  ggplot2::annotate(geom = 'text',
                    label = paste0('National: ', round(standChangePlotData.national$median, 2), 
                                   '% \n(', round(standChangePlotData.national$lower, 2), '% - ', round(standChangePlotData.national$upper, 2), '%)'),
                    x = nrow(standChangePlotData.localAuthority)/4, 
                    y = 30,
                    color = 'black',
                    size = 8) +
  my.theme(axis.ticks.x = element_blank(),
           axis.text.x = element_blank(),
           legend.position = 'none',
           text = element_text(size = textSize))
standChangePlot.localAuthority_uncer


if(saveImage){
  ggplot2::ggsave(filename = paste0(resDir_figures, '/standChangePlot.localAuthority_prev.png'),
                  plot = standChangePlot.localAuthority_prev,
                  height = height, width = width) 
  ggplot2::ggsave(filename = paste0(resDir_figures, '/standChangePlot.localAuthority_uncer.png'),
                  plot = standChangePlot.localAuthority_uncer,
                  height = height, width = width) 
}


### confounder profiles individually ----

standChangePlotData.allConfounders <- 
  theta.predictor %>%
  tidyr::pivot_longer(cols = c('age', 'edu', 'ethn', 'marStat', 'sex', 'imd10', 'bame5'), names_to = 'confounder', values_to = 'category') %>% 
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
                                                   levels = c('age', 'edu', 'ethn', 'marStat', 'sex', 'imd10', 'bame5'),
                                                   labels = c('Age', 'Education', 'Ethnicity', 
                                                              'Marital Status', 'Sex', 
                                                              'Deprivation', 'Ethnic Mix')), 
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
           text = element_text(size = textSize)); standChangePlot.allConfounders


if(saveImage){
  ggplot2::ggsave(filename = paste0(resDir_figures, '/standChangePlot.allConfounders.png'),
                  plot = standChangePlot.allConfounders,
                  height = height, width = 2*width)  
}

### individual x community confounder profiles ----

standChangePlotData.jointConfounders <- 
  theta.predictor %>%
  tidyr::pivot_longer(cols = c('age', 'edu', 'ethn', 'marStat', 'sex'), names_to = 'indConfounder', values_to = 'indCategory') %>% 
  tidyr::pivot_longer(cols = c('imd10', 'bame5'), names_to = 'comConfounder', values_to = 'comCategory') %>% 
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
                                                         levels = c('imd10', 'bame5'),
                                                         labels = c('Deprivation', 'Ethnic Mix')),
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
                                                         levels = c('Deprivation', 'Ethnic Mix')),
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
  my.theme(text = element_text(size = textSize),
           legend.position = 'bottom',
           legend.title.align = 0.5); standChangePlot.jointConfounders

if(saveImage){
  ggplot2::ggsave(filename = paste0(resDir_figures, '/standChangePlot.jointConfounders.png'),
                  plot = standChangePlot.jointConfounders,
                  height = height, width = width)
}

# tables  ----

## temporal profiles ---- 

### exposed & control ----

allYear.exposure.temporalProfile <- 
  theta.predictor %>%
  # dplyr::filter(time %in% -3:3) %>% 
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
  # dplyr::filter(time %in% -3:3) %>%
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
  # dplyr::filter(time %in% -3:3) %>%
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

print(xtable::xtable(temporalProfile), include.rownames = FALSE)

# % latex table generated in R 4.2.1 by xtable 1.8-4 package
# % Thu Jun 22 10:54:20 2023
# \begin{table}[ht]
# \centering
# \begin{tabular}{lrrrrrrrrr}
# \hline
# time & lower\_Exposed & median\_Exposed & upper\_Exposed & lower\_Control & median\_Control & upper\_Control & diff\_lower & diff\_median & diff\_upper \\ 
# \hline
# -10 & 29.87 & 30.38 & 30.81 & 17.81 & 18.01 & 18.22 & 1.66 & 1.69 & 1.72 \\
# -9 & 32.20 & 32.62 & 33.04 & 16.17 & 16.30 & 16.44 & 1.97 & 2.00 & 2.03 \\
# -8 & 36.14 & 36.53 & 36.92 & 15.89 & 16.00 & 16.12 & 2.26 & 2.28 & 2.31 \\
# -7 & 38.49 & 38.89 & 39.26 & 15.41 & 15.54 & 15.66 & 2.48 & 2.50 & 2.53 \\
# -6 & 43.10 & 43.48 & 43.89 & 15.59 & 15.71 & 15.84 & 2.74 & 2.77 & 2.79 \\
# -5 & 44.79 & 45.22 & 45.67 & 14.82 & 14.95 & 15.09 & 3.00 & 3.02 & 3.06 \\
# -4 & 43.76 & 44.30 & 44.88 & 14.26 & 14.39 & 14.51 & 3.04 & 3.08 & 3.12 \\
# -3 & 49.28 & 49.83 & 50.49 & 14.43 & 14.56 & 14.70 & 3.38 & 3.42 & 3.47 \\
# -2 & 52.77 & 53.37 & 54.05 & 14.77 & 14.91 & 15.06 & 3.53 & 3.58 & 3.64 \\
# -1 & 58.55 & 59.28 & 60.16 & 15.89 & 16.08 & 16.28 & 3.63 & 3.69 & 3.74 \\
# 0 & 47.90 & 49.60 & 51.31 & 18.14 & 18.37 & 18.60 & 2.60 & 2.70 & 2.80 \\
# 1 & 49.98 & 51.17 & 52.36 & 19.00 & 19.18 & 19.39 & 2.60 & 2.67 & 2.73 \\
# 2 & 42.69 & 44.93 & 46.94 & 17.49 & 17.82 & 18.16 & 2.39 & 2.52 & 2.64 \\
# 3 & 36.02 & 39.66 & 43.20 & 16.20 & 16.69 & 17.26 & 2.15 & 2.37 & 2.58 \\
# 4 & 34.73 & 39.69 & 44.36 & 17.42 & 18.47 & 19.62 & 1.86 & 2.14 & 2.42 \\
# Before & 41.59 & 41.90 & 42.25 & 15.35 & 15.42 & 15.50 & 2.69 & 2.72 & 2.74 \\
# After & 48.38 & 49.51 & 50.68 & 18.36 & 18.53 & 18.72 & 2.61 & 2.67 & 2.73 \\
# All & 42.39 & 42.73 & 43.07 & 15.86 & 15.93 & 16.01 & 2.66 & 2.68 & 2.70 \\
# \hline
# \end{tabular}
# \end{table}

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

print(xtable::xtable(confounderProfile.topBottom5), include.rownames = FALSE)

# % latex table generated in R 4.2.1 by xtable 1.8-4 package
# % Thu Jun 22 11:06:49 2023
# \begin{table}[ht]
# \centering
# \begin{tabular}{lllllrrr}
# \hline
# age & edu & ethn & marStat & sex & lower & median & upper \\ 
# \hline
# [16, 25) & [GCSE, A-level or equivalent] & [Black] & [Unmarried] & [Male] & 51.01 & 55.96 & 60.69 \\
# [25, 35) & [Degree or higher] & [White] & [Married or civil partnership] & [Female] & 42.13 & 51.22 & 60.13 \\
# [25, 35) & [Below GCSE and other] & [Black] & [Unmarried] & [Female] & 42.67 & 47.00 & 50.73 \\
# [45, 55) & [GCSE, A-level or equivalent] & [Black] & [Married or civil partnership] & [Male] & 39.95 & 46.25 & 52.25 \\
# [25, 35) & [Below GCSE and other] & [Other] & [Unmarried] & [Male] & 38.14 & 45.51 & 54.62 \\
# [55, 65) & [Degree or higher] & [Mixed] & [Married or civil partnership] & [Male] & -31.82 & -22.19 & -13.34 \\
# [45, 55) & [GCSE, A-level or equivalent] & [Asian] & [Married or civil partnership] & [Male] & -26.15 & -22.91 & -19.73 \\
# [55, 65) & [Below GCSE and other] & [Black] & [Unmarried] & [Male] & -27.97 & -23.78 & -20.05 \\
# [45, 55) & [Degree or higher] & [White] & [Married or civil partnership] & [Female] & -26.70 & -24.29 & -22.09 \\
# [35, 45) & [Below GCSE and other] & [Black] & [Married or civil partnership] & [Male] & -32.46 & -25.45 & -17.21 \\
# \hline
# \end{tabular}
# \end{table}

# additional results ----
## sample characteristics ----

percentInd <-
  data.frame(id = 'id',
             n = nrow(dataFinal),
             Freq = dataFinal$individual %>% unique() %>% length()); percentInd

percentAge <-
  dataFinal %>% 
  dplyr::select(individual, age) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'age') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                age = age %>% factor(., levels = c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)'))) %>% 
  dplyr::arrange(age); percentAge

percentEdu <-
  dataFinal %>% 
  dplyr::select(individual, edu) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'edu') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                edu = edu %>% factor(., levels = c('Degree or higher', 'GCSE, A-level or equivalent', 'Below GCSE and other'))) %>% 
  dplyr::arrange(edu); percentEdu

percentEthn <-
  dataFinal %>% 
  dplyr::select(individual, ethn) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'ethn') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                ethn = ethn %>% factor(., levels = c('Asian', 'Black', 'Mixed', 'Other', 'White'))) %>% 
  dplyr::arrange(ethn); percentEthn


percentMarStat <-
  dataFinal %>% 
  dplyr::select(individual, marStat) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'marStat') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                marStat = marStat %>% factor(., levels = c('Married or civil partnership', 'Unmarried'))) %>% 
  dplyr::arrange(marStat); percentMarStat


percentSex <-
  dataFinal %>% 
  dplyr::select(individual, sex) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'sex') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                sex = sex %>% factor(., levels = c('Male', 'Female'))) %>% 
  dplyr::arrange(sex); percentSex

percentDep <-
  dataFinal %>% 
  dplyr::select(individual, imd10) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'imd10') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                imd10 = imd10 %>% factor(., levels = 1:10)) %>% 
  dplyr::arrange(imd10); percentDep

percentDiv <-
  dataFinal %>% 
  dplyr::select(individual, bame5) %>% 
  dplyr::distinct() %>% 
  dplyr::select(-individual) %>% 
  dplyr::summarise(n = n(), .by = 'bame5') %>% 
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2),
                bame5 = bame5 %>% factor(., levels = 1:5)) %>% 
  dplyr::arrange(bame5); percentDiv

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
        rep('Ethnic mix relative to [1]', times = 4),
        rep('Random effect', times = 2)),
    parameter = 
      c('Intercept', 'Year', 'Intervention', 'Year+',
        'Intercept', 'Year', 'Intervention', 'Year+',
        '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)',
        '[Below GCSE and other]', '[GCSE, A-level or equivalent]',
        '[Asian]', '[Black]', '[Mixed]', '[Other]',
        '[Unmarried]',
        '[Female]',
        '[2]', '[3]', '[4]', '[5]', '[6]', '[7]', '[8]', '[9]', '[10]',
        '[2]', '[3]', '[4]', '[5]',
        'Temporal precision', 'Spatial precision'),
    lower = c(fit$summary.fixed[,3] %>% round(., digits = 2), fit$summary.hyperpar[,3] %>% round(., digits = 2)),
    median = c(fit$summary.fixed[,4] %>% round(., digits = 2), fit$summary.hyperpar[,4] %>% round(., digits = 2)),
    upper = c(fit$summary.fixed[,5] %>% round(., digits = 2), fit$summary.hyperpar[,5] %>% round(., digits = 2)))

print(xtable::xtable(fitSummary), include.rownames = FALSE)

# % latex table generated in R 4.2.1 by xtable 1.8-4 package
# % Thu Jun 22 09:52:14 2023
# \begin{table}[ht]
# \centering
# \begin{tabular}{llrrr}
# \hline
# group & parameter & lower & median & upper \\ 
# \hline
# Control ITS terms & Intercept & -1.08 & -0.84 & -0.58 \\ 
# Control ITS terms & Year & -0.01 & 0.02 & 0.05 \\ 
# Control ITS terms & Intervention & 0.17 & 0.20 & 0.24 \\ 
# Control ITS terms & Year+ & -0.19 & -0.17 & -0.15 \\ 
# Exposed ITS terms & Intercept & -0.70 & -0.67 & -0.64 \\ 
# Exposed ITS terms & Year & 0.11 & 0.12 & 0.12 \\ 
# Exposed ITS terms & Intervention & -0.54 & -0.41 & -0.28 \\ 
# Exposed ITS terms & Year+ & -0.27 & -0.21 & -0.14 \\ 
# Relative to [16, 25) & [25, 35) & 0.04 & 0.06 & 0.08 \\ 
# Relative to [16, 25) & [35, 45) & 0.13 & 0.15 & 0.16 \\ 
# Relative to [16, 25) & [45, 55) & 0.23 & 0.24 & 0.26 \\ 
# Relative to [16, 25) & [55, 65) & -0.04 & -0.03 & -0.01 \\ 
# Relative to [Degree or higher] & [Below GCSE and other] & -0.04 & -0.03 & -0.02 \\ 
# Relative to [Degree or higher] & [GCSE, A-level or equivalent] & 0.07 & 0.08 & 0.09 \\ 
# Relative to [White] & [Asian] & 0.12 & 0.14 & 0.17 \\ 
# Relative to [White] & [Black] & 0.10 & 0.12 & 0.15 \\ 
# Relative to [White] & [Mixed] & -0.19 & -0.16 & -0.12 \\ 
# Relative to [White] & [Other] & 0.29 & 0.37 & 0.45 \\ 
# Relative to [Married or civil partnership] & [Unmarried] & 0.29 & 0.30 & 0.31 \\ 
# Relative to [Male] & [Female] & 0.49 & 0.50 & 0.51 \\ 
# Deprivation relative to [1] & [2] & -0.17 & -0.15 & -0.12 \\ 
# Deprivation relative to [1] & [3] & -0.06 & -0.04 & -0.02 \\ 
# Deprivation relative to [1] & [4] & -0.23 & -0.21 & -0.19 \\ 
# Deprivation relative to [1] & [5] & -0.27 & -0.25 & -0.22 \\ 
# Deprivation relative to [1] & [6] & -0.17 & -0.15 & -0.12 \\ 
# Deprivation relative to [1] & [7] & -0.34 & -0.31 & -0.29 \\ 
# Deprivation relative to [1] & [8] & -0.43 & -0.41 & -0.39 \\ 
# Deprivation relative to [1] & [9] & -0.39 & -0.36 & -0.34 \\ 
# Deprivation relative to [1] & [10] & -0.35 & -0.33 & -0.30 \\ 
# Ethnic mix relative to [1] & [2] & 0.03 & 0.08 & 0.12 \\ 
# Ethnic mix relative to [1] & [3] & 0.03 & 0.08 & 0.12 \\ 
# Ethnic mix relative to [1] & [4] & 0.15 & 0.20 & 0.24 \\ 
# Ethnic mix relative to [1] & [5] & 0.06 & 0.11 & 0.15 \\ 
# Random effect & Temporal precision & 9.62 & 23.40 & 51.64 \\ 
# Random effect & Spatial precision & 6.67 & 7.88 & 9.25 \\ 
# \hline
# \end{tabular}
# \end{table}

### plot ----

fitSummaryPlotData <- 
  data.frame(parameter = 
               c('Intercept', 'Year', 'Intervention', 'Year+',
                 'Intercept', 'Year', 'Intervention', 'Year+',
                 '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)',
                 '[Below GCSE and other]', '[GCSE, A-level or equivalent]',
                 '[Asian]', '[Black]', '[Mixed]', '[Other]',
                 '[Unmarried]',
                 '[Female]',
                 '[2]', '[3]', '[4]', '[5]', '[6]', '[7]', '[8]', '[9]', '[10]',
                 '[2]', '[3]', '[4]', '[5]') %>% 
               factor(., levels = c('Intercept', 'Year', 'Intervention', 'Year+',
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
                       rep('Ethnic mix relative to [1]', times = 4)) %>%
               factor(., 
                      levels = c('Control ITS terms',
                                 'Exposed ITS terms',
                                 'Relative to [16, 25)',
                                 'Relative to [Degree or higher]',
                                 'Relative to [White]',
                                 'Relative to [Married or civil partnership]',
                                 'Relative to [Male]',
                                 'Deprivation relative to [1]',
                                 'Ethnic mix relative to [1]')),
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
           text = element_text(size = textSize),
           legend.position = 'bottom')
fitSummaryPlot

if(saveImage){
  ggplot2::ggsave(filename = paste0(resDir_figures, '/fitSummaryPlot.png'),
                  plot = fitSummaryPlot,
                  height = height, width = 2.5*width) 
}

## missing ltla ----

ltlaMissing.EB <-
  theta.predictor %>% 
  dplyr::select(exposed, treatment, localAuthority) %>%  
  dplyr::filter(exposed == 'Exposed', treatment == 0) %>% 
  dplyr::distinct() %>% 
  dplyr::pull(localAuthority) %>% 
  sort()

ltlaMissing.EA <-
  theta.predictor %>% 
  dplyr::select(exposed, treatment, localAuthority) %>%  
  dplyr::filter(exposed == 'Exposed', treatment == 1) %>% 
  dplyr::distinct() %>% 
  dplyr::pull(localAuthority) %>% 
  sort()

ltlaMissing.CB <-
  theta.predictor %>% 
  dplyr::select(exposed, treatment, localAuthority) %>%  
  dplyr::filter(exposed == 'Control', treatment == 0) %>% 
  dplyr::distinct() %>% 
  dplyr::pull(localAuthority) %>% 
  sort()

ltlaMissing.CA <-
  theta.predictor %>% 
  dplyr::select(exposed, treatment, localAuthority) %>%  
  dplyr::filter(exposed == 'Control', treatment == 1) %>% 
  dplyr::distinct() %>% 
  dplyr::pull(localAuthority) %>% 
  sort()

setdiff(ltlaPoly_England$LAD21NM, ltlaMissing.EB) %>% length() +
  setdiff(ltlaPoly_England$LAD21NM, ltlaMissing.EA) %>% length() +
  setdiff(ltlaPoly_England$LAD21NM, ltlaMissing.CB) %>% length() +
  setdiff(ltlaPoly_England$LAD21NM, ltlaMissing.CA) %>% length()

setdiff(ltlaPoly_England$LAD21NM, ltlaMissing.EB) %>% length()
setdiff(ltlaPoly_England$LAD21NM, ltlaMissing.EA) %>% length()
setdiff(ltlaPoly_England$LAD21NM, ltlaMissing.CB) %>% length()
setdiff(ltlaPoly_England$LAD21NM, ltlaMissing.CA) %>% length()
