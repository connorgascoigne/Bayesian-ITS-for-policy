# packages ----

library(tidyverse)
library(sf)
library(patchwork)

# directories ----

# extract file location of this script
codePath <- rstudioapi::getActiveDocumentContext()$path
codePathSplitted <- strsplit(codePath, "/")[[1]]

# retrieve directories
homeDir <- paste(codePathSplitted[1: (length(codePathSplitted)-3)], collapse = "/")
codeDir <- paste0(homeDir, '/Code')
dataDir <- paste0(homeDir, '/Data')
spatDir <- paste0(dataDir, '/shapeFiles')
resDir <- paste0(homeDir, '/Results')
resDir_dataExploration <- paste0(resDir, '/Data Exploration')

# results folder
if(!dir.exists(paths = resDir)) {
  dir.create(path = resDir)
}

# Data exploration folder
if(!dir.exists(paths = resDir_dataExploration)) {
  dir.create(path = resDir_dataExploration)
}

# saving ----

height <- width <- 10
textSize <- 20

# imports ----

## functions ----

source(paste0(codeDir, '/functions.R'))

## spatial data ----

### load in the data ----

# spatial polygons at lsoa, lad and nat levels
poly.lsoa <- sf::st_read(dsn = spatDir, layer = 'ONS11_LSOA')
poly.lad <- sf::st_read(dsn = spatDir, layer = 'ONS22_LAD')
poly.nat <- sf::st_read(dsn = spatDir, layer = 'ONS21_NAT')

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

### raw data ----

setwd(dataDir)

# look up from 2011 lsoa to 2021 lsoa and 2021 ltla
lookup.01.11 <- read.csv('rawData/UK Gov/LSOA01_LSOA11_LAD11_lookUp.csv')
lookup.11.21 <- read.csv('rawData/UK Gov/LSOA11_LSOA21_LAD22_lookUp.csv')

# imd data
load('Organised Data/LSOA11_2002_2021_IMD.rda')
# ethncity data
load('Organised Data/LSOA11_2002_2021_ETHNIC_DIVERSITY.rda')
# UC data
statXplore <- readxl::read_excel(path = 'rawData/Stat-Xplore/statxploreDat.xlsx', skip = 9)
# survey data
surveyData <- readRDS('Organised Data/ukhls_final.rds')

### sorted data ----

# imd
imd.02.21.sorted <-
  imd.02.21 %>%
  dplyr::mutate(DEPRIVATION = imdDecile %>% factor()) %>%
  dplyr::select(LSOA11CD, YEAR, DEPRIVATION)

# ethnicity
ed.02.21.sorted <-
  ed.02.21 %>%
  dplyr::mutate(DIVERSITY =
                  dplyr::case_when(Asian + Black + Mixed + Other < 0.09 ~ 4,
                                   0.09 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other < 0.18 ~ 3,
                                   0.18 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other < 0.36 ~ 2,
                                   TRUE ~ 1) %>%
                  factor()) %>%
  dplyr::select(LSOA11CD, YEAR, DIVERSITY)

# uc
ucAwareness <- 0.25
ucStartData <-
  statXplore %>%
  # remove unwanted rows and columns
  dplyr::select(-Month) %>%
  dplyr::rename(LAD22NM = 1) %>%
  na.omit() %>%
  dplyr::filter(!LAD22NM %in% c('Unknown', 'Total')) %>%
  # mutating 
  ## '..' to NA -> NA to 0 -> character column to numeric
  dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~ dplyr::na_if(., '..')),
                dplyr::across(-LAD22NM, ~ replace(., is.na(.), 0)),
                dplyr::across(-LAD22NM, ~ as.numeric(.))) %>%
  tidyr::pivot_longer(cols = !LAD22NM,
                      names_to = c('month', 'year'),
                      names_sep = ' ',
                      values_to = 'total') %>%
  # assume its the first of each month UC starts
  dplyr::mutate(ucStartDate = paste(year, month, 1, sep = '-') %>% as.Date(., '%Y-%B-%d')) %>%
  # remove all after December 2021 (end of study period)
  dplyr::filter(ucStartDate < '2022-01-01') %>% 
  # filter all the regions great than 20% of final value
  ## group within region
  dplyr::group_by(LAD22NM) %>%
  ## arrange each region into date order
  dplyr::arrange(LAD22NM, ucStartDate) %>%
  ## add a counter
  dplyr::mutate(element = 1:n()) %>%
  ## filter for the first time the total is greater than 20% of the most recent value
  dplyr::filter(total >= total[max(element)] * ucAwareness) %>%
  ## remove all but the first time this occurs
  dplyr::filter(ucStartDate == min(ucStartDate)) %>%
  dplyr::ungroup() %>%
  dplyr::select(LAD22NM, ucStartDate) %>%
  # !!!need to include the space ' /'!!!
  dplyr::mutate(LAD22NM = sub(" /.*", "", LAD22NM))

# survey
surveyDataFinal <- 
  surveyData %>%
  # rename
  dplyr::rename(c('id' = 'pidp', 'edu' = 'qfhigh', 'marStat' = 'mastat', 'jobStatus' = 'jbstat', 'ghq' = 'scghq2'))  %>%
  # uc start data
  dplyr::left_join(., ucStartData, by = 'LAD22NM')

# plots ----

setwd(resDir_dataExploration)

## spatial maps ----

### lad map ----

ladPlot <- 
  ggplot2::ggplot() +
  # England outline filled in with white - must be first
  ggplot2::geom_sf(data = poly.nat.england, fill = NA) +
  ggplot2::geom_sf(data = poly.lad.england, fill = NA, colour = 'black') + 
  my.map.theme(text = element_text(size = textSize))
if(spatialPlotSimple){ladPlot}

ggplot2::ggsave(ladPlot,
                filename = 'ladPlot.png',
                width = width, height = height)

### lsoa map ----

lsoaPlot <- 
  ggplot2::ggplot() +
  # England outline filled in with white - must be first
  ggplot2::geom_sf(data = poly.nat.england, fill = NA) +
  ggplot2::geom_sf(data = poly.lsoa.england, fill = NA, colour = 'black') + 
  my.map.theme(text = element_text(size = textSize))
if(spatialPlotSimple){ladPlot}

ggplot2::ggsave(lsoaPlot,
                filename = 'lsoaPlot.png',
                width = width, height = height)

### UC roll out ----

ucPlotData <-
  ucStartData %>%
  dplyr::left_join(., poly.lad.england, by = 'LAD22NM') %>%
  sf::st_as_sf() %>%
  dplyr::mutate(ucStartYear = ucStartDate %>% format(., '%Y') %>% factor())

ucPlot <- 
  ggplot2::ggplot() +
  # England outline filled in with white - must be first
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = ucPlotData, aes(fill = ucStartYear)) +
  ggplot2::scale_fill_viridis_d(name = 'Universal Credit\nStart Year', direction = -1, option = 'G', na.value = 'red') +
  my.map.theme(text = element_text(size = textSize))
if(spatialPlotSimple){ucPlot}

ggplot2::ggsave(ucPlot,
                filename = 'ucPlot.png',
                width = width, height = height)

### deprivation ----

deprivationPlotData <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              imd.years = c('Deprivation 2004', 'Deprivation 2007', 'Deprivation 2010', 'Deprivation 2015', 'Deprivation 2019')) %>% 
  dplyr::left_join(.,
                   imd.02.21.sorted %>% 
                     dplyr::filter(YEAR %in% c(2004, 2007, 2010, 2015, 2019)) %>% 
                     dplyr::mutate(imd.years = paste0('Deprivation ', YEAR)) %>% 
                     dplyr::select(LSOA11CD, imd.years, DEPRIVATION),
                   by = c('LSOA11CD', 'imd.years')) %>% 
  dplyr::mutate(DEPRIVATION_LABEL = DEPRIVATION %>% factor(., labels = c('1 (Most)', 2:9, '10 (Least)'))) %>% 
  # to filter out london
  dplyr::left_join(., lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), 
                   by = 'LSOA11CD', relationship = 'many-to-many') %>% 
  dplyr::left_join(., poly.lsoa.england, by = 'LSOA11CD') %>% 
  sf::st_as_sf()

imd.years.vector <- c('Deprivation 2004', 'Deprivation 2007', 'Deprivation 2010', 'Deprivation 2015', 'Deprivation 2019')

for(i in 1:length(imd.years.vector)){
  
  # i <- 1
  
  year <- c('04', '07', '10', '15', '19')
  
  deprivationPlotData.temp <-
    deprivationPlotData %>% 
    dplyr::filter(imd.years == imd.years.vector[i])
  
  deprivationPlot_london <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = 
                       deprivationPlotData.temp %>% 
                       dplyr::filter(str_detect(LAD22CD, "^E09")), 
                     aes(fill = DEPRIVATION_LABEL),
                     colour = NA) +
    ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
    my.map.theme(legend.position = 'none'); 
  # deprivationPlot_london
  
  deprivationPlot <- 
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
    ggplot2::geom_sf(data = deprivationPlotData.temp, aes(fill = DEPRIVATION_LABEL), colour = NA) +
    ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
    # add london in seperately
    ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
    ggplot2::annotation_custom(ggplot2::ggplotGrob(deprivationPlot_london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
    ggplot2::facet_grid(~ imd.years) +
    my.map.theme(text = element_text(size = textSize),
                 legend.position = 'bottom')
  # deprivationPlot
  
  ggplot2::ggsave(deprivationPlot,
                  filename = paste0('deprivationPlot_', i, '.png'),
                  width = width, height = height)
  
  
}

### diversity ----

diversityPlotData <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              ed.years = c('Diversity 2001', 'Diversity 2011', 'Diversity 2021')) %>% 
  dplyr::left_join(.,
                   ed.02.21.sorted %>% 
                     dplyr::filter(YEAR %in% c(2002, 2011, 2021)) %>% 
                     dplyr::mutate(YEAR = dplyr::if_else(YEAR == 2002, 2001, YEAR)) %>% 
                     dplyr::mutate(ed.years = paste0('Diversity ', YEAR)) %>% 
                     dplyr::select(LSOA11CD, ed.years, DIVERSITY),
                   by = c('LSOA11CD', 'ed.years')) %>% 
  dplyr::mutate(DIVERSITY_LABEL = DIVERSITY %>% factor(., labels = c('1 (Most)', 2:3, '4 (Least)'))) %>%  
  # to filter out london
  dplyr::left_join(., lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), 
                   by = 'LSOA11CD', relationship = 'many-to-many') %>% 
  dplyr::left_join(., poly.lsoa.england, by = 'LSOA11CD') %>% 
  sf::st_as_sf()

ed.years.vector <- c('Diversity 2001', 'Diversity 2011', 'Diversity 2021')

for(i in 1:length(ed.years.vector)){
  
  # i <- 1
  
  year <- c('01', '11', '21')
  
  diversityPlotData.temp <-
    diversityPlotData %>% 
    dplyr::filter(ed.years == ed.years.vector[i])
  
  diversityPlot_london <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = 
                       diversityPlotData.temp %>% 
                       dplyr::filter(str_detect(LAD22CD, "^E09")), 
                     aes(fill = DIVERSITY_LABEL),
                     colour = NA) +
    ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
    my.map.theme(legend.position = 'none'); 
  # diversityPlot_london
  
  diversityPlot <- 
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
    ggplot2::geom_sf(data = diversityPlotData.temp, aes(fill = DIVERSITY_LABEL), colour = NA) +
    ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
    # add london in seperately
    ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
    ggplot2::annotation_custom(ggplot2::ggplotGrob(diversityPlot_london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
    ggplot2::facet_grid(~ ed.years) +
    my.map.theme(text = element_text(size = textSize),
                 legend.position = 'bottom')
  # diversityPlot
  
  ggplot2::ggsave(diversityPlot,
                  filename = paste0('diversityPlot_', i, '.png'),
                  width = width, height = height)
  
  
}

## temporal plots ----

### employment ----

employmentPrevalencePlotData <- 
  surveyDataFinal  %>% 
  # data processing
  ## filtering
  dplyr::mutate(country = country %>% haven::as_factor()) %>% 
  dplyr::filter(country == 'England', age %in% 16:64, jobStatus %in% c(1:13,97), edu %in% c(1:16,96),
                ethn %in% c(1:17,96), marStat %in% 1:10, sex %in% 1:2, ghq %in% 0:12, !is.na(LAD22NM), !is.na(interviewDate)) %>% 
  # formatting
  ## grouping
  dplyr::mutate(# observation
    y = dplyr::case_when(ghq %in% 0:3 ~ 0,
                         TRUE ~ 1),
    employ = 
      dplyr::case_when(jobStatus %in% c(1,2,4,5,6,7,9,10,11,12,13,97) ~ 'Control',
                       jobStatus == 3 ~ 'Exposed',
                       TRUE ~ 'Life-time sick or disabled') %>% 
      factor(., levels = c('Exposed', 'Control', 'Life-time sick or disabled')),
    # interview and UC roll out years
    interviewYear = interviewDate %>% format(., '%Y') %>% as.numeric(),
    ucStartYear = ucStartDate %>% format(., '%Y') %>% as.numeric(),
    # ITS terms
    ## control
    time = interviewYear - ucStartYear) %>% 
  dplyr::group_by(time, employ) %>% 
  dplyr::reframe(y = sum(y),
                 n = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(prev = y / n) %>% 
  dplyr::filter(time %in% -3:3) %>%
  dplyr::mutate(time = time %>% factor(., labels = c(-3:-1, 'Start', 1:3)))

employmentPrevalencePlot <-
  ggplot2::ggplot(employmentPrevalencePlotData, aes(x = time, y = prev, colour = employ, group = employ)) +
  ggplot2::geom_vline(xintercept = 'Start', colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::scale_colour_manual(values = c('red3', 'blue3', 'green4')) + 
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::labs(x = 'Time since awareness to Universal Credit (years)', y = 'Psychological distress prevalence (%)') +
  my.theme(legend.title = element_blank(),
           legend.position = 'top',
           text = element_text(size = textSize)) 
employmentPrevalencePlot

ggplot2::ggsave(filename = 'employmentPrevalencePlot.png',
                plot = employmentPrevalencePlot,
                height = height, width = width)

# ethnicity ----

ethnicityPrevalencePlotData <- 
  surveyDataFinal  %>% 
  # data processing
  ## filtering
  dplyr::mutate(country = country %>% haven::as_factor()) %>% 
  dplyr::filter(country == 'England', age %in% 16:64, jobStatus %in% c(1:7,9:13,97), edu %in% c(1:16,96),
                ethn %in% c(1:17,96), marStat %in% 1:10, sex %in% 1:2, ghq %in% 0:12, !is.na(LAD22NM), !is.na(interviewDate)) %>% 
  # formatting
  ## grouping
  dplyr::mutate(# observation
    y = dplyr::case_when(ghq %in% 0:3 ~ 0,
                         TRUE ~ 1),
    ethn = dplyr::case_when(ethn %in% 1:4 ~ 'White',
                            ethn %in% 5:8 ~ 'Mixed',
                            ethn %in% 9:13 ~ 'Asian',
                            ethn %in% 14:16 ~ 'Black',
                            TRUE ~ 'Other') %>% 
      factor(., levels = c('Asian', 'Black', 'Mixed', 'Other', 'White')),
    # interview and UC roll out years
    interviewYear = interviewDate %>% format(., '%Y') %>% as.numeric(),
    ucStartYear = ucStartDate %>% format(., '%Y') %>% as.numeric(),
    # ITS terms
    ## control
    time = interviewYear - ucStartYear) %>% 
  dplyr::group_by(time, ethn) %>% 
  dplyr::reframe(y = sum(y),
                 n = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(prev = y / n) %>% 
  dplyr::filter(time %in% -3:3) %>%
  dplyr::mutate(time = time %>% factor(., labels = c(-3:-1, 'Start', 1:3)))

ethnicityPrevalencePlot <-
  ggplot2::ggplot(ethnicityPrevalencePlotData, aes(x = time, y = prev, colour = ethn, group = ethn)) +
  ggplot2::geom_vline(xintercept = 'Start', colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::scale_colour_manual(values = c('red3', 'blue3', 'green4', 'orange2', 'purple3')) + 
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::labs(x = 'Time since awareness to Universal Credit (years)', y = 'Psychological distress prevalence (%)') +
  my.theme(legend.title = element_blank(),
           legend.position = 'top',
           text = element_text(size = textSize))
ethnicityPrevalencePlot

ggplot2::ggsave(filename = 'ethnicityPrevalencePlot.png',
                plot = ethnicityPrevalencePlot,
                height = height, width = width)
