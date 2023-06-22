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

onsShapePath <- 'Data/shapeFiles_ons2/'
lsoaPoly <- sf::st_read(paste0(onsShapePath, 'ons21_GBR_LSOA_shp/ons21_GBR_LSOA.shp'))
ltlaPoly <- sf::st_read(paste0(onsShapePath, 'ons21_GBR_LTLA_shp/ons21_GBR_LTLA.shp'))
countryPoly <- sf::st_read(paste0(onsShapePath, 'ons21_GBR_country_shp/ons21_GBR_country.shp'))

lsoaPoly_England <- 
  lsoaPoly %>% 
  dplyr::filter(str_detect(LSOA21CD, "^E"))
ltlaPoly_England <-
  ltlaPoly %>%
  dplyr::filter(str_detect(LAD21CD, "^E"))
countryPoly_England <-
  countryPoly %>%
  dplyr::filter(str_detect(CTRY21CD, "^E"))

# look up from 2011 lsoa to 2021 lsoa and 2021 ltla
lsoa11_lookup <- 
  read.csv('Data/rawData/UK Gov/lsoa11_to_lsao21_and_ltla22.csv') %>% 
  dplyr::rename(LAD21NM = 'LAD22NM',
                LAD21CD = 'LAD22CD') %>% 
  dplyr::filter(str_detect(LSOA11CD, "^E"))

### simplify ----

# lsoaPoly_England_simple <-
#   lsoaPoly_England %>%
#   sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)
# ltlaPoly_England_simple <-
#   ltlaPoly_England %>%
#   sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)
# countryPoly_England_simple <-
#   countryPoly_England %>%
#   sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)


# full detail for FINAL plots
lsoaPoly_England_simple <-
  lsoaPoly_England
ltlaPoly_England_simple <-
  ltlaPoly_England
countryPoly_England_simple <-
  countryPoly_England

## imd data ----

### raw ----

imd2011 <- 
  read.csv('Data/rawData/UK Gov/imd2011_lsoa.csv') %>%
  dplyr::select(`LSOA.code..2011.`,
                `LSOA.name..2011.`,
                `Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.`) %>%
  dplyr::rename('LSOA11CD' = `LSOA.code..2011.`,
                'LSOA11NM' = `LSOA.name..2011.`,
                'imdRank' = `Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.`)

### organised ----

# deprivation data
deprivationData <- 
  lsoaPoly_England %>% 
  dplyr::select(LSOA21CD, LSOA21NM) %>% 
  dplyr::left_join(., lsoa11_lookup %>% dplyr::select(LSOA11CD, LAD21CD, LAD21NM, LSOA21CD), by = 'LSOA21CD') %>% 
  dplyr::left_join(., imd2011, by = 'LSOA11CD') %>% 
  dplyr::select(LSOA11CD, LSOA21CD, LSOA21NM, LAD21CD, LAD21NM, imdRank) %>%
  # include tiles
  dplyr::mutate(imd10 = dplyr::ntile(imdRank, 10) %>% factor(., labels = 1:10),
                imd5 = dplyr::ntile(imdRank, 5) %>% factor(., labels = 1:5),
                imd4 = dplyr::ntile(imdRank, 4) %>% factor(., labels = 1:4))

## ethnicity data ----

### raw ----

ethnicity2021 <- 
  read.csv(file = 'Data/rawData/UK Gov/ethnicity2021_lsoa.csv', stringsAsFactors = TRUE) %>%
  dplyr::rename('LSOA21CD' = 'Lower.layer.Super.Output.Areas.Code',
                'ethnicityCD' = 'Ethnic.group..20.categories..Code',
                'count' = 'Observation') %>%
  # aggregate counts over bame and non-bame
  dplyr::mutate(ethnicityGroup = dplyr::if_else(ethnicityCD %in% 13:17, 'nonBameCount', 'bameCount')) %>% 
  dplyr::summarise(count = sum(count),
                   .by = c('LSOA21CD', 'ethnicityGroup')) %>%
  tidyr::pivot_wider(names_from = ethnicityGroup, 
                     values_from = count) %>% 
  dplyr::mutate(
    # percentages
    bamePercentage = bameCount/(bameCount + nonBameCount),
    nonBamePercentage = nonBameCount/(bameCount + nonBameCount),
    # rank based on percentage
    bameRank = bamePercentage %>% dplyr::desc() %>% dplyr::dense_rank())

### organised ----

# diversity data
diversityData <-
  lsoaPoly_England %>% 
  dplyr::select(LSOA21CD, LSOA21NM) %>% 
  dplyr::left_join(., lsoa11_lookup %>% dplyr::select(LSOA11CD, LAD21CD, LAD21NM, LSOA21CD), by = 'LSOA21CD') %>% 
  dplyr::left_join(., ethnicity2021, by = 'LSOA21CD') %>% 
  dplyr::select(LSOA11CD, LSOA21CD, LSOA21NM, LAD21CD, LAD21NM, bamePercentage, bameRank) %>%
  # include tiles
  dplyr::mutate(bame5 = ggplot2::cut_interval((1-bamePercentage), n = 10) %>% factor(., labels = 1:10),
                bame5 = ggplot2::cut_interval((1-bamePercentage), n = 5) %>% factor(., labels = 1:5),
                bame4 = ggplot2::cut_interval((1-bamePercentage), n = 4) %>% factor(., labels = 1:4))

## UC data ----

### raw ----

# import so the row names are the months
statXplore <- readxl::read_excel(path = 'Data/rawData/Stat-Xplore/statxploreDat.xlsx', skip = 9)

### organised ----

ucAwareness <- 0.25
ucStartData <-
  statXplore %>%
  # remove unwanted rows and columns
  dplyr::select(-Month) %>%
  dplyr::rename(LAD21NM = 1) %>%
  na.omit() %>%
  dplyr::filter(!LAD21NM %in% c('Unknown', 'Total')) %>%
  # mutating 
  ## '..' to NA -> NA to 0 -> character column to numeric
  dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~ dplyr::na_if(., '..')),
                dplyr::across(-LAD21NM, ~ replace(., is.na(.), 0)),
                dplyr::across(-LAD21NM, ~ as.numeric(.))) %>%
  tidyr::pivot_longer(cols = !LAD21NM,
                      names_to = c('month', 'year'),
                      names_sep = ' ',
                      values_to = 'total') %>%
  # assume its the first of each month UC starts
  dplyr::mutate(ucStartDate = paste(year, month, 1, sep = '-') %>% as.Date(., '%Y-%B-%d')) %>%
  # remove all after December 2021 (end of study period)
  dplyr::filter(ucStartDate < '2022-01-01') %>% 
  # filter all the regions great than 20% of final value
  ## group within region
  dplyr::group_by(LAD21NM) %>%
  ## arrange each region into date order
  dplyr::arrange(LAD21NM, ucStartDate) %>%
  ## add a counter
  dplyr::mutate(element = 1:n()) %>%
  ## filter for the first time the total is greater than 20% of the most recent value
  dplyr::filter(total >= total[max(element)] * ucAwareness) %>%
  ## remove all but the first time this occurs
  dplyr::filter(ucStartDate == min(ucStartDate)) %>%
  dplyr::ungroup() %>%
  dplyr::select(LAD21NM, ucStartDate) %>%
  # !!!need to include the space ' /'!!!
  dplyr::mutate(LAD21NM = sub(" /.*", "", LAD21NM))

## survey data ----

### raw ----

surveyData <- readRDS('Data/Organised Data/ukhls_final.rds')

### organised ----

surveyDataFinal <- 
  surveyData %>%
  # rename
  dplyr::rename(c('id' = 'pidp', 'localAuthority' = 'LAD21NM', 'edu' = 'qfhigh', 
                  'marStat' = 'mastat', 'jobStatus' = 'jbstat', 'ghq' = 'scghq2'))  %>%
  # uc start data
  dplyr::left_join(., ucStartData, by = c('localAuthority' = 'LAD21NM'))

# plots ----

## spatial maps ----

### UC roll out ----

ucPlotData <-
  ucStartData %>%
  dplyr::left_join(., ltlaPoly_England_simple, by = 'LAD21NM') %>%
  sf::st_as_sf() %>%
  dplyr::mutate(ucStartYear = ucStartDate %>% format(., '%Y') %>% factor())

ucPlot <- 
  ggplot2::ggplot() +
  # England outline filled in with white - must be first
  ggplot2::geom_sf(data = countryPoly_England_simple, fill = 'white') +
  ggplot2::geom_sf(data = ucPlotData, fill = scales::viridis_pal(option = 'G')(20)[20]) + 
  ggplot2::facet_wrap(~ ucStartYear) +
  my.map.theme(text = element_text(size = textSize))
# ucPlot

ggplot2::ggsave(ucPlot,
                filename = paste0(resDir_dataExploration, '/ucPlot.png'),
                width = width, height = height)

### deprivation ----

deprivationPlotData <-
  deprivationData %>%
  dplyr::mutate(imd10_v2 = imd10 %>% factor(., labels = c('1 (Most)', 2:9, '10 (Least)')))

deprivationPlot_london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     deprivationPlotData %>% 
                     dplyr::filter(str_detect(LAD21CD, "^E09")), 
                   aes(fill = imd10_v2),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  my.map.theme(legend.position = 'none'); 
# deprivationPlot_london

deprivationPlot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = countryPoly_England_simple, fill = 'white') +
  ggplot2::geom_sf(data = deprivationPlotData, aes(fill = imd10_v2), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(deprivationPlot_london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  my.map.theme(text = element_text(size = textSize),
               legend.position = 'bottom')
# deprivationPlot

ggplot2::ggsave(deprivationPlot,
                filename = paste0(resDir_dataExploration, '/deprivationPlot.png'),
                width = width, height = height)


### bame ----

bamePlotData <-
  diversityData %>%
  dplyr::mutate(bame5_v2 = bame5 %>% factor(., labels = c('1 (Most)', 2:4, '5 (Least)')))

bamePlot_london <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     bamePlotData %>% 
                     dplyr::filter(str_detect(LAD21CD, "^E09")), 
                   aes(fill = bame5_v2),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Ethnic Mix', direction = 1, option = 'G', na.value = 'red') +
  my.map.theme(legend.position = 'none'); 
# bamePlot_london

bamePlot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = countryPoly_England_simple, fill = 'white') +
  ggplot2::geom_sf(data = bamePlotData, aes(fill = bame5_v2), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Ethnic Mix', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(bamePlot_london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  my.map.theme(text = element_text(size = textSize),
               legend.position = 'bottom'); 
# bamePlot

ggplot2::ggsave(bamePlot,
                filename = paste0(resDir_dataExploration, '/bamePlot.png'),
                width = width, height = height)

## temporal plots ----

### employment ----

employmentPrevalencePlotData <- 
  surveyDataFinal  %>% 
  # data processing
  ## filtering
  dplyr::mutate(country = country %>% haven::as_factor()) %>% 
  dplyr::filter(country == 'England', age %in% 16:64, jobStatus %in% c(1:13,97), edu %in% c(1:16,96),
                ethn %in% c(1:17,96), marStat %in% 1:10, sex %in% 1:2, ghq %in% 0:12, !is.na(localAuthority), !is.na(interviewDate)) %>% 
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

ggplot2::ggsave(filename = paste0(resDir_dataExploration, '/employmentPrevalencePlot.png'),
                plot = employmentPrevalencePlot,
                height = height, width = width)

# ethnicity ----

ethnicityPrevalencePlotData <- 
  surveyDataFinal  %>% 
  # data processing
  ## filtering
  dplyr::mutate(country = country %>% haven::as_factor()) %>% 
  dplyr::filter(country == 'England', age %in% 16:64, jobStatus %in% c(1:7,9:13,97), edu %in% c(1:16,96),
                ethn %in% c(1:17,96), marStat %in% 1:10, sex %in% 1:2, ghq %in% 0:12, !is.na(localAuthority), !is.na(interviewDate)) %>% 
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

ggplot2::ggsave(filename = paste0(resDir_dataExploration, '/ethnicityPrevalencePlot.png'),
                plot = ethnicityPrevalencePlot,
                height = height, width = width)
