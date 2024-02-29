# packages ----

library(tidyverse)
library(sf)

# directories ---- 

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-3)], collapse = "/")
data.dir <- paste0(home.dir, '/Data')
res.dir <- paste0(home.dir, '/Results')
imd.dir <- paste0(data.dir, '/Index Multiple Deprivation')
data.population.dir <- paste0(data.dir, '/Population Totals')
data.spatial.dir <- paste0(data.dir, '/shapeFiles')
res.data.expl.dir <- paste0(res.dir, '/Data Exploration')

# shape file ----

poly.lsoa <- sf::st_read(dsn = data.spatial.dir, layer = 'ONS11_LSOA')
poly.lad <- sf::st_read(dsn = data.spatial.dir, layer = 'ONS22_LAD')
poly.nat <- sf::st_read(dsn = data.spatial.dir, layer = 'ONS21_NAT')

# neighbourhood structure ----

poly.lsoa.england <- poly.lsoa %>%  dplyr::filter(str_detect(LSOA11CD, '^E'))
poly.lad.england <- poly.lad %>%  dplyr::filter(str_detect(LAD22CD, '^E'))
poly.nat.england <- poly.nat %>%  dplyr::filter(str_detect(CTRY21CD, '^E'))
lsoa.nbs <- spdep::poly2nb(pl = as(poly.lsoa.england, 'Spatial'))

# lookup file ----

setwd(data.dir)
lookup.01.11 <- read.csv('rawData/UK Gov/LSOA01_LSOA11_LAD11_lookUp.csv')
lookup.11.21 <- read.csv('rawData/UK Gov/LSOA11_LSOA21_LAD22_lookUp.csv')

# imd files ----

setwd(imd.dir)
imd.04 <- readxl::read_excel('LSOA01_2004_IMD_SCORES.xls', sheet = 'IMD 2004')
imd.07 <- readxl::read_excel('LSOA01_2007_IMD_SCORES.xls', sheet = 'IMD 2007')
imd.10 <- readxl::read_excel('LSOA01_2010_IMD_SCORES.xls', sheet = 'IMD 2010')
imd.15 <- readxl::read_excel('LSOA11_2015_IMD_SCORES.xlsx', sheet = 'ID2015 Scores')
imd.19 <- readxl::read_excel('LSOA11_2019_IMD_SCORES.xlsx', sheet = 'IoD2019 Scores')

# population files ----

setwd(data.population.dir)
# males
population.04.males <- readxl::read_xls('LSOA11_2002_2006_MALES_POPULATION_TOTAL.xls', sheet = 'Mid-2004')
population.07.males <- readxl::read_xls('LSOA11_2007_2011_MALES_POPULATION_TOTAL.xls', sheet = 'Mid-2007')
population.10.males <- readxl::read_xls('LSOA11_2007_2011_MALES_POPULATION_TOTAL.xls', sheet = 'Mid-2010')
population.15.males <- readxl::read_xls('LSOA11_2015_POPULATION_TOTAL.xls', sheet = 'Mid-2015 Males', skip = 3)
population.19.males <- readxl::read_xlsx('LSOA11_2019_POPULATION_TOTAL.xlsx', sheet = 'Mid-2019 Males', skip = 3)
# females
population.04.females <- readxl::read_xls('LSOA11_2002_2006_FEMALES_POPULATION_TOTAL.xls', sheet = 'Mid-2004')
population.07.females <- readxl::read_xls('LSOA11_2007_2011_FEMALES_POPULATION_TOTAL.xls', sheet = 'Mid-2007')
population.10.females <- readxl::read_xls('LSOA11_2007_2011_FEMALES_POPULATION_TOTAL.xls', sheet = 'Mid-2010')
population.15.females <- readxl::read_xls('LSOA11_2015_POPULATION_TOTAL.xls', sheet = 'Mid-2015 Females', skip = 3)
population.19.females <- readxl::read_xlsx('LSOA11_2019_POPULATION_TOTAL.xlsx', sheet = 'Mid-2019 Females', skip = 3)
# all
population.04.all <- 
  dplyr::left_join(population.04.males %>% dplyr::select(LSOA11CD, all_ages) %>% dplyr::rename(MALES = all_ages),
                   population.04.females %>% dplyr::select(LSOA11CD, all_ages) %>% dplyr::rename(FEMALES = all_ages),
                   by = 'LSOA11CD') %>% 
  dplyr::mutate(POPULATION = MALES + FEMALES)
population.07.all <- 
  dplyr::left_join(population.07.males %>% dplyr::select(LSOA11CD, all_ages) %>% dplyr::rename(MALES = all_ages),
                   population.07.females %>% dplyr::select(LSOA11CD, all_ages) %>% dplyr::rename(FEMALES = all_ages),
                   by = 'LSOA11CD') %>% 
  dplyr::mutate(POPULATION = MALES + FEMALES)
population.10.all <- 
  dplyr::left_join(population.10.males %>% dplyr::select(LSOA11CD, all_ages) %>% dplyr::rename(MALES = all_ages),
                   population.10.females %>% dplyr::select(LSOA11CD, all_ages) %>% dplyr::rename(FEMALES = all_ages),
                   by = 'LSOA11CD') %>% 
  dplyr::mutate(POPULATION = MALES + FEMALES)
population.15.all <- 
  dplyr::left_join(population.15.males %>% dplyr::select(`Area Codes`, `All Ages`) %>% dplyr::rename(LSOA11CD = `Area Codes`, MALES = `All Ages`),
                   population.15.females %>% dplyr::select(`Area Codes`, `All Ages`) %>% dplyr::rename(LSOA11CD = `Area Codes`, FEMALES = `All Ages`),
                   by = 'LSOA11CD') %>% 
  dplyr::mutate(POPULATION = MALES + FEMALES)
population.19.all <- 
  dplyr::left_join(population.19.males %>% dplyr::select(`LSOA Code`, `All Ages`) %>% dplyr::rename(LSOA11CD = `LSOA Code`, MALES = `All Ages`),
                   population.19.females %>% dplyr::select(`LSOA Code`, `All Ages`) %>% dplyr::rename(LSOA11CD = `LSOA Code`, FEMALES = `All Ages`),
                   by = 'LSOA11CD') %>% 
  dplyr::mutate(POPULATION = MALES + FEMALES)

# sort data ----

## 2004 imd ----

imd.04.sorted <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.04 %>% 
                     ## Rename
                     dplyr::rename('LSOA01CD' = 'SOA',
                                   'imdScore' = 'IMD SCORE') %>% 
                     ## join the LSOA11CD
                     dplyr::left_join(., 
                                      lookup.01.11 %>% 
                                        dplyr::select(LSOA01CD, LSOA11CD),
                                      by = 'LSOA01CD',
                                      relationship = 'many-to-many') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD
                     dplyr::summarise(imdScore = imdScore %>% mean(),
                                      .by = 'LSOA11CD'),
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.04.missing.lsoa <- 
  imd.04.sorted %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(GROUP = 'IMD 2004')

if(nrow(imd.04.missing.lsoa) > 0){
  imd.04.sorted$imdScore[is.na(imd.04.sorted$imdScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.04.sorted$imdScore))],
           FUN = function(x) { mean(imd.04.sorted$imdScore[x], na.rm = TRUE) %>% round() })
}

### lsoa ----

imd.04.sorted.lsoa <- 
  imd.04.sorted %>% 
  dplyr::select(LSOA11CD, imdScore) %>% 
  sf::st_drop_geometry() %>% 
  # define rank
  dplyr::arrange(dplyr::desc(imdScore)) %>% 
  dplyr::mutate(imdRank = 1:n(),
                DEPRIVATION = dplyr::ntile(x = imdRank, n = 10))

### lad ----

imd.04.sorted.lad <- 
  imd.04.sorted %>% 
  dplyr::select(LSOA11CD, imdScore) %>% 
  sf::st_drop_geometry() %>% 
  # join LAD codes
  dplyr::left_join(.,
                   lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD),
                   by = 'LSOA11CD') %>% 
  # join population totals 
  dplyr::left_join(.,
                   population.04.all %>% dplyr::select(LSOA11CD, POPULATION),
                   by = 'LSOA11CD') %>% 
  # population weighted average
  dplyr::summarise(imdScore = stats::weighted.mean(x = imdScore, w = POPULATION, na.rm = TRUE),
                   .by = 'LAD22CD') %>% 
  # define rank
  dplyr::arrange(dplyr::desc(imdScore)) %>% 
  dplyr::mutate(imdRank = 1:n(),
                DEPRIVATION = dplyr::ntile(x = imdRank, n = 10))

## 2007 imd ----

imd.07.sorted <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.07 %>% 
                     ## Rename
                     dplyr::rename('LSOA01CD' = 'LSOA',
                                   'imdScore' = 'IMD SCORE') %>% 
                     ## join the LSOA11CD
                     dplyr::left_join(., 
                                      lookup.01.11 %>% 
                                        dplyr::select(LSOA01CD, LSOA11CD),
                                      by = 'LSOA01CD',
                                      relationship = 'many-to-many') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD
                     dplyr::summarise(imdScore = imdScore %>% mean(),
                                      .by = 'LSOA11CD'),
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.07.missing.lsoa <- 
  imd.07.sorted %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(GROUP = 'IMD 2007')

if(nrow(imd.07.missing.lsoa) > 0){
  imd.07.sorted$imdScore[is.na(imd.07.sorted$imdScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.07.sorted$imdScore))],
           FUN = function(x) { mean(imd.07.sorted$imdScore[x], na.rm = TRUE) %>% round() })
}

### lsoa ----

imd.07.sorted.lsoa <- 
  imd.07.sorted %>% 
  dplyr::select(LSOA11CD, imdScore) %>% 
  sf::st_drop_geometry() %>% 
  # define rank
  dplyr::arrange(dplyr::desc(imdScore)) %>% 
  dplyr::mutate(imdRank = 1:n(),
                DEPRIVATION = dplyr::ntile(x = imdRank, n = 10))

### lad ----

imd.07.sorted.lad <- 
  imd.07.sorted %>% 
  dplyr::select(LSOA11CD, imdScore) %>% 
  sf::st_drop_geometry() %>% 
  # join LAD codes
  dplyr::left_join(.,
                   lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD),
                   by = 'LSOA11CD') %>% 
  # join population totals 
  dplyr::left_join(.,
                   population.07.all %>% dplyr::select(LSOA11CD, POPULATION),
                   by = 'LSOA11CD') %>% 
  # population weighted average
  dplyr::summarise(imdScore = stats::weighted.mean(x = imdScore, w = POPULATION, na.rm = TRUE),
                   .by = 'LAD22CD') %>% 
  # define rank
  dplyr::arrange(dplyr::desc(imdScore)) %>% 
  dplyr::mutate(imdRank = 1:n(),
                DEPRIVATION = dplyr::ntile(x = imdRank, n = 10)) 

## 2010 imd ----

imd.10.sorted <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.10 %>% 
                     ## Rename
                     dplyr::rename('LSOA01CD' = 'LSOA CODE',
                                   'imdScore' = 'IMD SCORE') %>% 
                     ## join the LSOA11CD
                     dplyr::left_join(., 
                                      lookup.01.11 %>% 
                                        dplyr::select(LSOA01CD, LSOA11CD),
                                      by = 'LSOA01CD',
                                      relationship = 'many-to-many') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD
                     dplyr::summarise(imdScore = imdScore %>% mean(),
                                      .by = 'LSOA11CD'),
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.10.missing.lsoa <- 
  imd.10.sorted %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(GROUP = 'IMD 2010')

if(nrow(imd.10.missing.lsoa) > 0){
  imd.10.sorted$imdScore[is.na(imd.10.sorted$imdScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.10.sorted$imdScore))],
           FUN = function(x) { mean(imd.10.sorted$imdScore[x], na.rm = TRUE) %>% round() })
}

### lsoa ----

imd.10.sorted.lsoa <- 
  imd.10.sorted %>% 
  dplyr::select(LSOA11CD, imdScore) %>% 
  sf::st_drop_geometry() %>% 
  # define rank
  dplyr::arrange(dplyr::desc(imdScore)) %>% 
  dplyr::mutate(imdRank = 1:n(),
                DEPRIVATION = dplyr::ntile(x = imdRank, n = 10))

### lad ----

imd.10.sorted.lad <- 
  imd.10.sorted %>% 
  dplyr::select(LSOA11CD, imdScore) %>% 
  dplyr::select(LSOA11CD, imdScore) %>% 
  sf::st_drop_geometry() %>% 
  # join LAD codes
  dplyr::left_join(.,
                   lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD),
                   by = 'LSOA11CD') %>% 
  # join population totals 
  dplyr::left_join(.,
                   population.10.all %>% dplyr::select(LSOA11CD, POPULATION),
                   by = 'LSOA11CD') %>% 
  # population weighted average
  dplyr::summarise(imdScore = stats::weighted.mean(x = imdScore, w = POPULATION, na.rm = TRUE),
                   .by = 'LAD22CD') %>% 
  # define rank
  dplyr::arrange(dplyr::desc(imdScore)) %>% 
  dplyr::mutate(imdRank = 1:n(),
                DEPRIVATION = dplyr::ntile(x = imdRank, n = 10))

## 2015 imd ----

imd.15.sorted <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.15 %>% 
                     ## Rename
                     dplyr::rename('LSOA11CD' = 'LSOA code (2011)',
                                   'imdScore' = 'Index of Multiple Deprivation (IMD) Score') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD (does nothing for 2015+)
                     dplyr::summarise(imdScore = imdScore %>% mean(),
                                      .by = 'LSOA11CD'),
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.15.missing.lsoa <- 
  imd.15.sorted %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(GROUP = 'IMD 2015')

if(nrow(imd.15.missing.lsoa) > 0){
  imd.15.sorted$imdScore[is.na(imd.15.sorted$imdScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.15.sorted$imdScore))],
           FUN = function(x) { mean(imd.15.sorted$imdScore[x], na.rm = TRUE) %>% round() })
}

### lsoa ----

imd.15.sorted.lsoa <- 
  imd.15.sorted %>% 
  dplyr::select(LSOA11CD, imdScore) %>% 
  sf::st_drop_geometry() %>% 
  # define rank
  dplyr::arrange(dplyr::desc(imdScore)) %>% 
  dplyr::mutate(imdRank = 1:n(),
                DEPRIVATION = dplyr::ntile(x = imdRank, n = 10))

### lad ----

imd.15.sorted.lad <- 
  imd.15.sorted %>% 
  dplyr::select(LSOA11CD, imdScore) %>% 
  sf::st_drop_geometry() %>%   
  # join LAD codes
  dplyr::left_join(.,
                   lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD),
                   by = 'LSOA11CD') %>% 
  # join population totals 
  dplyr::left_join(.,
                   population.15.all %>% dplyr::select(LSOA11CD, POPULATION),
                   by = 'LSOA11CD') %>% 
  # population weighted average
  dplyr::summarise(imdScore = stats::weighted.mean(x = imdScore, w = POPULATION, na.rm = TRUE),
                   .by = 'LAD22CD') %>% 
  # define rank
  dplyr::arrange(dplyr::desc(imdScore)) %>% 
  dplyr::mutate(imdRank = 1:n(),
                DEPRIVATION = dplyr::ntile(x = imdRank, n = 10))

## 2019 imd ----

imd.19.sorted <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.19 %>% 
                     ## Rename
                     dplyr::rename('LSOA11CD' = 'LSOA code (2011)',
                                   'imdScore' = 'Index of Multiple Deprivation (IMD) Score') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD (does nothing for 2015+)
                     dplyr::summarise(imdScore = imdScore %>% mean(),
                                      .by = 'LSOA11CD'),
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.19.missing.lsoa <- 
  imd.19.sorted %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(GROUP = 'IMD 2019')

if(nrow(imd.19.missing.lsoa) > 0){
  imd.19.sorted$imdScore[is.na(imd.19.sorted$imdScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.19.sorted$imdScore))],
           FUN = function(x) { mean(imd.19.sorted$imdScore[x], na.rm = TRUE) %>% round() })
}

### lsoa ----

imd.19.sorted.lsoa <- 
  imd.19.sorted %>% 
  dplyr::select(LSOA11CD, imdScore) %>% 
  sf::st_drop_geometry() %>% 
  # define rank
  dplyr::arrange(dplyr::desc(imdScore)) %>% 
  dplyr::mutate(imdRank = 1:n(),
                DEPRIVATION = dplyr::ntile(x = imdRank, n = 10))

### lad ----

imd.19.sorted.lad <- 
  imd.19.sorted %>% 
  dplyr::select(LSOA11CD, imdScore) %>% 
  sf::st_drop_geometry() %>%    
  # join LAD codes
  dplyr::left_join(.,
                   lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD),
                   by = 'LSOA11CD') %>% 
  # join population totals 
  dplyr::left_join(.,
                   population.15.all %>% dplyr::select(LSOA11CD, POPULATION),
                   by = 'LSOA11CD') %>% 
  # population weighted average
  dplyr::summarise(imdScore = stats::weighted.mean(x = imdScore, w = POPULATION, na.rm = TRUE),
                   .by = 'LAD22CD') %>% 
  # define rank
  dplyr::arrange(dplyr::desc(imdScore)) %>% 
  dplyr::mutate(imdRank = 1:n(),
                DEPRIVATION = dplyr::ntile(x = imdRank, n = 10))

# combine all ----

### missing ----

imd.01.21.missing <-
  dplyr::bind_rows(imd.04.missing.lsoa,
                   imd.07.missing.lsoa,
                   imd.10.missing.lsoa,
                   imd.15.missing.lsoa,
                   imd.19.missing.lsoa)

### lsoa ----

imd.01.06.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2001:2006) %>% 
  dplyr::left_join(.,
                   imd.04.sorted.lsoa,
                   by = 'LSOA11CD')

imd.07.09.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2007:2009) %>% 
  dplyr::left_join(.,
                   imd.07.sorted.lsoa,
                   by = 'LSOA11CD')

imd.10.14.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2010:2014) %>% 
  dplyr::left_join(.,
                   imd.10.sorted.lsoa,
                   by = 'LSOA11CD')

imd.15.18.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2015:2018) %>% 
  dplyr::left_join(.,
                   imd.15.sorted.lsoa,
                   by = 'LSOA11CD')

imd.19.21.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2019:2021) %>% 
  dplyr::left_join(.,
                   imd.19.sorted.lsoa,
                   by = 'LSOA11CD')

imd.01.21.lsoa <-
  dplyr::bind_rows(imd.01.06.lsoa,
                   imd.07.09.lsoa,
                   imd.10.14.lsoa,
                   imd.15.18.lsoa,
                   imd.19.21.lsoa)

nrow(poly.lsoa.england)*length(2001:2021) == nrow(imd.01.21.lsoa)


### lad ----

imd.01.06.lad <-
  expand.grid(LAD22CD = poly.lad.england$LAD22CD,
              YEAR = 2001:2006) %>% 
  dplyr::left_join(.,
                   imd.04.sorted.lad,
                   by = 'LAD22CD')

imd.07.09.lad <-
  expand.grid(LAD22CD = poly.lad.england$LAD22CD,
              YEAR = 2007:2009) %>% 
  dplyr::left_join(.,
                   imd.07.sorted.lad,
                   by = 'LAD22CD')

imd.10.14.lad <-
  expand.grid(LAD22CD = poly.lad.england$LAD22CD,
              YEAR = 2010:2014) %>% 
  dplyr::left_join(.,
                   imd.10.sorted.lad,
                   by = 'LAD22CD')

imd.15.18.lad <-
  expand.grid(LAD22CD = poly.lad.england$LAD22CD,
              YEAR = 2015:2018) %>% 
  dplyr::left_join(.,
                   imd.15.sorted.lad,
                   by = 'LAD22CD')

imd.19.21.lad <-
  expand.grid(LAD22CD = poly.lad.england$LAD22CD,
              YEAR = 2019:2021) %>% 
  dplyr::left_join(.,
                   imd.19.sorted.lad,
                   by = 'LAD22CD')

imd.01.21.lad <-
  dplyr::bind_rows(imd.01.06.lad,
                   imd.07.09.lad,
                   imd.10.14.lad,
                   imd.15.18.lad,
                   imd.19.21.lad)

nrow(poly.lad.england)*length(2001:2021) == nrow(imd.01.21.lad)

# plot ----

## arguments ----

text.size <- 20
width <- height <- 10

# theme for map plots
myMapTheme <- function(...){
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 legend.text.align=0,
                 legend.key=element_rect(fill=NA),
                 panel.background=element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 ...)
}

## lsoa ----

### 2004 imd ----

imd.04.lsoa.plot.data <-
  imd.04.sorted.lsoa %>% 
  dplyr::left_join(., 
                   lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), 
                   by = 'LSOA11CD') %>% 
  mutate(IMD = '2004 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)'))) %>% 
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.04.lsoa.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.04.lsoa.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.04.lsoa.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.04.lsoa.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.04.lsoa.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  # ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

### 2007 imd ----

imd.07.lsoa.plot.data <-
  imd.07.sorted.lsoa %>% 
  dplyr::left_join(., 
                   lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), 
                   by = 'LSOA11CD') %>% 
  mutate(IMD = '2007 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)'))) %>%
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.07.lsoa.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.07.lsoa.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.07.lsoa.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.07.lsoa.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.07.lsoa.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  # ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

### 2010 imd ----

imd.10.lsoa.plot.data <-
  imd.10.sorted.lsoa %>% 
  dplyr::left_join(., 
                   lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), 
                   by = 'LSOA11CD') %>% 
  mutate(IMD = '2010 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)'))) %>% 
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.10.lsoa.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.10.lsoa.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.10.lsoa.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.10.lsoa.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.10.lsoa.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  # ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

### 2015 imd ----

imd.15.lsoa.plot.data <-
  imd.15.sorted.lsoa %>% 
  dplyr::left_join(., 
                   lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), 
                   by = 'LSOA11CD') %>% 
  mutate(IMD = '2015 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)'))) %>% 
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.15.lsoa.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.15.lsoa.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.15.lsoa.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.15.lsoa.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.15.lsoa.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  # ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

### 2019 imd ----

imd.19.lsoa.plot.data <-
  imd.19.sorted.lsoa %>% 
  dplyr::left_join(., 
                   lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), 
                   by = 'LSOA11CD') %>% 
  mutate(IMD = '2019 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)'))) %>% 
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.19.lsoa.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.19.lsoa.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.19.lsoa.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.19.lsoa.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.19.lsoa.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  # ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

## lad ----

### 2004 imd ----

imd.04.lad.plot.data <-
  imd.04.sorted.lad %>% 
  mutate(IMD = '2004 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)'))) %>% 
  dplyr::left_join(.,
                   poly.lad.england,
                   by = 'LAD22CD') %>%
  sf::st_as_sf()

imd.04.lad.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.04.lad.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.04.lad.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.04.lad.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.04.lad.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  # ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

### 2007 imd ----

imd.07.lad.plot.data <-
  imd.07.sorted.lad %>% 
  mutate(IMD = '2007 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)'))) %>% 
  dplyr::left_join(.,
                   poly.lad.england,
                   by = 'LAD22CD') %>%
  sf::st_as_sf()

imd.07.lad.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.07.lad.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.07.lad.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.07.lad.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.07.lad.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  # ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

### 2010 imd ----

imd.10.lad.plot.data <-
  imd.10.sorted.lad %>% 
  mutate(IMD = '2010 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)'))) %>% 
  dplyr::left_join(.,
                   poly.lad.england,
                   by = 'LAD22CD') %>%
  sf::st_as_sf()

imd.10.lad.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.10.lad.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.10.lad.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.10.lad.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.10.lad.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  # ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

### 2015 imd ----

imd.15.lad.plot.data <-
  imd.15.sorted.lad %>% 
  mutate(IMD = '2015 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)'))) %>% 
  dplyr::left_join(.,
                   poly.lad.england,
                   by = 'LAD22CD') %>%
  sf::st_as_sf()

imd.15.lad.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.15.lad.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.15.lad.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.15.lad.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.15.lad.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  # ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

### 2019 imd ----

imd.19.lad.plot.data <-
  imd.19.sorted.lad %>% 
  mutate(IMD = '2019 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)'))) %>% 
  dplyr::left_join(.,
                   poly.lad.england,
                   by = 'LAD22CD') %>%
  sf::st_as_sf()

imd.19.lad.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.19.lad.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.19.lad.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.19.lad.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.19.lad.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  # ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

# save ----

setwd(data.dir)
# lsoa
save(imd.01.21.lsoa, file = 'Organised Data/LSOA11_2001_2021_IMD.rda')
# lad
save(imd.01.21.lad, file = 'Organised Data/LAD22_2001_2021_IMD.rda')

setwd(res.data.expl.dir)
print(x = xtable::xtable(x = imd.01.21.missing),
      include.rownames = FALSE,
      file = 'LSOA11_2002_2020_DEPRVATION_MISSING.txt')
# lsoa
ggplot2::ggsave(imd.04.lsoa.plot,
                filename = paste0('LSOA11_IMD_2004_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(imd.07.lsoa.plot,
                filename = paste0('LSOA11_IMD_2007_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(imd.10.lsoa.plot,
                filename = paste0('LSOA11_IMD_2010_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(imd.15.lsoa.plot,
                filename = paste0('LSOA11_IMD_2015_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(imd.19.lsoa.plot,
                filename = paste0('LSOA11_IMD_2019_PLOT.png'),
                width = width, height = height)
# lad
ggplot2::ggsave(imd.04.lad.plot,
                filename = paste0('LAD22_IMD_2004_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(imd.07.lad.plot,
                filename = paste0('LAD22_IMD_2007_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(imd.10.lad.plot,
                filename = paste0('LAD22_IMD_2010_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(imd.15.lad.plot,
                filename = paste0('LAD22_IMD_2015_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(imd.19.lad.plot,
                filename = paste0('LAD22_IMD_2019_PLOT.png'),
                width = width, height = height)