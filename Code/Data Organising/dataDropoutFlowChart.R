# packages ----

library(tidyverse)

# directories ----

# extract file location of this script
codePath <- rstudioapi::getActiveDocumentContext()$path
codePathSplitted <- strsplit(codePath, "/")[[1]]

# retrieve directories
homeDir <- paste(codePathSplitted[1: (length(codePathSplitted)-3)], collapse = "/")
codeDir <- paste0(homeDir, '/Code')
dataDir <- paste0(homeDir, '/Data')

# all data ----

setwd(dataDir)
lookup.11.21 <- read.csv('rawData/UK Gov/LSOA11_LSOA21_LAD22_lookUp.csv')
data0 <-
  readRDS('Organised Data/ukhls_dropout.rds') %>% 
  # ukhls naming convention to ons naming convention
  dplyr::rename('LSOA11CD' = 'lsoa11', 'LAD11CD' = 'oslaua') %>%
  # include LAD21 and LSOA21 codes and names
  dplyr::left_join(., lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22NM, LAD22CD), by = 'LSOA11CD', relationship = 'many-to-many') %>% 
  # rename
  dplyr::rename(c('id' = 'pidp', 'edu' = 'qfhigh','marStat' = 'mastat', 'jobStatus' = 'jbstat', 'ghq' = 'scghq2')) %>% 
  dplyr::mutate(country = country %>% haven::as_factor())
# dplyr::filter(country == 'England', age %in% 16:64, jobStatus %in% c(1:7,9:13,97), edu %in% c(1:16,96),
#               ethn %in% c(1:17,96), marStat %in% 1:10, sex %in% 1:2, ghq %in% 0:12, !is.na(LAD22NM), !is.na(interviewDate))

startData_obs <- data0 %>% nrow()

# exlusion criteria ----

## remove all but england ----

data1 <- 
  data0 %>% 
  dplyr::filter(stringr::str_detect(LSOA11CD, '^E'), country == 'England')  

nrow(data0) - nrow(data1)

## remove all non working age ----

data2 <-
  data1 %>% 
  dplyr::filter(age %in% 16:64)

nrow(data1) - nrow(data2)

## remove life time sick or unemployed ----

data3 <-
  data2 %>% 
  dplyr::filter(jobStatus != 8)

nrow(data2) - nrow(data3)

## number of people left ----

nrow(data3)

# missingness ----

## education ----

data4 <-
  data3 %>% 
  dplyr::filter(edu %in% c(1:16,96))

nrow(data3) - nrow(data4)

## employement ----

data5 <-
  data4 %>% 
  dplyr::filter(jobStatus %in% c(1:7,9:13,97))

nrow(data4) - nrow(data5)

## ethnicity ----

data6 <-
  data5 %>% 
  dplyr::filter(ethn %in% c(1:17,96))

nrow(data5) - nrow(data6)

## martial status ----

data7 <-
  data6 %>% 
  dplyr::filter(marStat %in% 1:10)

nrow(data6) - nrow(data7)

## sex ----

data8 <-
  data7 %>% 
  dplyr::filter(sex %in% 1:2)

nrow(data7) - nrow(data8)

## outcome ----

data9 <-
  data8 %>% 
  dplyr::filter(ghq %in% 0:12)

nrow(data8) - nrow(data9)

## spatial + interview date ----

data10 <-
  data9 %>% 
  dplyr::filter(!is.na(LAD22NM), !is.na(interviewDate))

nrow(data9) - nrow(data10)

## number of people left ----

nrow(data10)

