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

data0 <-
  readRDS(paste0(dataDir, '/Organised Data/ukhls_dropout.rds')) %>%
  # rename
  dplyr::rename(c('id' = 'pidp', 'edu' = 'qfhigh','marStat' = 'mastat', 'jobStatus' = 'jbstat', 'ghq' = 'scghq2')) %>% 
  dplyr::mutate(country = country %>% tidyr::replace_na(., -999),
                age = age %>% tidyr::replace_na(., -999),
                jobStatus = jobStatus %>% tidyr::replace_na(., -999),
                edu = edu %>% tidyr::replace_na(., -999),
                ethn = ethn %>% tidyr::replace_na(., -999),
                marStat = marStat %>% tidyr::replace_na(., -999),
                sex = sex %>% tidyr::replace_na(., -999),
                ghq = ghq %>% tidyr::replace_na(., -999))

startData_obs <- data0 %>% nrow()

# exlusion criteria ----

## remove all but england ----

data1 <- 
  data0 %>% 
  dplyr::select(id, country) %>% 
  dplyr::filter(country != 1)

## + remove all non working age ----

data2 <- 
  data0 %>% 
  dplyr::select(id, country, age) %>%
  dplyr::filter(country == 1) %>% 
  dplyr::filter(!(age %in% 16:64))

## + remove LT sick or disabled ----

data3 <- 
  data0 %>% 
  dplyr::select(id, country, age, jobStatus) %>%
  dplyr::filter(country == 1, age %in% 16:64) %>% 
  dplyr::filter(jobStatus == 8)

## final size ----

postExlusionData <-
  data0 %>% 
  dplyr::select(id, country, age, jobStatus) %>%
  dplyr::filter(country == 1, age %in% 16:64, jobStatus != 8)

postExlusionData_obs <- postExlusionData %>% nrow()

(nrow(data0) - nrow(data1) - nrow(data2) - nrow(data3)) == postExlusionData_obs

# missingness ----

## + missing education ----

data4 <- 
  data0 %>% 
  dplyr::select(id, country, age, jobStatus, edu) %>%
  dplyr::filter(country == 1, age %in% 16:64, jobStatus != 8) %>% 
  dplyr::filter(!(edu %in% c(1:16,96)))

## + missing employment ----

data5 <- 
  data0 %>% 
  dplyr::select(id, country, age, jobStatus, edu) %>%
  dplyr::filter(country == 1, age %in% 16:64, jobStatus != 8, edu %in% c(1:16,96)) %>% 
  dplyr::filter(!(jobStatus %in% c(1:13,97)))

## + missing ethnicity ----

data6 <- 
  data0 %>% 
  dplyr::select(id, country, age, jobStatus, edu, ethn) %>%
  dplyr::filter(country == 1, age %in% 16:64, jobStatus %in% c(1:7,9:13,97), edu %in% c(1:16,96)) %>% 
  dplyr::filter(!(ethn %in% c(1:17,96)))

## + missing marital status ----

data7 <- 
  data0 %>% 
  dplyr::select(id, country, age, jobStatus, edu, ethn, marStat) %>%
  dplyr::filter(country == 1, age %in% 16:64, jobStatus %in% c(1:7,9:13,97), edu %in% c(1:16,96), ethn %in% c(1:17,96)) %>% 
  dplyr::filter(!(marStat %in% 1:10))

## + missing sex ----

data8 <- 
  data0 %>% 
  dplyr::select(id, country, age, jobStatus, edu, ethn, marStat, sex) %>%
  dplyr::filter(country == 1, age %in% 16:64, jobStatus %in% c(1:7,9:13,97), edu %in% c(1:16,96), ethn %in% c(1:17,96), marStat %in% 1:10) %>% 
  dplyr::filter(!(sex %in% 1:2))

## + outcome missing ----

data9 <-
  data0 %>% 
  dplyr::select(id, country, age, jobStatus, edu, ethn, marStat, sex, ghq) %>%
  dplyr::filter(country == 1, age %in% 16:64, jobStatus %in% c(1:7,9:13,97), edu %in% c(1:16,96), ethn %in% c(1:17,96), marStat %in% 1:10, sex %in% 1:2) %>% 
  dplyr::filter(!(ghq %in% 0:12))

## + missing ltla ----

data10 <- 
  data0 %>% 
  dplyr::select(id, country, age, jobStatus, edu, ethn, marStat, sex, ghq, lsoa11) %>%
  dplyr::filter(country == 1, age %in% 16:64, jobStatus %in% c(1:7,9:13,97), edu %in% c(1:16,96), ethn %in% c(1:17,96), marStat %in% 1:10, sex %in% 1:2, ghq %in% 0:12) %>% 
  dplyr::filter(is.na(lsoa11))

## + missing interview date ----

data11 <- 
  data0 %>% 
  dplyr::select(id, country, age, jobStatus, edu, ethn, marStat, sex, ghq, lsoa11, interviewDate) %>%
  dplyr::filter(country == 1, age %in% 16:64, jobStatus %in% c(1:7,9:13,97), edu %in% c(1:16,96), ethn %in% c(1:17,96), marStat %in% 1:10, sex %in% 1:2, ghq %in% 0:12, !is.na(lsoa11)) %>% 
  dplyr::filter(is.na(interviewDate))

## final size ----

postExlusionMissingData <- 
  data0 %>% 
  dplyr::select(id, country, age, jobStatus, edu, ethn, marStat, sex, ghq, lsoa11, interviewDate) %>%
  dplyr::filter(country == 1, age %in% 16:64, jobStatus %in% c(1:7,9:13,97), edu %in% c(1:16,96), ethn %in% c(1:17,96), marStat %in% 1:10, sex %in% 1:2, ghq %in% 0:12, !is.na(lsoa11), !is.na(interviewDate))

postExlusionMissingData_obs <- postExlusionMissingData %>% nrow()

# results ----

results1 <- 
  data.frame(parameter = c('England', 'Working age', 'LT sick or disabled', 
                           'Education', 'Employment', 'Ethnicity', 'Marriage Status', 
                           'Sex', 'Outcome', 'LTLA', 'Interview date'),
             reason = c(rep('Criteria', times = 3), rep('Missing', times = 8)),
             numberObs = c(nrow(data1), nrow(data2), nrow(data3), nrow(data4), 
                           nrow(data5), nrow(data6), nrow(data7), nrow(data8), 
                           nrow(data9), nrow(data10), nrow(data11)))

results1 

results2 <- 
  results1 %>% 
  dplyr::group_by(reason) %>% 
  dplyr::summarise(numberObs = numberObs %>% sum())

results2

# check ----

excluded_obs <- results %>% dplyr::pull(numberObs) %>% sum()
postExlusionMissingData_obs == (startData_obs - excluded_obs)


