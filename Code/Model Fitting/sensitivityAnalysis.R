# packages ----

library(tidyverse)
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
resDir_sensitivity <- paste0(resDir, '/Sensitivity Analysis')

# results folder
if(!dir.exists(paths = resDir)) {
  dir.create(path = resDir)
}

# model fit folder
if(!dir.exists(paths = resDir_sensitivity)) {
  dir.create(path = resDir_sensitivity)
}

# saving ----

height <- width <- 10
textSize <- 20

# imports ----

## functions ----

source(paste0(codeDir, '/functions.R'))

## spatial data ----

aMat <- readRDS('Data/Organised Data/aMat_england.rds')
aMatRegions <- data.frame(localAuthority = rownames(aMat), localAuthority_id = 1:nrow(aMat))
ltla21_rgn21_lookup <- read.csv('Data/rawData/UK Gov/ltla21_to_rgn21_england.csv')
ltla21_cnty21_lookup <- read.csv('Data/rawData/UK Gov/ltla21_to_utla21_england_wales.csv')

## survey and uc data ----

ucData <- readxl::read_excel(path = 'Data/rawData/Stat-Xplore/statxploreDat.xlsx', skip = 9)
dataImport <- readRDS('Data/Organised Data/ukhls_final.rds')

# define different datasets ----

percentage <- as.matrix(c(NA, 0.05, 0.15, 0.25, 0.35, 0.45))

contextual.awareness.data <- function(percentage, ucData, surveyData){
  
  # percentage <- 0.25; ucData <- ucData; surveyData <- dataImport
  # percentage <- NA; ucData <- ucData; surveyData <- dataImport
  
  if(!is.na(percentage)){
    
    # oraganise the stat xplore data 
    ucStartData <-
      ucData %>%
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
      dplyr::filter(total >= total[max(element)] * percentage) %>%
      ## remove all but the first time this occurs
      dplyr::filter(ucStartDate == min(ucStartDate)) %>%
      dplyr::ungroup() %>%
      dplyr::select(LAD21NM, ucStartDate) %>%
      # !!!need to include the space ' /'!!!
      dplyr::mutate(LAD21NM = sub(" /.*", "", LAD21NM))
    
  } else {
    
    # oraganise the stat xplore data 
    ucStartData <-
      ucData %>%
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
      ## remove all but the first time this occurs
      dplyr::filter(ucStartDate == min(ucStartDate)) %>%
      dplyr::ungroup() %>%
      dplyr::select(LAD21NM, ucStartDate) %>%
      # !!!need to include the space ' /'!!!
      dplyr::mutate(LAD21NM = sub(" /.*", "", LAD21NM))
    
  }
  
  # make the weights
  adjustedWeight <-
    surveyData  %>%
    select(pidp, wave, crossSectionalWeight) %>%
    dplyr::group_by(pidp) %>%
    dplyr::mutate(allWaves = dplyr::if_else(max(n()) == 11, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::select(pidp, crossSectionalWeight, allWaves) %>%
    dplyr::distinct() %>%
    tidyr::nest(data = dplyr::everything()) %>% 
    dplyr::mutate(fit = purrr::map(data, ~ glm(allWaves ~ pidp, family = 'binomial', data = .x)),
                  inclusionProbability = purrr::map(fit, 'fitted.values')) %>%
    tidyr::unnest(cols = c(data, inclusionProbability))  %>%
    dplyr::rowwise() %>%
    dplyr::mutate(adjustedWeight = crossSectionalWeight * (1/inclusionProbability)) %>%
    dplyr::ungroup() %>% 
    dplyr::select(- fit)
  
  # initially creating the data
  dataStart <- 
    surveyData %>%
    # joining data sets
    ## rename
    dplyr::rename(c('id' = 'pidp', 'localAuthority' = 'LAD21NM', 'edu' = 'qfhigh', 
                    'marStat' = 'mastat', 'jobStatus' = 'jbstat', 'ghq' = 'scghq2')) %>%
    ## uc start data
    dplyr::left_join(., ucStartData, by = c('localAuthority' = 'LAD21NM')) %>% 
    ## adjusted weight 
    dplyr::left_join(., adjustedWeight %>% dplyr::select(pidp, inclusionProbability, adjustedWeight), by = c('id' = 'pidp')) %>%
    ## localAuthority_id
    dplyr::left_join(., aMatRegions, by = 'localAuthority') %>% 
    ## region names
    dplyr::left_join(., ltla21_rgn21_lookup %>% dplyr::select(LAD21NM, RGN21NM) %>% dplyr::rename('region' = 'RGN21NM'), by = c('localAuthority' = 'LAD21NM')) %>% 
    ## county names
    dplyr::left_join(., ltla21_cnty21_lookup %>% dplyr::select(LTLA21NM, UTLA21NM) %>% dplyr::rename('county' = 'UTLA21NM'), by = c('localAuthority' = 'LTLA21NM')) %>% 
    # data processing
    ## filtering
    dplyr::mutate(country = country %>% haven::as_factor()) %>% 
    dplyr::filter(country == 'England', age %in% 16:64, jobStatus %in% c(1:7,9:13,97), edu %in% c(1:16,96),
                  ethn %in% c(1:17,96), marStat %in% 1:10, sex %in% 1:2, ghq %in% 0:12, !is.na(localAuthority), !is.na(interviewDate))
  
  dataFinal <- 
    dataStart %>% 
    # formatting
    ## grouping
    dplyr::mutate(individual = id %>% as.numeric,
                  # weights
                  sampleWeight = dplyr::case_when(is.na(adjustedWeight) ~ 0,
                                                  TRUE ~ adjustedWeight),
                  # observation
                  y = dplyr::case_when(ghq %in% 0:3 ~ 0,
                                       TRUE ~ 1),
                  # confounders
                  age = dplyr::case_when(age %in% 16:24 ~ '[16, 25)',
                                         age %in% 25:34 ~ '[25, 35)',
                                         age %in% 35:44 ~ '[35, 45)',
                                         age %in% 45:54 ~ '[45, 55)',
                                         TRUE ~ '[55, 65)') %>% 
                    factor(., levels = c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)')),
                  country = dplyr::case_when(country == 1 ~ 'England',
                                             TRUE ~ 'Remove') %>% 
                    factor(., levels = c('England')),
                  edu = dplyr::case_when(edu %in% 1:6 ~ 'Degree or higher',
                                         edu %in% 7:13 ~ 'GCSE, A-level or equivalent',
                                         TRUE ~ 'Below GCSE and other') %>% 
                    factor(., levels = c('Degree or higher', 'Below GCSE and other', 'GCSE, A-level or equivalent')),
                  ethn = dplyr::case_when(ethn %in% 1:4 ~ 'White',
                                          ethn %in% 5:8 ~ 'Mixed',
                                          ethn %in% 9:13 ~ 'Asian',
                                          ethn %in% 14:16 ~ 'Black',
                                          TRUE ~ 'Other') %>% 
                    factor(., levels = c('White', 'Asian', 'Black', 'Mixed', 'Other')),
                  marStat = dplyr::case_when(marStat %in% c(1,4:10) ~ 'Unmarried',
                                             TRUE ~ 'Married or civil partnership') %>% 
                    factor(., levels = c('Married or civil partnership', 'Unmarried')),
                  sex = dplyr::case_when(sex == 1 ~ 'Male',
                                         TRUE ~ 'Female') %>% 
                    factor(., levels = c('Male', 'Female')),
                  # interview and UC roll out years
                  interviewYear = interviewDate %>% format(., '%Y') %>% as.numeric(),
                  ucStartYear = ucStartDate %>% format(., '%Y') %>% as.numeric(),
                  # ITS terms
                  ## control
                  time = interviewYear - ucStartYear,
                  treatment = dplyr::if_else(ucStartYear <= interviewYear, 1, 0),
                  timeSinceTreatment = dplyr::if_else(ucStartYear <= interviewYear, interviewYear - ucStartYear + 1, 0),
                  ## exposed
                  exposed = dplyr::case_when(jobStatus %in% c(1,2,4,5,6,7,9,10,11,12,13,97) ~ 'Control',
                                             TRUE ~ 'Exposed') %>% 
                    factor(., levels = c('Exposed', 'Control')),
                  time.exposed = dplyr::case_when(exposed == 'Exposed' ~ time, TRUE ~ 0),
                  treatment.exposed = dplyr::case_when(exposed == 'Exposed' ~ treatment, TRUE ~ 0),
                  timeSinceTreatment.exposed = dplyr::case_when(exposed == 'Exposed' ~ timeSinceTreatment, TRUE ~ 0),
                  # _id terms for inla
                  ## confounders
                  age_id = age %>% as.numeric() %>% factor(),
                  country_id = country %>% as.numeric() %>% factor(),
                  edu_id = edu %>% as.numeric() %>% factor(),
                  ethn_id = ethn %>% as.numeric() %>% factor(),
                  marStat_id = marStat %>% as.numeric() %>% factor(),
                  sex_id = sex %>% as.numeric() %>% factor(),
                  ## ITS terms
                  ### control
                  time_id = time %>% as.factor() %>% as.numeric(),
                  treatment_id = treatment %>% as.numeric(),
                  timeSinceTreatment_id = timeSinceTreatment %>% as.numeric(),
                  ### exposed
                  exposed_id = exposed %>% as.numeric(),
                  time.exposed_id = dplyr::case_when(exposed == 'Exposed' ~ time_id, TRUE ~ 0),
                  treatment.exposed_id = treatment.exposed %>% as.numeric(),
                  timeSinceTreatment.exposed_id = timeSinceTreatment.exposed %>% as.numeric(),
                  ## community level covariates
                  bame5_id = bame5 %>% as.numeric() %>% factor(),
                  imd10_id = imd10 %>% as.numeric() %>% factor(),
                  ## random effects
                  timeRandom_id = interviewYear %>% factor() %>% as.numeric(),
                  localAuthority_id = localAuthority_id)
  
  return(dataFinal)
  
}

allDatas <- apply(X = percentage, 1, FUN = contextual.awareness.data, ucData = ucData, surveyData = dataImport)

# ITS model fits ----

## inla args ----

itsFormula <- 
  paste('y ~ ', 
        paste(paste0(c('time', 'treatment', 'timeSinceTreatment', 
                       'exposed', 'time.exposed', 'treatment.exposed', 'timeSinceTreatment.exposed',
                       'age', 'edu', 'ethn', 'marStat', 'sex', 'imd10', 'bame5'), '_id'), 
              collapse = ' + '), sep = ' ') %>% 
  as.formula()
# hyperparameters
pc.u <- 1; pc.alpha <- 0.01
# pc hyper priors
hyper_pc_time <- hyper_pc_space_unstruc <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)))

# add random effects
itsFormula <- update(itsFormula, ~ . + f(timeRandom_id, model = 'iid', hyper= hyper_pc_time))
itsFormula <- update(itsFormula, ~ . + f(localAuthority_id, graph = aMat, model = 'iid', hyper= hyper_pc_space_unstruc))

# inla arguments
inla.mode = c('classic', 'twostage', 'experimental')[3]
verbose = FALSE
control.compute = list(config = TRUE)
control.predictor = list(compute = TRUE, link = 1)
control.inla = list(strategy = 'adaptive', int.strategy = 'auto')
family = 'binomial'

## different models ----

fit0 <-
  INLA::inla(itsFormula, family = family, 
             data = allDatas[[1]],
             weights = allDatas[[1]]$sampleWeight,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

fit05 <-
  INLA::inla(itsFormula, family = family, 
             data = allDatas[[2]],
             weights = allDatas[[2]]$sampleWeight,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

fit15 <-
  INLA::inla(itsFormula, family = family, 
             data = allDatas[[3]],
             weights = allDatas[[3]]$sampleWeight,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

fit25 <-
  INLA::inla(itsFormula, family = family, 
             data = allDatas[[4]],
             weights = allDatas[[4]]$sampleWeight,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

fit35 <-
  INLA::inla(itsFormula, family = family, 
             data = allDatas[[5]],
             weights = allDatas[[5]]$sampleWeight,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

fit45 <-
  INLA::inla(itsFormula, family = family, 
             data = allDatas[[6]],
             weights = allDatas[[6]]$sampleWeight,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

# results ----

## fixed effects -----

## table ----

results <- 
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
    dataIntroduction = c(paste0(fit0$summary.fixed[,4] %>% round(., digits = 2), ' (', fit0$summary.fixed[,3] %>% round(., digits = 2), ', ', fit0$summary.fixed[,5] %>% round(., digits = 2), ')'),
                         paste0(fit0$summary.hyperpar[,4] %>% round(., digits = 2), ' (', fit0$summary.hyperpar[,3] %>% round(., digits = 2), ', ', fit0$summary.hyperpar[,5] %>% round(., digits = 2), ')')),
    data05pct = c(paste0(fit05$summary.fixed[,4] %>% round(., digits = 2), ' (', fit05$summary.fixed[,3] %>% round(., digits = 2), ', ', fit05$summary.fixed[,5] %>% round(., digits = 2), ')'),
                  paste0(fit05$summary.hyperpar[,4] %>% round(., digits = 2), ' (', fit05$summary.hyperpar[,3] %>% round(., digits = 2), ', ', fit05$summary.hyperpar[,5] %>% round(., digits = 2), ')')),
    data15pct = c(paste0(fit15$summary.fixed[,4] %>% round(., digits = 2), ' (', fit15$summary.fixed[,3] %>% round(., digits = 2), ', ', fit15$summary.fixed[,5] %>% round(., digits = 2), ')'),
                  paste0(fit15$summary.hyperpar[,4] %>% round(., digits = 2), ' (', fit15$summary.hyperpar[,3] %>% round(., digits = 2), ', ', fit15$summary.hyperpar[,5] %>% round(., digits = 2), ')')),
    data25pct = c(paste0(fit25$summary.fixed[,4] %>% round(., digits = 2), ' (', fit25$summary.fixed[,3] %>% round(., digits = 2), ', ', fit25$summary.fixed[,5] %>% round(., digits = 2), ')'),
                  paste0(fit25$summary.hyperpar[,4] %>% round(., digits = 2), ' (', fit25$summary.hyperpar[,3] %>% round(., digits = 2), ', ', fit25$summary.hyperpar[,5] %>% round(., digits = 2), ')')),
    data35pct = c(paste0(fit35$summary.fixed[,4] %>% round(., digits = 2), ' (', fit35$summary.fixed[,3] %>% round(., digits = 2), ', ', fit35$summary.fixed[,5] %>% round(., digits = 2), ')'),
                  paste0(fit35$summary.hyperpar[,4] %>% round(., digits = 2), ' (', fit35$summary.hyperpar[,3] %>% round(., digits = 2), ', ', fit35$summary.hyperpar[,5] %>% round(., digits = 2), ')')),
    data45pct = c(paste0(fit45$summary.fixed[,4] %>% round(., digits = 2), ' (', fit45$summary.fixed[,3] %>% round(., digits = 2), ', ', fit45$summary.fixed[,5] %>% round(., digits = 2), ')'),
                  paste0(fit45$summary.hyperpar[,4] %>% round(., digits = 2), ' (', fit45$summary.hyperpar[,3] %>% round(., digits = 2), ', ', fit45$summary.hyperpar[,5] %>% round(., digits = 2), ')')))

print(xtable::xtable(results), include.rownames = FALSE)

# % latex table generated in R 4.2.1 by xtable 1.8-4 package
# % Thu Jun  8 12:30:28 2023
# \begin{table}[ht]
# \centering
# \begin{tabular}{llllllll}
# \hline
# group & parameter & dataIntroduction & data05pct & data15pct & data25pct & data35pct & data45pct \\ 
# \hline
# Control ITS terms & Intercept & -0.28 (-0.72, 0.17) & -0.83 (-1.08, -0.55) & -0.75 (-0.96, -0.52) & -0.84 (-1.08, -0.58) & -0.65 (-0.9, -0.36) & -0.68 (-0.89, -0.46) \\ 
# Control ITS terms & Year & -0.02 (-0.18, 0.13) & 0.01 (-0.02, 0.03) & 0 (-0.02, 0.02) & 0.02 (-0.01, 0.05) & 0.02 (-0.02, 0.04) & 0.01 (-0.01, 0.03) \\ 
# Control ITS terms & Intervention & -0.13 (-0.51, 0.27) & 0.02 (0, 0.05) & 0.12 (0.09, 0.15) & 0.2 (0.17, 0.24) & 0.03 (-0.01, 0.06) & 0.03 (-0.01, 0.07) \\ 
# Control ITS terms & Year+ & 0.05 (-0.12, 0.21) & -0.03 (-0.04, -0.02) & -0.04 (-0.06, -0.03) & -0.17 (-0.19, -0.15) & -0.1 (-0.12, -0.08) & -0.05 (-0.08, -0.03) \\ 
# Exposed ITS terms & Intercept & -0.87 (-0.91, -0.82) & -0.66 (-0.7, -0.63) & -0.68 (-0.71, -0.65) & -0.67 (-0.7, -0.64) & -0.76 (-0.79, -0.73) & -0.74 (-0.77, -0.71) \\ 
# Exposed ITS terms & Year & 0.03 (0.01, 0.05) & 0.09 (0.08, 0.1) & 0.11 (0.11, 0.12) & 0.12 (0.11, 0.12) & 0.09 (0.08, 0.09) & 0.1 (0.09, 0.1) \\ 
# Exposed ITS terms & Intervention & 0.48 (0.43, 0.53) & 0.22 (0.14, 0.29) & -0.24 (-0.35, -0.13) & -0.41 (-0.54, -0.28) & 0.18 (0.05, 0.31) & -0.14 (-0.29, 0.02) \\ 
# Exposed ITS terms & Year+ & 0 (-0.02, 0.02) & -0.27 (-0.3, -0.25) & -0.29 (-0.34, -0.24) & -0.21 (-0.27, -0.14) & -0.55 (-0.63, -0.47) & -0.46 (-0.57, -0.35) \\ 
# Relative to [16, 25) & [25, 35) & 0.05 (0.04, 0.07) & 0.06 (0.05, 0.08) & 0.06 (0.04, 0.08) & 0.06 (0.04, 0.08) & 0.06 (0.04, 0.08) & 0.06 (0.04, 0.07) \\ 
# Relative to [16, 25) & [35, 45) & 0.14 (0.13, 0.16) & 0.15 (0.13, 0.17) & 0.15 (0.13, 0.16) & 0.15 (0.13, 0.16) & 0.15 (0.13, 0.16) & 0.15 (0.13, 0.16) \\ 
# Relative to [16, 25) & [45, 55) & 0.24 (0.22, 0.25) & 0.25 (0.24, 0.27) & 0.25 (0.23, 0.26) & 0.24 (0.23, 0.26) & 0.25 (0.23, 0.26) & 0.25 (0.23, 0.26) \\ 
# Relative to [16, 25) & [55, 65) & -0.03 (-0.05, -0.01) & -0.02 (-0.04, 0) & -0.02 (-0.04, -0.01) & -0.03 (-0.04, -0.01) & -0.02 (-0.04, -0.01) & -0.02 (-0.04, -0.01) \\ 
# Relative to [Degree or higher] & [Below GCSE and other] & -0.03 (-0.04, -0.02) & -0.03 (-0.04, -0.02) & -0.03 (-0.04, -0.02) & -0.03 (-0.04, -0.02) & -0.03 (-0.04, -0.02) & -0.03 (-0.04, -0.02) \\ 
# Relative to [Degree or higher] & [GCSE, A-level or equivalent] & 0.07 (0.06, 0.08) & 0.08 (0.07, 0.09) & 0.08 (0.07, 0.09) & 0.08 (0.07, 0.09) & 0.08 (0.07, 0.09) & 0.08 (0.07, 0.08) \\ 
# Relative to [White] & [Asian] & 0.14 (0.12, 0.16) & 0.15 (0.13, 0.17) & 0.15 (0.13, 0.17) & 0.14 (0.12, 0.17) & 0.15 (0.13, 0.17) & 0.14 (0.12, 0.16) \\ 
# Relative to [White] & [Black] & 0.13 (0.11, 0.16) & 0.14 (0.12, 0.17) & 0.13 (0.11, 0.16) & 0.12 (0.1, 0.15) & 0.14 (0.12, 0.16) & 0.14 (0.12, 0.16) \\ 
# Relative to [White] & [Mixed] & -0.15 (-0.19, -0.12) & -0.15 (-0.19, -0.12) & -0.16 (-0.19, -0.12) & -0.16 (-0.19, -0.12) & -0.16 (-0.19, -0.12) & -0.15 (-0.19, -0.12) \\ 
# Relative to [White] & [Other] & 0.37 (0.28, 0.45) & 0.39 (0.31, 0.47) & 0.38 (0.3, 0.46) & 0.37 (0.29, 0.45) & 0.38 (0.3, 0.46) & 0.37 (0.29, 0.46) \\ 
# Relative to [Married or civil partnership] & [Unmarried] & 0.3 (0.29, 0.31) & 0.3 (0.29, 0.31) & 0.3 (0.29, 0.31) & 0.3 (0.29, 0.31) & 0.3 (0.29, 0.31) & 0.3 (0.29, 0.31) \\ 
# Relative to [Male] & [Female] & 0.5 (0.49, 0.51) & 0.5 (0.49, 0.51) & 0.5 (0.49, 0.51) & 0.5 (0.49, 0.51) & 0.5 (0.49, 0.51) & 0.5 (0.49, 0.51) \\ 
# Deprivation relative to [1] & [2] & -0.15 (-0.17, -0.12) & -0.14 (-0.17, -0.12) & -0.15 (-0.17, -0.12) & -0.15 (-0.17, -0.12) & -0.14 (-0.17, -0.12) & -0.14 (-0.17, -0.12) \\ 
# Deprivation relative to [1] & [3] & -0.04 (-0.07, -0.02) & -0.04 (-0.06, -0.02) & -0.04 (-0.06, -0.02) & -0.04 (-0.06, -0.02) & -0.04 (-0.06, -0.02) & -0.04 (-0.06, -0.02) \\ 
# Deprivation relative to [1] & [4] & -0.21 (-0.24, -0.19) & -0.21 (-0.23, -0.19) & -0.21 (-0.23, -0.19) & -0.21 (-0.23, -0.19) & -0.21 (-0.23, -0.19) & -0.21 (-0.23, -0.19) \\ 
# Deprivation relative to [1] & [5] & -0.25 (-0.27, -0.22) & -0.24 (-0.26, -0.22) & -0.25 (-0.27, -0.22) & -0.25 (-0.27, -0.22) & -0.24 (-0.27, -0.22) & -0.24 (-0.26, -0.22) \\ 
# Deprivation relative to [1] & [6] & -0.15 (-0.17, -0.12) & -0.14 (-0.17, -0.12) & -0.14 (-0.17, -0.12) & -0.15 (-0.17, -0.12) & -0.14 (-0.17, -0.12) & -0.14 (-0.17, -0.12) \\ 
# Deprivation relative to [1] & [7] & -0.32 (-0.34, -0.3) & -0.31 (-0.34, -0.29) & -0.31 (-0.34, -0.29) & -0.31 (-0.34, -0.29) & -0.32 (-0.34, -0.29) & -0.31 (-0.34, -0.29) \\ 
# Deprivation relative to [1] & [8] & -0.41 (-0.44, -0.39) & -0.41 (-0.43, -0.38) & -0.41 (-0.43, -0.39) & -0.41 (-0.43, -0.39) & -0.41 (-0.43, -0.39) & -0.41 (-0.43, -0.38) \\ 
# Deprivation relative to [1] & [9] & -0.37 (-0.39, -0.34) & -0.36 (-0.38, -0.33) & -0.36 (-0.38, -0.34) & -0.36 (-0.39, -0.34) & -0.36 (-0.38, -0.34) & -0.36 (-0.38, -0.34) \\ 
# Deprivation relative to [1] & [10] & -0.33 (-0.36, -0.31) & -0.32 (-0.35, -0.3) & -0.33 (-0.35, -0.3) & -0.33 (-0.35, -0.3) & -0.33 (-0.35, -0.3) & -0.33 (-0.35, -0.3) \\ 
# Ethnic mix relative to [1] & [2] & 0.07 (0.03, 0.12) & 0.07 (0.03, 0.12) & 0.08 (0.03, 0.12) & 0.08 (0.03, 0.12) & 0.08 (0.03, 0.12) & 0.08 (0.03, 0.12) \\ 
# Ethnic mix relative to [1] & [3] & 0.07 (0.03, 0.12) & 0.07 (0.03, 0.12) & 0.08 (0.03, 0.12) & 0.08 (0.03, 0.12) & 0.08 (0.03, 0.12) & 0.07 (0.03, 0.12) \\ 
# Ethnic mix relative to [1] & [4] & 0.19 (0.15, 0.24) & 0.19 (0.15, 0.24) & 0.2 (0.15, 0.24) & 0.2 (0.15, 0.24) & 0.19 (0.15, 0.24) & 0.19 (0.15, 0.24) \\ 
# Ethnic mix relative to [1] & [5] & 0.1 (0.06, 0.15) & 0.1 (0.05, 0.15) & 0.11 (0.06, 0.15) & 0.11 (0.06, 0.15) & 0.1 (0.06, 0.15) & 0.1 (0.06, 0.15) \\ 
# Random effect & Temporal precision & 34.81 (12.19, 87.06) & 29.9 (12.21, 67.57) & 36.14 (14.61, 80.4) & 23.4 (9.62, 51.64) & 25.05 (10.06, 56.57) & 35.21 (14.32, 77.1) \\ 
# Random effect & Spatial precision & 7.96 (6.74, 9.34) & 7.91 (6.7, 9.29) & 7.92 (6.71, 9.3) & 7.88 (6.67, 9.25) & 7.9 (6.69, 9.27) & 7.93 (6.71, 9.3) \\ 
# \hline
# \end{tabular}
# \end{table}

## plot ----

resultsPlotData <- 
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
               rep(., times = 6) %>% 
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
               rep(., times = 6) %>% 
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
             data = c(rep('Introduction Data', times = 33), rep('5% Data', times = 33), rep('15% Data', times = 33), rep('25% Data', times = 33), rep('35% Data', times = 33), rep('45% Data', times = 33)) %>% 
               factor(., levels = c('Introduction Data', '5% Data', '15% Data', '25% Data', '35% Data', '45% Data')),
             lower = c(fit0$summary.fixed[,3], fit05$summary.fixed[,3], fit15$summary.fixed[,3], fit25$summary.fixed[,3], fit35$summary.fixed[,3], fit45$summary.fixed[,3]),
             median = c(fit0$summary.fixed[,4], fit05$summary.fixed[,4], fit15$summary.fixed[,4], fit25$summary.fixed[,4], fit35$summary.fixed[,4], fit45$summary.fixed[,4]),
             upper = c(fit0$summary.fixed[,5], fit05$summary.fixed[,5], fit15$summary.fixed[,5], fit25$summary.fixed[,5], fit35$summary.fixed[,5], fit45$summary.fixed[,5])) %>% 
  dplyr::mutate(value = dplyr::case_when(lower < 0 & upper < 0 ~ 'Negative',
                                         lower < 0 & 0 < upper ~ 'Random Noise',
                                         0 < lower & 0 < upper ~ 'Positive',
                                         TRUE ~ 'Error') %>% factor(., levels = c('Negative', 'Random Noise', 'Positive', 'Error')))


resultsPlot <-
  ggplot2::ggplot(resultsPlotData, 
                  aes(x = parameter, y = median, group = interaction(data, parameter), colour = data)) +
  ggplot2::geom_hline(yintercept = 0, colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point(position = position_dodge(width = 0.5)) +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), width = .05, position = position_dodge(width = 0.5)) +
  ggplot2::scale_colour_manual(values = c('red3', 'green4', 'orange2', 'blue3', 'purple3', 'pink2')) + 
  ggplot2::labs(x = '', y = '') +
  ggplot2::facet_wrap(~ group, scales = 'free') +
  my.theme(legend.title = element_blank(),
           text = element_text(size = textSize),
           legend.position = 'top'); resultsPlot

ggplot2::ggsave(filename = paste0(resDir_sensitivity, '/sensitivityAnalysis2.png'),
                plot = resultsPlot,
                height = height, width = 2.5*width) 

## exposed vs control ----


extract.samples <- function(data, fit, nSims, ...){
  
  
  set.seed(11)
  
  ### marginals from INLA ----
  
  sampAll <- INLA::inla.posterior.sample(n = nSims, result = fit, intern = TRUE, ...) # <--- remove selection from havard
  
  ### linear predictor ----
  
  theta.predictor.a <-
    lapply(X = sampAll,
           FUN = function(x) {x$latent[startsWith(rownames(x$latent), 'Predictor:')]}) %>%
    unlist() %>%
    matrix(., ncol = nSims)
  colnames(theta.predictor.a) <- paste0('theta:', 1:nSims)
  theta.predictor <- 
    cbind(data %>% 
            dplyr::select(localAuthority, county, region, country, 
                          ucStartYear, interviewYear, time, treatment, exposed, 
                          age, edu, ethn, marStat, sex, bame5, imd10), 
          theta.predictor.a) %>% 
    arrange(time, treatment, localAuthority, region, country, exposed)
  
}


theta.predictor.0 <- extract.samples(data = allDatas[[1]], fit = fit0, nSims = 1000)
theta.predictor.05 <- extract.samples(data = allDatas[[2]], fit = fit05, nSims = 1000)
theta.predictor.15 <- extract.samples(data = allDatas[[3]], fit = fit15, nSims = 1000)
theta.predictor.25 <- extract.samples(data = allDatas[[4]], fit = fit25, nSims = 1000)
theta.predictor.35 <- extract.samples(data = allDatas[[5]], fit = fit35, nSims = 1000)
theta.predictor.45 <- extract.samples(data = allDatas[[6]], fit = fit45, nSims = 1000)

theta.predictor <- 
  rbind(theta.predictor.0 %>% dplyr::mutate(dataType = 'Introduction Data'),
        theta.predictor.15 %>% dplyr::mutate(dataType = '5% Data'),
        theta.predictor.15 %>% dplyr::mutate(dataType = '15% Data'),
        theta.predictor.25 %>% dplyr::mutate(dataType = '25% Data'),
        theta.predictor.35 %>% dplyr::mutate(dataType = '35% Data'),
        theta.predictor.45 %>% dplyr::mutate(dataType = '45% Data')) %>% 
  dplyr::mutate(dataType =  dataType %>% factor(., levels = c('Introduction Data',
                                                              '5% Data', '15% Data', '25% Data', '35% Data', '45% Data')))

plotData <-
  theta.predictor %>%
  dplyr::filter(time %in% -3:3) %>%
  dplyr::mutate(time = time %>% factor(., labels = c(-3:-1, 'Start', 1:3))) %>%
  # theta.predictor %>% 
  # dplyr::mutate(time = time %>% factor(., labels = c(-11:-1, 'Start', 1:8))) %>%
  # # dplyr::filter(time != 5) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'time', 'dataType')) %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  expit() %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:'))

plot <-
  ggplot2::ggplot(data = plotData, aes(x = time, y = median,
                                       group = interaction(exposed, dataType),
                                       colour = exposed,
                                       linetype = dataType,
                                       shape = dataType)) +
  ggplot2::geom_vline(xintercept = 'Start', colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::scale_colour_manual(values = c('red3', 'blue3')) +
  ggplot2::scale_linetype_manual(values= c('solid', 'dashed', 'dotted', 'dotdash', 'longdash', 'twodash')) +
  ggplot2::scale_shape_manual(values= c('circle', 'square', 'triangle', 'diamond', 'plus', 'asterisk')) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::labs(x = 'Time since awareness to Universal Credit (years)', y = 'Psychological distress prevalence (%)') +
  my.theme(legend.title = element_blank(),
           legend.position = 'bottom',
           text = element_text(size = textSize)); plot

ggplot2::ggsave(filename = paste0(resDir_sensitivity, '/sensitivityAnalysis1.png'),
                plot = plot,
                height = height, width = 2.5*width) 
