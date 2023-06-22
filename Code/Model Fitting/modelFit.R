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
resDir_modelFit <- paste0(resDir, '/Model Fit')

# results folder
if(!dir.exists(paths = resDir)) {
  dir.create(path = resDir)
}

# model fit folder
if(!dir.exists(paths = resDir_modelFit)) {
  dir.create(path = resDir_modelFit)
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

## StatXplore - define UC ----

# import so the row names are the months
statXplore <- readxl::read_excel(path = 'Data/rawData/Stat-Xplore/statxploreDat.xlsx', skip = 9)
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

dataImport <- readRDS('Data/Organised Data/ukhls_final.rds')

### adjusted weights ----

adjustedWeight <-
  dataImport %>%
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

### data starting point ----

dataStart <- 
  dataImport %>%
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

### data final ----

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
                country = country %>% factor(),
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

# model fit ----

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
hyper_pc <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)))

# add random effects
itsFormula <- update(itsFormula, ~ . + f(timeRandom_id, model = 'iid', hyper= hyper_pc))
itsFormula <- update(itsFormula, ~ . + f(localAuthority_id, graph = aMat, model = 'iid', hyper= hyper_pc))

# inla arguments
inla.mode <- c('classic', 'twostage', 'experimental')[3]
verbose <- FALSE
control.compute <- list(config = TRUE)
control.predictor <- list(compute = TRUE, link = 1)
control.inla <- list(strategy = 'adaptive', int.strategy = 'auto')
family <- 'binomial'

# weights
weightsFinal <- dataFinal$sampleWeight

# model fit
fit <-
  INLA::inla(itsFormula, family = family, 
             data = dataFinal,
             weights = weightsFinal,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

# prevalence predictions ----

set.seed(11)

## marginals from INLA ----

nSims <- 1000
sampAll <- INLA::inla.posterior.sample(n = nSims, result = fit, intern = TRUE, verbose = verbose)

## linear predictor ----

theta.predictor.a <-
  lapply(X = sampAll,
         FUN = function(x) {x$latent[startsWith(rownames(x$latent), 'Predictor:')]}) %>%
  unlist() %>%
  matrix(., ncol = nSims)
colnames(theta.predictor.a) <- paste0('theta:', 1:nSims)
theta.predictor <- 
  cbind(dataFinal %>% 
          dplyr::select(localAuthority, county, region, country, 
                        ucStartYear, interviewYear, time, treatment, exposed, 
                        age, edu, ethn, marStat, sex, bame5, imd10), 
        theta.predictor.a) %>% 
  arrange(time, treatment, localAuthority, region, country, exposed)

## observations ----

theta.observation.a <-
  theta.predictor.a %>%
  lapply(X = .,
         FUN = function(x) {rbinom(prob = expit(x), n = 1, size = 1)}) %>%
  unlist() %>%
  matrix(., ncol = nSims)
colnames(theta.observation.a) <- paste0('theta:', 1:nSims)
theta.observation <- 
  cbind(dataFinal %>% 
          dplyr::select(localAuthority, county, region, country, 
                        ucStartYear, interviewYear, time, treatment, exposed, 
                        age, edu, ethn, marStat, sex, bame5, imd10), 
        theta.observation.a) %>% 
  arrange(time, treatment, localAuthority, region, country, exposed)

## spatial random effect ----

theta.space.a <- 
  lapply(X = sampAll, 
         FUN = function(x) {x$latent[startsWith(rownames(x$latent), 'localAuthority_id:')]}) %>%
  unlist() %>%
  matrix(., ncol = nSims)
colnames(theta.space.a) <- paste0('theta:', 1:nSims)
theta.space <- 
  cbind(dataFinal %>% 
          dplyr::select(localAuthority, localAuthority_id) %>% 
          dplyr::distinct() %>% 
          arrange(localAuthority_id), 
        theta.space.a)

## temporal random effect ----

theta.time.a <-
  lapply(X = sampAll,
         FUN = function(x) {x$latent[startsWith(rownames(x$latent), 'timeRandom_id:')]}) %>%
  unlist() %>%
  matrix(., ncol = nSims)
colnames(theta.time.a) <- paste0('theta:', 1:nSims)
theta.time <- 
  cbind(dataFinal %>% 
          dplyr::select(interviewYear, timeRandom_id) %>% 
          dplyr::distinct() %>% 
          arrange(timeRandom_id), 
        theta.time.a)

# save results ----

saveRDS(dataFinal, file = paste0(resDir_modelFit, '/modelData.rds'))
saveRDS(fit, file = paste0(resDir_modelFit, '/modelFit.rds'))
saveRDS(theta.predictor, file = paste0(resDir_modelFit, '/linearPredictorSamples.rds'))
saveRDS(theta.observation, file = paste0(resDir_modelFit, '/observationSamples.rds'))
saveRDS(theta.space, file = paste0(resDir_modelFit, '/spatialRandomEffectSamples.rds'))
saveRDS(theta.time, file = paste0(resDir_modelFit, '/temporalRandomEffectSamples.rds'))

