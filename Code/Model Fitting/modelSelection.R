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

# results folder
if(!dir.exists(paths = resDir)) {
  dir.create(path = resDir)
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
                  factor(., levels = c('Control', 'Exposed')),
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

# ITS model selection ----

## hyperparameters ----

pc.u <- 1; pc.alpha <- 0.01; pc.u.phi <- 0.5; pc.alpha.phi <- 2/3
pc.st.u <- 1; pc.st.alpha <- 0.01
# pc hyper priors
hyper_pc_time <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)))
hyper_pc_space_unstruc <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)))
hyper_pc_space_struc <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)), phi = list(prior = 'pc', param = c(pc.u.phi, pc.alpha.phi)))

## formulas ----

### base ----

formula0 <- 
  paste('y ~ ', 
        paste(paste0(c('time', 'treatment', 'timeSinceTreatment', 
                       'exposed', 'time.exposed', 'treatment.exposed', 'timeSinceTreatment.exposed',
                       'age', 'edu', 'ethn', 'marStat', 'sex', 'imd10', 'bame5'), '_id'), 
              collapse = ' + '), sep = ' ') %>% 
  as.formula()

### base + time ----

formula1a <- update(formula0, ~ . + f(timeRandom_id, model = 'iid', hyper= hyper_pc_time))
formula1b <- update(formula0, ~ . + f(timeRandom_id, model = 'rw1', hyper= hyper_pc_time))

### base + space ----

formula2a <- update(formula0, ~ . + f(localAuthority_id, graph = aMat, model = 'iid', hyper= hyper_pc_space_unstruc))
formula2b <- update(formula0, ~ . + f(localAuthority_id, graph = aMat, model = 'bym2', hyper= hyper_pc_space_struc, scale.model = TRUE, adjust.for.con.comp = TRUE))

### base + time + space ----

formula3aa <- update(formula1a, ~ . + f(localAuthority_id, graph = aMat, model = 'iid', hyper= hyper_pc_space_unstruc))
formula3ab <- update(formula1a, ~ . + f(localAuthority_id, graph = aMat, model = 'bym2', hyper= hyper_pc_space_struc, scale.model = TRUE, adjust.for.con.comp = TRUE))
formula3ba <- update(formula1b, ~ . + f(localAuthority_id, graph = aMat, model = 'iid', hyper= hyper_pc_space_unstruc))
formula3bb <- update(formula1b, ~ . + f(localAuthority_id, graph = aMat, model = 'bym2', hyper= hyper_pc_space_struc, scale.model = TRUE, adjust.for.con.comp = TRUE))

## fits ----

# inla.mode = 'classic'; 
inla.mode = 'experimental';
control.compute = list(config = TRUE, waic = TRUE)
control.predictor = list(compute = TRUE, link = 1)
control.inla = list(strategy = 'adaptive', int.strategy = 'auto')
family = 'binomial'

weightsFinal <- dataFinal$sampleWeight

### base ----

fit0 <- INLA::inla(formula0, family = family, data = dataFinal, 
                   weights = weightsFinal, 
                   control.compute = control.compute, 
                   control.predictor = control.predictor, 
                   control.inla = control.inla, 
                   inla.mode = inla.mode)

### base + time ----

fit1a <- INLA::inla(formula1a, family = family, data = dataFinal, 
                    weights = weightsFinal, 
                    control.compute = control.compute, 
                    control.predictor = control.predictor, 
                    control.inla = control.inla, 
                    inla.mode = inla.mode)
fit1b <- INLA::inla(formula1b, family = family, data = dataFinal, 
                    weights = weightsFinal, 
                    control.compute = control.compute, 
                    control.predictor = control.predictor, 
                    control.inla = control.inla, 
                    inla.mode = inla.mode)

### base + space ----

fit2a <- INLA::inla(formula2a, family = family, data = dataFinal, 
                    weights = weightsFinal, 
                    control.compute = control.compute, 
                    control.predictor = control.predictor, 
                    control.inla = control.inla, 
                    inla.mode = inla.mode)
fit2b <- INLA::inla(formula2b, family = family, data = dataFinal, 
                    weights = weightsFinal, 
                    control.compute = control.compute, 
                    control.predictor = control.predictor, 
                    control.inla = control.inla, 
                    inla.mode = inla.mode)

### base + time + space ----

fit3aa <- INLA::inla(formula3aa, family = family, data = dataFinal, 
                     weights = weightsFinal, 
                     control.compute = control.compute, 
                     control.predictor = control.predictor, 
                     control.inla = control.inla, 
                     inla.mode = inla.mode)
fit3ab <- INLA::inla(formula3ab, family = family, data = dataFinal, 
                     weights = weightsFinal, 
                     control.compute = control.compute, 
                     control.predictor = control.predictor, 
                     control.inla = control.inla, 
                     inla.mode = inla.mode)
fit3ba <- INLA::inla(formula3ba, family = family, data = dataFinal, 
                     weights = weightsFinal, 
                     control.compute = control.compute, 
                     control.predictor = control.predictor, 
                     control.inla = control.inla, 
                     inla.mode = inla.mode)
fit3bb <- INLA::inla(formula3bb, family = family, data = dataFinal, 
                     weights = weightsFinal, 
                     control.compute = control.compute, 
                     control.predictor = control.predictor, 
                     control.inla = control.inla, 
                     inla.mode = inla.mode)

## scores ----

results <- 
  data.frame(model = c('fit0', 
                       paste0('fit1', c('a', 'b')), 
                       paste0('fit2', c('a', 'b')), 
                       paste0('fit3', c('aa', 'ab', 'ba', 'bb'))),
             timeModel = c('x', 'Unstructured', 'Structured', 'x', 'x', 'Unstructured', 'Unstructured', 'Structured', 'Structured'),
             spatialModel = c('x', 'x', 'x', 'unstructured', 'Structured', 'Unstructured', 'Structured', 'Untructured', 'Structured'),
             WAIC = c(fit0$waic$waic, 
                      fit1a$waic$waic, fit1b$waic$waic,
                      fit2a$waic$waic, fit2b$waic$waic,
                      fit3aa$waic$waic, fit3ab$waic$waic, fit3ba$waic$waic, fit3bb$waic$waic),
             time = c((summary(fit0)$cpu.used %>% strsplit(., ',') %>% unlist())[4] %>% stringr::str_replace(., ' Total = ', ''), 
                      (summary(fit1a)$cpu.used %>% strsplit(., ',') %>% unlist())[4] %>% stringr::str_replace(., ' Total = ', ''),
                      (summary(fit1b)$cpu.used %>% strsplit(., ',') %>% unlist())[4] %>% stringr::str_replace(., ' Total = ', ''),
                      (summary(fit2a)$cpu.used %>% strsplit(., ',') %>% unlist())[4] %>% stringr::str_replace(., ' Total = ', ''),
                      (summary(fit2b)$cpu.used %>% strsplit(., ',') %>% unlist())[4] %>% stringr::str_replace(., ' Total = ', ''),
                      (summary(fit3aa)$cpu.used %>% strsplit(., ',') %>% unlist())[4] %>% stringr::str_replace(., ' Total = ', ''),
                      (summary(fit3ab)$cpu.used %>% strsplit(., ',') %>% unlist())[4] %>% stringr::str_replace(., ' Total = ', ''),
                      (summary(fit3ba)$cpu.used %>% strsplit(., ',') %>% unlist())[4] %>% stringr::str_replace(., ' Total = ', ''),
                      (summary(fit3bb)$cpu.used %>% strsplit(., ',') %>% unlist())[4] %>% stringr::str_replace(., ' Total = ', '')),
             modelFeature = c('base', 
                              'base + time.unstructured', 
                              'base + time.structured',
                              'base + space.unstructured', 
                              'base + space.structured', 
                              'base + time.unstructured + space.unstructured',
                              'base + time.unstructured + space.structured', 
                              'base + time.structured + space.unstructured',
                              'base + time.structured + space.structured'))

### full results ----

results

# model    timeModel spatialModel    WAIC time                                  modelFeature
# 1   fit0            x            x 1564299   15                                          base
# 2  fit1a Unstructured            x 1562519 31.1                      base + time.unstructured
# 3  fit1b   Structured            x 1562518 30.6                        base + time.structured
# 4  fit2a            x unstructured 1539916 37.1                     base + space.unstructured
# 5  fit2b            x   Structured 1539917 69.3                       base + space.structured
# 6 fit3aa Unstructured Unstructured 1538232 49.7 base + time.unstructured + space.unstructured
# 7 fit3ab Unstructured   Structured 1538232 67.2   base + time.unstructured + space.structured
# 8 fit3ba   Structured  Untructured 1538232   49   base + time.structured + space.unstructured
# 9 fit3bb   Structured   Structured 1538232 62.4     base + time.structured + space.structured

print(xtable::xtable(results %>% dplyr::select(timeModel, spatialModel, WAIC)),
      include.rownames = FALSE)

# % latex table generated in R 4.2.1 by xtable 1.8-4 package
# % Fri May 26 15:51:12 2023
# \begin{table}[ht]
# \centering
# \begin{tabular}{llrl}
# \hline
# timeModel & spatialModel & WAIC & time \\ 
# \hline
# x & x & 1564291.93 & 14.1 \\ 
# Unstructured & x & 1562534.32 & 30.9 \\ 
# Structured & x & 1562533.49 & 29.9 \\ 
# x & unstructured & 1539909.13 & 36.8 \\ 
# x & Structured & 1539909.54 & 68.1 \\ 
# Unstructured & Unstructured & 1538239.44 & 50 \\ 
# Unstructured & Structured & 1538239.49 & 63.5 \\ 
# Structured & Untructured & 1538239.25 & 48.9 \\ 
# Structured & Structured & 1538239.60 & 61.9 \\ 
# \hline
# \end{tabular}
# \end{table}

### best fitting results ----

results %>% dplyr::filter(WAIC == min(WAIC))

# model  timeModel spatialModel    WAIC time                                modelFeature
# 1 fit3ba Structured  Untructured 1538239 48.9 base + time.structured + space.unstructured