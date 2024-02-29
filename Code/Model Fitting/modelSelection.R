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
res.model.selection.dir <- paste0(res.dir, '/Model Selection')
data.organised.dir <- paste0(data.dir, '/Organised Data')

# saving ----

height <- width <- 10
text.size <- 20

# imports ----

## functions ----

source(paste0(code.dir, '/functions.R'))

## organised data ----

setwd(data.organised.dir)

load('aMat_england.rds')
load('aMat_names_england.rds')
data.import <- readRDS('ukhls_final.rds')

## StatXplore - define UC ----

setwd(data.dir)
# import so the row names are the months
stat.xplore.data <- readxl::read_excel(path = 'rawData/Stat-Xplore/statxploreDat.xlsx', skip = 9)
uc.threshold <- 0.25

uc.start.data <-
  stat.xplore.data %>%
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
  dplyr::filter(total >= total[max(element)] * uc.threshold) %>%
  ## remove all but the first time this occurs
  dplyr::filter(ucStartDate == min(ucStartDate)) %>%
  dplyr::ungroup() %>%
  dplyr::select(LAD22NM, ucStartDate) %>%
  # !!!need to include the space ' /'!!!
  dplyr::mutate(LAD22NM = sub(' /.*', '', LAD22NM))

### data starting point ----

data.start <- 
  data.import %>%
  # joining data sets
  ## rename
  dplyr::rename(c('id' = 'pidp', 'edu' = 'qfhigh', 'rela' = 'mastat', 'jobStatus' = 'jbstat', 'ghq' = 'scghq1')) %>%
  ## uc start data
  dplyr::left_join(., uc.start.data, by = 'LAD22NM') %>%
  # data processing
  ## filtering
  dplyr::mutate(country = country %>% haven::as_factor()) %>% 
  dplyr::filter(country == 'England', age %in% 16:64, jobStatus %in% c(1:7,9:13,97), edu %in% c(1:16,96),
                ethn %in% c(1:17,96), rela %in% 1:10, sex %in% 1:2, ghq %in% 0:36, !is.na(LAD22NM), !is.na(interviewDate))

### data final ----

data.final <- 
  data.start %>% 
  # select relevant columns 
  dplyr::select(
    # survey terms
    id, psu, strata,
    # spatial identifiers
    LSOA11CD,
    LAD22CD, LAD22NM, 
    # temporal terms
    ucStartDate, interviewDate, 
    # individual confounders
    jobStatus, age, edu, ethn, rela, sex, 
    # community confounders
    DIVERSITY, DEPRIVATION, 
    # observations
    ghq) %>% 
  # formatting
  dplyr::mutate(individual = id %>% as.factor() %>% as.numeric(),
                # weights
                psu = psu %>% as.factor() %>% as.numeric(),
                strata = strata %>% as.factor %>% as.numeric(),
                # observation
                y = ghq %>% as.numeric(),
                # confounders
                age = dplyr::case_when(age %in% 16:24 ~ '[16, 25)',
                                       age %in% 25:34 ~ '[25, 35)',
                                       age %in% 35:44 ~ '[35, 45)',
                                       age %in% 45:54 ~ '[45, 55)',
                                       TRUE ~ '[55, 65)') %>%  
                  factor(., levels = c('[16, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)')),
                edu = dplyr::case_when(edu %in% 1:6 ~ 'Degree or higher',
                                       edu %in% 7:13 ~ 'GCSE, A-level or equivalent',
                                       TRUE ~ 'Below GCSE and other') %>% 
                  factor(., levels = c('Below GCSE and other', 'GCSE, A-level or equivalent', 'Degree or higher')),
                ethn = dplyr::case_when(ethn %in% 1:2 ~ 'White',
                                        ethn %in% 5:8 ~ 'Mixed',
                                        ethn %in% 9:13 ~ 'Asian',
                                        ethn %in% 14:16 ~ 'Black',
                                        TRUE ~ 'Other') %>% 
                  factor(., levels = c('White', 'Asian', 'Black', 'Mixed', 'Other')),
                rela = dplyr::case_when(rela %in% c(2, 3, 10) ~ 'Relationship',
                                        TRUE ~ 'Single') %>% 
                  factor(., levels = c('Single', 'Relationship')),
                sex = dplyr::case_when(sex == 1 ~ 'Male',
                                       TRUE ~ 'Female') %>% 
                  factor(., levels = c('Male', 'Female')),
                year = interviewDate %>% lubridate::year() %>% as.numeric(),
                # ITS terms
                ## control
                # ## weekly
                # time = difftime(time1 = interviewDate, time2 = ucStartDate, units = 'weeks') %>% as.numeric() %>% floor(),
                ## monthly 
                time = (lubridate::interval(start = ucStartDate, end = interviewDate) %/% months(1)),
                # ## yearly
                # time = (lubridate::interval(start = ucStartDate, end = interviewDate) %/% months(12)),
                treatment = dplyr::if_else(ucStartDate <= interviewDate, 1, 0),
                timeSinceTreatment = dplyr::if_else(ucStartDate <= interviewDate, 
                                                    as.numeric(interviewDate - ucStartDate) + 1, 0),
                ## exposed
                exposed = dplyr::case_when(jobStatus %in% c(1,2,4,5,6,7,9,10,11,12,13,97) ~ 'Control',
                                           TRUE ~ 'Exposed') %>% 
                  factor(., levels = c('Control', 'Exposed')),
                time.exposed = dplyr::if_else(exposed == 'Exposed', time, 0),
                treatment.exposed = dplyr::if_else(exposed == 'Exposed', treatment, 0),
                timeSinceTreatment.exposed = dplyr::if_else(exposed == 'Exposed', timeSinceTreatment, 0),
                # _id terms for inla
                ## survey design
                psu_id = psu,
                strata_id = strata,
                ## confounders (as factors)
                age_id = age,
                edu_id = edu,
                ethn_id = ethn,
                rela_id = rela,
                sex_id = sex,
                ## ITS terms
                ### control
                time_id = time,
                treatment_id = treatment,
                timeSinceTreatment_id = timeSinceTreatment,
                ### exposed
                exposed_id = exposed %>% factor(levels = c('Control', 'Exposed'), labels = 1:2),
                time.exposed_id = time.exposed,
                treatment.exposed_id = treatment.exposed,
                timeSinceTreatment.exposed_id = timeSinceTreatment.exposed,
                ## community level covariates (as factors)
                diversity_id = DIVERSITY %>% relevel(., ref = 1),
                deprivation_id = DEPRIVATION %>% relevel(., ref = 10),
                ## random effects
                individual_id = id %>% as.factor() %>% as.numeric(), 
                year_id = year %>% as.factor() %>% as.numeric(),
                time.month_id = time %>% as.factor() %>% as.numeric(),
                # space_id = LAD22CD %>% as.factor() %>% as.numeric(),
                space_id = LSOA11CD %>% as.factor() %>% as.numeric(),
                error_id = 1:n())

# ITS model selection ----

## hyperparameters ----

pc.u <- 1
pc.alpha <- 0.01
pc.u.phi <- 0.5
pc.alpha.phi <- 2/3
# pc hyper priors
hyper.pc <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)))
hyper.pc.space <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)), 
                       phi = list(prior = 'pc', param = c(pc.u.phi, pc.alpha.phi)))

## formulas ----

### base ----

formula0 <- 
  paste('y ~ ', 
        paste(paste0(c('time', 'treatment', 'timeSinceTreatment', 
                       'exposed', 'time.exposed', 'treatment.exposed', 'timeSinceTreatment.exposed',
                       'age', 'edu', 'ethn', 'rela', 'sex', 'deprivation', 'diversity'), '_id'), 
              collapse = ' + '), sep = ' ') %>%
  as.formula() %>% 
  update(., ~ . + f(strata_id, model = 'iid', hyper= hyper.pc) + f(psu_id, model = 'iid', hyper= hyper.pc))

### base + time ----

formula1a <- update(formula0, ~ . + f(time.month_id, model = 'iid', hyper= hyper.pc))
formula1b <- update(formula0, ~ . + f(time.month_id, model = 'rw1', hyper= hyper.pc))

### base + space ----

formula2a <- update(formula0, ~ . + f(space_id, graph = lsoa.mat, model = 'iid', hyper= hyper.pc))
formula2b <- update(formula0, ~ . + f(space_id, graph = lsoa.mat, model = 'bym2', hyper= hyper.pc.space, scale.model = TRUE, adjust.for.con.comp = TRUE))

### base + time + space ----

formula3aa <- update(formula1a, ~ . + f(space_id, graph = lsoa.mat, model = 'iid', hyper= hyper.pc))
formula3ab <- update(formula1a, ~ . + f(space_id, graph = lsoa.mat, model = 'bym2', hyper= hyper.pc.space, scale.model = TRUE, adjust.for.con.comp = TRUE))
formula3ba <- update(formula1b, ~ . + f(space_id, graph = lsoa.mat, model = 'iid', hyper= hyper.pc))
formula3bb <- update(formula1b, ~ . + f(space_id, graph = lsoa.mat, model = 'bym2', hyper= hyper.pc.space, scale.model = TRUE, adjust.for.con.comp = TRUE))

## fits ----

# inla arguments
inla.mode <- c('classic', 'twostage', 'experimental')[1]
verbose <- FALSE
control.compute <- list(config = TRUE, dic = TRUE, waic = TRUE)
control.predictor <- list(compute = TRUE, link = 1)
control.inla <- list(strategy = 'adaptive', int.strategy = 'auto')
family <- 'gaussian'

### base ----

fit0 <- INLA::inla(formula0, 
                   family = family, 
                   data = data.final, 
                   control.compute = control.compute, 
                   control.predictor = control.predictor, 
                   control.inla = control.inla, 
                   inla.mode = inla.mode)

### base + time ----

fit1a <- INLA::inla(formula1a, 
                    family = family, 
                    data = data.final, 
                    control.compute = control.compute, 
                    control.predictor = control.predictor, 
                    control.inla = control.inla, 
                    inla.mode = inla.mode)
fit1b <- INLA::inla(formula1b, 
                    family = family, 
                    data = data.final, 
                    control.compute = control.compute, 
                    control.predictor = control.predictor, 
                    control.inla = control.inla, 
                    inla.mode = inla.mode)

### base + space ----

fit2a <- INLA::inla(formula2a, 
                    family = family, 
                    data = data.final, 
                    control.compute = control.compute, 
                    control.predictor = control.predictor, 
                    control.inla = control.inla, 
                    inla.mode = inla.mode)
fit2b <- INLA::inla(formula2b, 
                    family = family, 
                    data = data.final, 
                    control.compute = control.compute, 
                    control.predictor = control.predictor, 
                    control.inla = control.inla, 
                    inla.mode = inla.mode)

### base + time + space ----

fit3aa <- INLA::inla(formula3aa, 
                     family = family, 
                     data = data.final, 
                     control.compute = control.compute, 
                     control.predictor = control.predictor, 
                     control.inla = control.inla, 
                     inla.mode = inla.mode)
fit3ab <- INLA::inla(formula3ab, 
                     family = family, 
                     data = data.final, 
                     control.compute = control.compute, 
                     control.predictor = control.predictor, 
                     control.inla = control.inla, 
                     inla.mode = inla.mode)
fit3ba <- INLA::inla(formula3ba, 
                     family = family, 
                     data = data.final, 
                     control.compute = control.compute, 
                     control.predictor = control.predictor, 
                     control.inla = control.inla, 
                     inla.mode = inla.mode)
fit3bb <- INLA::inla(formula3bb, 
                     family = family, 
                     data = data.final, 
                     control.compute = control.compute, 
                     control.predictor = control.predictor, 
                     control.inla = control.inla, 
                     inla.mode = inla.mode)

## scores ----

score0 <- model.scores(results = fit0, true = data.final$y)
score1a <- model.scores(results = fit1a, true = data.final$y)
score1b <- model.scores(results = fit1b, true = data.final$y)
score2a <- model.scores(results = fit2a, true = data.final$y)
score2b <- model.scores(results = fit2b, true = data.final$y)
score3aa <- model.scores(results = fit3aa, true = data.final$y)
score3ab <- model.scores(results = fit3ab, true = data.final$y)
score3ba <- model.scores(results = fit3ba, true = data.final$y)
score3bb <- model.scores(results = fit3bb, true = data.final$y)

results <- 
  data.frame(model = c('fit0', 
                       paste0('fit1', c('a', 'b')), 
                       paste0('fit2', c('a', 'b')), 
                       paste0('fit3', c('aa', 'ab', 'ba', 'bb'))),
             timeModel = c('x', 'Unstructured', 'Structured', 'x', 'x', 'Unstructured', 'Unstructured', 'Structured', 'Structured'),
             spatialModel = c('x', 'x', 'x', 'Unstructured', 'Structured', 'Unstructured', 'Structured', 'Unstructured', 'Structured'),
             DIC = c(fit0$dic$dic, 
                     fit1a$dic$dic, fit1b$dic$dic,
                     fit2a$dic$dic, fit2b$dic$dic,
                     fit3aa$dic$dic, fit3ab$dic$dic, fit3ba$dic$dic, fit3bb$dic$dic),
             WAIC = c(fit0$waic$waic, 
                      fit1a$waic$waic, fit1b$waic$waic,
                      fit2a$waic$waic, fit2b$waic$waic,
                      fit3aa$waic$waic, fit3ab$waic$waic, fit3ba$waic$waic, fit3bb$waic$waic),
             IS = c(score0$intervalScore, 
                    score1a$intervalScore, score1b$intervalScore,
                    score2a$intervalScore, score2b$intervalScore,
                    score3aa$intervalScore, score3ab$intervalScore, score3ba$intervalScore, score3bb$intervalScore),
             width = c(score0$width, 
                       score1a$width, score1b$width,
                       score2a$width, score2b$width,
                       score3aa$width, score3ab$width, score3ba$width, score3bb$width),
             coverage = c(score0$coverage, 
                          score1a$coverage, score1b$coverage,
                          score2a$coverage, score2b$coverage,
                          score3aa$coverage, score3ab$coverage, score3ba$coverage, score3bb$coverage),
             mixingParameter = c(NA,
                                 NA, NA,
                                 NA, fit2b$summary.hyperpar['Phi for space_id', '0.5quant'],
                                 NA, fit3ab$summary.hyperpar['Phi for space_id', '0.5quant'], NA, fit3bb$summary.hyperpar['Phi for space_id', '0.5quant']),
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

results.subset <- 
  results %>% 
  dplyr::select(timeModel, spatialModel, mixingParameter, DIC, WAIC, IS, time)

### save ----

setwd(res.model.selection.dir)

save(fit0, 
     fit1a, fit1b,
     fit2a, fit2b,
     fit3aa, fit3ab, fit3ba, fit3bb, 
     file = 'modelSelection_modelFits.rds')

print(x = xtable::xtable(results.subset), 
      include.rownames = FALSE,
      file = 'modelSelectionTable.tex')
