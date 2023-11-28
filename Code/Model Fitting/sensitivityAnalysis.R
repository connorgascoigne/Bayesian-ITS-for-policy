# packages ----

library(tidyverse)
library(INLA)

# directories ----

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-3)], collapse = '/')
code.dir <- paste0(home.dir, '/Code')
data.dir <- paste0(home.dir, '/Data')
res.dir <- paste0(home.dir, '/Results')
data.organised.dir <- paste0(data.dir, '/Organised Data')
res.sensitivity.analysis.dir <- paste0(res.dir, '/Sensitivity Analysis')

# results folder
if(!dir.exists(paths = res.dir)) {
  dir.create(path = res.dir)
}

# model fit folder
if(!dir.exists(paths = res.sensitivity.analysis.dir)) {
  dir.create(path = res.sensitivity.analysis.dir)
}

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

setwd(data.dir)
stat.xplore.data <- readxl::read_excel(path = 'rawData/Stat-Xplore/statxploreDat.xlsx', skip = 9)

# define different datasets ----

percentage <- as.matrix(c(NA, 0.05, 0.15, 0.25, 0.35, 0.45))

contextual.awareness.data <- function(percentage, uc.data, survey.data){
  
  # percentage <- 0.25; uc.data <- stat.xplore.data; survey.data <- data.import
  # percentage <- NA; uc.data <- stat.xplore.data; survey.data <- data.import
  
  if(!is.na(percentage)){
    
    # oraganise the stat xplore data 
    uc.start.data <-
      uc.data %>%
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
      # filter all the regions great than x% of final value
      ## group within region
      dplyr::group_by(LAD22NM) %>%
      ## arrange each region into date order
      dplyr::arrange(LAD22NM, ucStartDate) %>%
      ## add a counter
      dplyr::mutate(element = 1:n()) %>%
      ## filter for the first time the total is greater than x% of the most recent value
      dplyr::filter(total >= total[max(element)] * percentage) %>%
      ## remove all but the first time this occurs
      dplyr::filter(ucStartDate == min(ucStartDate)) %>%
      dplyr::ungroup() %>%
      dplyr::select(LAD22NM, ucStartDate) %>%
      # !!!need to include the space ' /'!!!
      dplyr::mutate(LAD22NM = sub(' /.*', '', LAD22NM))
    
  } else {
    
    # oraganise the stat xplore data 
    uc.start.data <-
      uc.data %>%
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
      # filter the first date regions seen
      ## group within region
      dplyr::group_by(LAD22NM) %>%
      ## arrange each region into date order
      dplyr::arrange(LAD22NM, ucStartDate) %>%
      ## remove all but the first time this occurs
      dplyr::filter(ucStartDate == min(ucStartDate)) %>%
      dplyr::ungroup() %>%
      dplyr::select(LAD22NM, ucStartDate) %>%
      # !!!need to include the space ' /'!!!
      dplyr::mutate(LAD22NM = sub(" /.*", "", LAD22NM))
    
  }
  
  # make the weights
  adjusted.weight.data <-
    survey.data %>%
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
  data.start <- 
    data.import %>%
    # joining data sets
    ## rename
    dplyr::rename(c('id' = 'pidp', 'edu' = 'qfhigh', 'marStat' = 'mastat', 'jobStatus' = 'jbstat', 'ghq' = 'scghq2')) %>%
    ## uc start data
    dplyr::left_join(., uc.start.data, by = 'LAD22NM') %>% 
    ## adjusted weight 
    dplyr::left_join(., adjusted.weight.data %>% dplyr::select(pidp, inclusionProbability, adjustedWeight), by = c('id' = 'pidp')) %>%
    # data processing
    ## filtering
    dplyr::mutate(country = country %>% haven::as_factor()) %>% 
    dplyr::filter(country == 'England', age %in% 16:64, jobStatus %in% c(1:7,9:13,97), edu %in% c(1:16,96),
                  ethn %in% c(1:17,96), marStat %in% 1:10, sex %in% 1:2, ghq %in% 0:12, !is.na(LAD22NM), !is.na(interviewDate))
  
  data.final <- 
    data.start %>% 
    # select relevant columns 
    dplyr::select(
      # survey terms
      id, adjustedWeight,
      # spatial identifiers
      LSOA11CD,
      LAD22CD, LAD22NM, 
      # temporal terms
      ucStartDate, interviewDate, 
      # individual confounders
      jobStatus, age, edu, ethn, marStat, sex, 
      # community confounders
      DIVERSITY, DEPRIVATION, 
      # observations
      ghq) %>% 
    # formatting
    dplyr::mutate(individual = id %>% as.numeric(),
                  # weights
                  sampleWeight = dplyr::case_when(is.na(adjustedWeight) ~ 0,
                                                  TRUE ~ adjustedWeight),
                  # observation
                  # y = ghq %>% as.numeric(),
                  y = dplyr::case_when(ghq %in% 0:3 ~ 0, TRUE ~ 1) %>% as.numeric(),
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
                  time.exposed = dplyr::case_when(exposed == 'Exposed' ~ time, TRUE ~ 0),
                  treatment.exposed = dplyr::case_when(exposed == 'Exposed' ~ treatment, TRUE ~ 0),
                  timeSinceTreatment.exposed = dplyr::case_when(exposed == 'Exposed' ~ timeSinceTreatment, TRUE ~ 0),
                  # _id terms for inla
                  ## confounders (as factors)
                  age_id = age %>% as.numeric() %>% as.factor(),
                  edu_id = edu %>% as.numeric() %>% as.factor(),
                  ethn_id = ethn %>% as.numeric() %>% as.factor(),
                  marStat_id = marStat %>% as.numeric() %>% as.factor(),
                  sex_id = sex %>% as.numeric() %>% as.factor(),
                  ## ITS terms
                  ### control
                  time_id = time,
                  treatment_id = treatment,
                  timeSinceTreatment_id = timeSinceTreatment,
                  ### exposed
                  exposed_id = exposed %>% as.numeric() %>% as.factor(),
                  time.exposed_id = dplyr::case_when(exposed == 'Exposed' ~ time_id, TRUE ~ 0),
                  treatment.exposed_id = treatment.exposed,
                  timeSinceTreatment.exposed_id = timeSinceTreatment.exposed,
                  ## community level covariates (as factors)
                  diversity_id = DIVERSITY,
                  deprivation_id = DEPRIVATION,
                  ## random effects
                  individual_id = id %>% as.factor() %>% as.numeric(), 
                  year_id = year %>% as.factor() %>% as.numeric(),
                  time.month_id = time %>% as.factor() %>% as.numeric(),
                  space_id = LAD22CD %>% as.factor() %>% as.numeric(),
                  error_id = 1:n())
  
  return(data.final)
  
}

all.datas <- apply(X = percentage, 1, FUN = contextual.awareness.data, uc.data = stat.xplore.data, survey.data = data.import)

# ITS model fits ----

## inla args ----

its.formula <- 
  paste('y ~ ', 
        paste(paste0(c('time', 'treatment', 'timeSinceTreatment', 
                       'exposed', 'time.exposed', 'treatment.exposed', 'timeSinceTreatment.exposed',
                       'age', 'edu', 'ethn', 'marStat', 'sex', 'deprivation', 'diversity'), '_id'), 
              collapse = ' + '), sep = ' ') %>% 
  as.formula()
# hyperparameters
pc.u <- 1; pc.alpha <- 0.01; pc.u.phi <- 0.5; pc.alpha.phi <- 2/3
# pc hyper priors
hyper.pc <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)))
hyper.pc.space <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)), 
                       phi = list(prior = 'pc', param = c(pc.u.phi, pc.alpha.phi)))

# add random effects
its.formula <- update(its.formula, ~ . + f(time.month_id,
                                           model = 'rw1',
                                           hyper= hyper.pc))
its.formula <- update(its.formula, ~ . + f(space_id,
                                           graph = lad.mat,
                                           model = 'bym2',
                                           hyper= hyper.pc.space,
                                           scale.model = TRUE,
                                           adjust.for.con.comp = TRUE))

# inla arguments
inla.mode <- c('classic', 'twostage', 'experimental')[1]
verbose <- FALSE
control.compute <- list(config = TRUE)
control.predictor <- list(compute = TRUE, link = 1)
control.inla <- list(strategy = 'adaptive', int.strategy = 'auto')
family <- 'binomial'

## different models ----

fit.00 <-
  INLA::inla(its.formula, 
             family = family, 
             data = all.datas[[1]],
             weights = all.datas[[1]]$sampleWeight,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

fit.05 <-
  INLA::inla(its.formula, 
             family = family, 
             data = all.datas[[2]],
             weights = all.datas[[2]]$sampleWeight,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

fit.15 <-
  INLA::inla(its.formula,
             family = family, 
             data = all.datas[[3]],
             weights = all.datas[[3]]$sampleWeight,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

fit.25 <-
  INLA::inla(its.formula,
             family = family, 
             data = all.datas[[4]],
             weights = all.datas[[4]]$sampleWeight,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

fit.35 <-
  INLA::inla(its.formula,
             family = family, 
             data = all.datas[[5]],
             weights = all.datas[[5]]$sampleWeight,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

fit.45 <-
  INLA::inla(its.formula,
             family = family, 
             data = all.datas[[6]],
             weights = all.datas[[6]]$sampleWeight,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

## extract posterior ----

extractSamples <- function(data, fit, n.sims, ...){
  
  # data = all.datas[[2]]
  # fit = fit.05
  # n.sims = 10
  
  set.seed(11)
  
  ### marginals from INLA ----
  
  samp.all <- INLA::inla.posterior.sample(n = n.sims, result = fit, intern = TRUE, ...) # <--- remove selection from havard
  
  ### extract samples ----
  
  # linear predictor
  theta.predictor.a <-
    lapply(X = samp.all,
           FUN = function(x) {x$latent[startsWith(rownames(x$latent), 'Predictor:')]}) %>%
    unlist() %>%
    matrix(., ncol = n.sims)
  # all model parameters
  theta.parameter.a <-
    lapply(X = samp.all,
           FUN = function(x) {x$latent[!startsWith(rownames(x$latent), 'Predictor:')]}) %>%
    unlist() %>%
    matrix(., ncol = n.sims)
  
  ### organise ----
  
  # linear predictor
  colnames(theta.predictor.a) <- paste0('theta:', 1:n.sims)
  theta.predictor <- 
    cbind(data %>% 
            dplyr::rename(diversity = DIVERSITY,
                          deprivation = DEPRIVATION) %>% 
            dplyr::select(LAD22CD, LAD22NM,
                          ucStartDate, interviewDate, time, treatment, exposed, 
                          age, edu, ethn, marStat, sex, diversity, deprivation), 
          theta.predictor.a) %>% 
    arrange(time, treatment, LAD22CD, exposed)
  
  # all model predictor
  colnames(theta.parameter.a) <- paste0('theta:', 1:n.sims)
  theta.parameter <- 
    data.frame(parameter = rownames(samp.all[[1]]$latent)[!startsWith(rownames(samp.all[[1]]$latent), 'Predictor:')]) %>% 
    cbind(., theta.parameter.a)
  
  # casual parameters
  theta.causal <-
    theta.parameter %>% 
    dplyr::filter(grepl(pattern = 'exposed', x = parameter))
  
  return(list(theta.predictor = theta.predictor,
              theta.parameter = theta.parameter,
              theta.causal = theta.causal))
  
}

theta.predictor.00 <- extractSamples(data = all.datas[[1]], fit = fit.00, n.sims = 1000)
theta.predictor.05 <- extractSamples(data = all.datas[[2]], fit = fit.05, n.sims = 1000)
theta.predictor.15 <- extractSamples(data = all.datas[[3]], fit = fit.15, n.sims = 1000)
theta.predictor.25 <- extractSamples(data = all.datas[[4]], fit = fit.25, n.sims = 1000)
theta.predictor.35 <- extractSamples(data = all.datas[[5]], fit = fit.35, n.sims = 1000)
theta.predictor.45 <- extractSamples(data = all.datas[[6]], fit = fit.45, n.sims = 1000)

# results ----

setwd(res.sensitivity.analysis.dir)

## fixed effects -----

### table ----

model.parameter.table <- 
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
        rep('Diversity relative to [1]', times = 3),
        rep('Random effect', times = 3)),
    parameter = 
      c('Intercept', 'Time', 'Intervention', 'Time+',
        'Intercept', 'Time', 'Intervention', 'Time+',
        '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)',
        '[Below GCSE and other]', '[GCSE, A-level or equivalent]',
        '[Asian]', '[Black]', '[Mixed]', '[Other]',
        '[Unmarried]',
        '[Female]',
        '[2]', '[3]', '[4]', '[5]', '[6]', '[7]', '[8]', '[9]', '[10]',
        '[2]', '[3]', '[4]',
        'Temporal precision', 'Spatial precision', 'Spatial mixing'),
    dataIntroduction = c(paste0(fit.00$summary.fixed[,4] %>% round(., digits = 4), ' (', fit.00$summary.fixed[,3] %>% round(., digits = 4), ', ', fit.00$summary.fixed[,5] %>% round(., digits = 4), ')'),
                         paste0(fit.00$summary.hyperpar[,4] %>% round(., digits = 4), ' (', fit.00$summary.hyperpar[,3] %>% round(., digits = 4), ', ', fit.00$summary.hyperpar[,5] %>% round(., digits = 4), ')')),
    # data05pct = c(paste0(fit.05$summary.fixed[,4] %>% round(., digits = 4), ' (', fit.05$summary.fixed[,3] %>% round(., digits = 4), ', ', fit.05$summary.fixed[,5] %>% round(., digits = 4), ')'),
    #               paste0(fit.05$summary.hyperpar[,4] %>% round(., digits = 4), ' (', fit.05$summary.hyperpar[,3] %>% round(., digits = 4), ', ', fit.05$summary.hyperpar[,5] %>% round(., digits = 4), ')')),
    # data15pct = c(paste0(fit.15$summary.fixed[,4] %>% round(., digits = 4), ' (', fit.15$summary.fixed[,3] %>% round(., digits = 4), ', ', fit.15$summary.fixed[,5] %>% round(., digits = 4), ')'),
    #               paste0(fit.15$summary.hyperpar[,4] %>% round(., digits = 4), ' (', fit.15$summary.hyperpar[,3] %>% round(., digits = 4), ', ', fit.15$summary.hyperpar[,5] %>% round(., digits = 4), ')')),
    data25pct = c(paste0(fit.25$summary.fixed[,4] %>% round(., digits = 4), ' (', fit.25$summary.fixed[,3] %>% round(., digits = 4), ', ', fit.25$summary.fixed[,5] %>% round(., digits = 4), ')'),
                  paste0(fit.25$summary.hyperpar[,4] %>% round(., digits = 4), ' (', fit.25$summary.hyperpar[,3] %>% round(., digits = 4), ', ', fit.25$summary.hyperpar[,5] %>% round(., digits = 4), ')')))
# data35pct = c(paste0(fit.35$summary.fixed[,4] %>% round(., digits = 4), ' (', fit.35$summary.fixed[,3] %>% round(., digits = 4), ', ', fit.35$summary.fixed[,5] %>% round(., digits = 4), ')'),
#               paste0(fit.35$summary.hyperpar[,4] %>% round(., digits = 4), ' (', fit.35$summary.hyperpar[,3] %>% round(., digits = 4), ', ', fit.35$summary.hyperpar[,5] %>% round(., digits = 4), ')')),
# data45pct = c(paste0(fit.45$summary.fixed[,4] %>% round(., digits = 4), ' (', fit.45$summary.fixed[,3] %>% round(., digits = 4), ', ', fit.45$summary.fixed[,5] %>% round(., digits = 4), ')'),
#               paste0(fit.45$summary.hyperpar[,4] %>% round(., digits = 4), ' (', fit.45$summary.hyperpar[,3] %>% round(., digits = 4), ', ', fit.45$summary.hyperpar[,5] %>% round(., digits = 4), ')')))

### plot ----

model.parameters.data <- 
  data.frame(parameter = 
               c('Intercept', 'Time', 'Intervention', 'Time+',
                 'Intercept', 'Time', 'Intervention', 'Time+',
                 '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)',
                 '[Below GCSE and other]', '[GCSE, A-level or equivalent]',
                 '[Asian]', '[Black]', '[Mixed]', '[Other]',
                 '[Unmarried]',
                 '[Female]',
                 '[2]', '[3]', '[4]', '[5]', '[6]', '[7]', '[8]', '[9]', '[10]',
                 '[2]', '[3]', '[4]') %>% 
               rep(., times = 2) %>% 
               # rep(., times = 6) %>% 
               factor(., levels = c('Intercept', 'Time', 'Intervention', 'Time+',
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
                       rep('Diversity relative to [1]', times = 3)) %>%
               rep(., times = 2) %>% 
               # rep(., times = 6) %>% 
               factor(., 
                      levels = c('Control ITS terms',
                                 'Exposed ITS terms',
                                 'Relative to [16, 25)',
                                 'Relative to [Degree or higher]',
                                 'Relative to [White]',
                                 'Relative to [Married or civil partnership]',
                                 'Relative to [Male]',
                                 'Deprivation relative to [1]',
                                 'Diversity relative to [1]')),
             data = c(rep('Introduction', times = 32), 
                      # rep('5% Data', times = 32), 
                      # rep('15% Data', times = 32), 
                      rep('Contextual Awareness', times = 32)
                      # rep('35% Data', times = 32), 
                      # rep('45% Data', times = 32)
             ) %>% 
               factor(., levels = c('Introduction', 
                                    # '5% Data', 
                                    # '15% Data', 
                                    'Contextual Awareness' 
                                    # '35% Data', 
                                    # '45% Data'
               )),
             lower = c(fit.00$summary.fixed[,3],
                       # fit.05$summary.fixed[,3], 
                       # fit.15$summary.fixed[,3], 
                       fit.25$summary.fixed[,3]
                       # fit.35$summary.fixed[,3], 
                       # fit.45$summary.fixed[,3]
             ),
             median = c(fit.00$summary.fixed[,4], 
                        # fit.05$summary.fixed[,4], 
                        # fit.15$summary.fixed[,4], 
                        fit.25$summary.fixed[,4]
                        # fit.35$summary.fixed[,4], 
                        # fit.45$summary.fixed[,4]
             ),
             upper = c(fit.00$summary.fixed[,5],
                       # fit.05$summary.fixed[,5], 
                       # fit.15$summary.fixed[,5], 
                       fit.25$summary.fixed[,5]
                       # fit.35$summary.fixed[,5], 
                       # fit.45$summary.fixed[,5]
             )) %>% 
  dplyr::mutate(value = dplyr::case_when(lower < 0 & upper < 0 ~ 'Negative',
                                         lower < 0 & 0 < upper ~ 'Random Noise',
                                         0 < lower & 0 < upper ~ 'Positive',
                                         TRUE ~ 'Error') %>% factor(., levels = c('Negative', 'Random Noise', 'Positive', 'Error')))


model.parameters.plot <-
  ggplot2::ggplot(model.parameters.data, 
                  aes(x = parameter, y = median, group = interaction(data, parameter), colour = data)) +
  ggplot2::geom_hline(yintercept = 0, colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point(position = position_dodge(width = 0.5)) +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), width = .05, position = position_dodge(width = 0.5)) +
  ggplot2::scale_colour_manual(values = c('red3', 'green4', 'orange2', 'blue3', 'purple3', 'pink2')) + 
  ggplot2::labs(x = '', y = '') +
  ggplot2::facet_wrap(~ group, scales = 'free') +
  my.theme(legend.title = element_blank(),
           text = element_text(size = text.size),
           legend.position = 'top'); model.parameters.plot

## centered time ----

cenetered.time.data <-
  dplyr::bind_rows(theta.predictor.00$theta.predictor %>% dplyr::mutate(dataType = 'Introduction'),
                   # theta.predictor.05$theta.predictor %>% dplyr::mutate(dataType = '5% Data'),
                   # theta.predictor.15$theta.predictor %>% dplyr::mutate(dataType = '15% Data'),
                   theta.predictor.25$theta.predictor %>% dplyr::mutate(dataType = 'Contextual Awareness')
                   # theta.predictor.35$theta.predictor %>% dplyr::mutate(dataType = '35% Data'),
                   # theta.predictor.45$theta.predictor %>% dplyr::mutate(dataType = '45% Data')
  ) %>% 
  dplyr::mutate(dataType =  
                  dataType %>% 
                  factor(., 
                         levels = c('Introduction', 
                                    # '5% Data', 
                                    # '15% Data', 
                                    'Contextual Awareness'
                                    # '35% Data', 
                                    # '45% Data'
                         )),
                time = (lubridate::interval(start = ucStartDate, end = interviewDate) %/% months(12))) %>% 
  # dplyr::filter(time %in% -3:3) %>%
  # dplyr::mutate(time = time %>% factor(., labels = c(-3:-1, 'Start', 1:3))) %>%
  dplyr::mutate(time = time %>% factor(., labels = c(-10:-1, 'Start', 1:8))) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'time', 'dataType')) %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  expit() %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:'))

cenetered.time.plot <-
  ggplot2::ggplot(data = cenetered.time.data, aes(x = time, y = median,
                                                  group = interaction(exposed, dataType),
                                                  colour = exposed,
                                                  linetype = dataType,
                                                  shape = dataType,
                                                  fill = exposed)) +
  ggplot2::geom_vline(xintercept = 'Start', colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, colour = NA) +
  ggplot2::scale_colour_manual(values = c('blue3', 'red3')) +
  ggplot2::scale_fill_manual(values = c('blue3', 'red3')) +
  ggplot2::scale_linetype_manual(values= c('dashed', 'solid')) +
  ggplot2::scale_shape_manual(values= c('square', 'circle')) +
  # ggplot2::scale_linetype_manual(values= c('dotdash', 'dashed', 'dotted', 'solid', 'longdash', 'twodash')) +
  # ggplot2::scale_shape_manual(values= c('diamond', 'square', 'triangle', 'circle', 'plus', 'asterisk')) +
  ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  ggplot2::labs(x = 'Time since awareness to Universal Credit (years)', y = 'Psychological distress prevalence (%)') +
  my.theme(legend.title = element_blank(),
           legend.position = 'bottom',
           text = element_text(size = text.size)); cenetered.time.plot

## causal parameter odds ratio ----

causal.data <-
  dplyr::bind_rows(theta.predictor.00$theta.causal %>% dplyr::mutate(dataType = 'Introduction'),
                   # theta.predictor.05$theta.causal %>% dplyr::mutate(dataType = '5% Data'),
                   # theta.predictor.15$theta.causal %>% dplyr::mutate(dataType = '15% Data'),
                   theta.predictor.25$theta.causal %>% dplyr::mutate(dataType = 'Contextual Awareness')
                   # theta.predictor.35$theta.causal %>% dplyr::mutate(dataType = '35% Data'),
                   # theta.predictor.45$theta.causal %>% dplyr::mutate(dataType = '45% Data')
  ) %>% 
  dplyr::mutate(dataType =  
                  dataType %>% 
                  factor(., 
                         levels = c('Introduction', 
                                    # '5% Data', 
                                    # '15% Data', 
                                    'Contextual Awareness'
                                    # '35% Data', 
                                    # '45% Data'
                         ),
                         labels = c('Intro.', 
                                    # '5%', 
                                    # '15%', 
                                    'Cont. Awar.'
                                    # '35%', 
                                    # '45%'
                         )),
                parameterLabel = 
                  parameter %>% 
                  factor(., 
                         levels = paste0(c('exposed_id2', 'time.exposed_id', 'treatment.exposed_id', 'timeSinceTreatment.exposed_id'), ':1'),
                         labels = c(latex2exp::TeX('Baseline ($\\beta_4$)'),
                                    latex2exp::TeX('Time ($\\beta_5$)'),
                                    latex2exp::TeX('Intervention ($\\beta_6$)'),
                                    latex2exp::TeX('Time+ ($\\beta_7$)')))) %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  exp() %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:'))

casual.plot <-
  ggplot2::ggplot(data = causal.data, aes(x = dataType, y = median, group = dataType)) +
  ggplot2::geom_hline(yintercept = 1, linetype = 'dashed', colour = 'red3') +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), width = .05) +
  ggplot2::labs(x = '', y = 'Odds Ratio') +
  ggplot2::facet_wrap(~ parameterLabel, scales = 'free', labeller = label_parsed) +
  my.theme(legend.title = element_blank(),
           legend.position = 'bottom',
           text = element_text(size = text.size)); casual.plot

## save ----

setwd(res.sensitivity.analysis.dir)

print(x = xtable::xtable(model.parameter.table, digits = 4), 
      include.rownames = FALSE,
      file = 'modelParameterTable.tex')

ggplot2::ggsave(filename = 'sensitivityAnalysis_modelParameters.png',
                plot = model.parameters.plot,
                height = height, width = 2.5*width)

ggplot2::ggsave(filename = 'sensitivityAnalysis_centeredTime.png',
                plot = cenetered.time.plot,
                height = height, width = width)

ggplot2::ggsave(filename = 'sensitivityAnalysis_casualOddsRatio.png',
                plot = casual.plot,
                height = height, width = width)

save(fit.00, fit.05, fit.15, fit.25, fit.35, fit.45, 
     file = 'sensitivityAnalysis_modelFits.rds')

save(theta.predictor.00, theta.predictor.05, theta.predictor.15, 
     theta.predictor.25, theta.predictor.35, theta.predictor.45, 
     file = 'sensitivityAnalysis_posteriorSamples.rds')
