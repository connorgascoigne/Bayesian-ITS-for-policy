# packages ----

library(tidyverse)
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
data.organised.dir <- paste0(data.dir, '/Organised Data')
res.model.fit.dir <- paste0(res.dir, '/Model Fit')

# results folder
if(!dir.exists(paths = res.dir)) {
  dir.create(path = res.dir)
}

# model fit folder
if(!dir.exists(paths = res.model.fit.dir)) {
  dir.create(path = res.model.fit.dir)
}

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


### adjusted weights ----

adjusted.weight.data <-
  data.import %>%
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

### data final ----

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

# model fit ----

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
# family <- 'gaussian'
family <- 'binomial'

# weights
weights.final <- data.final$sampleWeight

# model fit
fit <-
  INLA::inla(its.formula, 
             family = family, 
             data = data.final,
             weights = weights.final,
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

# prevalence predictions ----

set.seed(11)

## marginals from INLA ----

n.sims <- 1000
samp.all <- INLA::inla.posterior.sample(n = n.sims, result = fit, intern = TRUE, verbose = verbose)

## extract draws from samples ----

use.cluster <- FALSE
if(use.cluster){
  ### set up cluster ----
  
  # set up cluster 
  number.cores <- 10
  my.cluster <- parallel::makeCluster(number.cores, type = 'PSOCK')
  
  # extract packages on parallel cluster
  parallel::clusterEvalQ(cl = my.cluster, {library(tidyverse)})
  
  # extract R objects on parallel environment
  parallel::clusterExport(cl = my.cluster, c('samp.all', 'expit'))
  
  ### extract different sets of draws ----
  
  # linear predictor
  theta.predictor.a <-
    parallel::parLapply(cl = my.cluster,
                        X = samp.all,
                        fun = function(x) {x$latent[startsWith(rownames(x$latent), 'Predictor:')]}) %>%
    unlist() %>%
    matrix(., ncol = n.sims)
  # observations
  theta.observation.a <-
    theta.predictor.a %>%
    parallel::parApply(cl = my.cluster,
                       X = .,
                       MARGIN = 2,
                       FUN = function(x) {rbinom(prob = expit(x), n = 1, size = 1)}) %>%
    unlist() %>%
    matrix(., ncol = n.sims)
  # parameters
  theta.parameter.a <-
    parallel::parLapply(cl = my.cluster,
                        X = samp.all, 
                        fun = function(x) {x$latent[!startsWith(rownames(x$latent), 'Predictor:')]}) %>%
    unlist() %>%
    matrix(., ncol = n.sims)
  
  ### close cluster ----
  
  # close parallel environment
  parallel::stopCluster(my.cluster)
  
} else {
  ### extract different sets of draws ----
  
  # linear predictor
  theta.predictor.a <-
    lapply(X = samp.all,
           FUN = function(x) {x$latent[startsWith(rownames(x$latent), 'Predictor:')]}) %>%
    unlist() %>%
    matrix(., ncol = n.sims)
  # observations
  theta.observation.a <-
    theta.predictor.a %>%
    apply(X = .,
          MARGIN = 2,
          FUN = function(x) {rbinom(prob = expit(x), n = 1, size = 1)}) %>%
    unlist() %>%
    matrix(., ncol = n.sims)
  # parameters
  theta.parameter.a <-
    lapply(X = samp.all, 
           FUN = function(x) {x$latent[!startsWith(rownames(x$latent), 'Predictor:')]}) %>%
    unlist() %>%
    matrix(., ncol = n.sims)
  
}
## organise -----

colnames(theta.predictor.a) <- paste0('theta:', 1:n.sims)
theta.predictor <- 
  cbind(data.final %>% 
          dplyr::rename(diversity = DIVERSITY,
                        deprivation = DEPRIVATION) %>% 
          dplyr::select(LAD22CD, LAD22NM,
                        ucStartDate, interviewDate, time, treatment, exposed, 
                        age, edu, ethn, marStat, sex, diversity, deprivation), 
        theta.predictor.a) %>% 
  arrange(time, treatment, LAD22CD, LAD22NM, exposed)
# observations
colnames(theta.observation.a) <- paste0('theta:', 1:n.sims)
theta.observation <- 
  cbind(data.final %>%  
          dplyr::rename(diversity = DIVERSITY,
                        deprivation = DEPRIVATION) %>% 
          dplyr::select(LAD22CD, LAD22NM,
                        ucStartDate, interviewDate, time, treatment, exposed, 
                        age, edu, ethn, marStat, sex, diversity, deprivation), 
        theta.observation.a) %>% 
  arrange(time, treatment, LAD22CD, LAD22NM, exposed)
# parameters
colnames(theta.parameter.a) <- paste0('theta:', 1:n.sims)
theta.parameter <- 
  cbind(samp.all[[1]]$latent %>%
          as.data.frame() %>% 
          tibble::rownames_to_column(., var = 'level') %>% 
          dplyr::filter(!startsWith(level, 'Predictor')) %>% 
          dplyr::select(level),
        theta.parameter.a)
# spatial random effect
theta.space <-
  cbind(lad.names %>%
          dplyr::filter(LAD22NM != 'Isles of Scilly'),
        # we dont have isle of scilly data
        theta.parameter %>%
          dplyr::filter(level %in% paste0('space_id:', 1:(nrow(lad.names))-1)))
# # temporal random effect
# theta.time <- 
#   cbind(data.final %>% 
#           dplyr::select(year, year_id) %>% 
#           dplyr::distinct() %>% 
#           arrange(year_id), 
#         theta.parameter %>%
#           dplyr::filter(level %in% paste0('year_id:', 1:length(unique(data.final$year)))))
# temporal random effect
theta.time <- 
  cbind(data.final %>% 
          dplyr::select(time, time.month_id) %>% 
          dplyr::distinct() %>% 
          arrange(time.month_id), 
        theta.parameter %>%
          dplyr::filter(level %in% paste0('time.month_id:', 1:length(unique(data.final$time)))))

# save results ----

setwd(res.model.fit.dir)

saveRDS(data.final, file = 'modelData.rds')
saveRDS(fit, file = 'modelFit.rds')
saveRDS(theta.predictor, file = 'linearPredictorSamples.rds')
saveRDS(theta.observation, file = 'observationSamples.rds')
saveRDS(theta.parameter, file = 'parameterSamples.rds')
saveRDS(theta.space, file = 'spatialRandomEffectSamples.rds')
saveRDS(theta.time, file = 'temporalRandomEffectSamples.rds')
