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
                time = (lubridate::interval(start = ucStartDate, end = interviewDate) %/% months(1)),
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
                space_id = LSOA11CD %>% as.factor() %>% as.numeric(),
                error_id = 1:n())

# model fit ----

# hyperparameters
pc.u <- 1; pc.alpha <- 0.01; pc.u.phi <- 0.5; pc.alpha.phi <- 2/3
# pc hyper priors
hyper.pc <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)))
hyper.pc.space <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)), 
                       phi = list(prior = 'pc', param = c(pc.u.phi, pc.alpha.phi)))

its.formula <- 
  paste('y ~ ', 
        paste(paste0(c('time', 'treatment', 'timeSinceTreatment', 
                       'exposed', 'time.exposed', 'treatment.exposed', 'timeSinceTreatment.exposed',
                       'age', 'edu', 'ethn', 'rela', 'sex', 'deprivation', 'diversity'), '_id'), 
              collapse = ' + '), sep = ' ') %>%
  as.formula() %>% 
  update(., ~ . + f(strata_id, model = 'iid', hyper= hyper.pc) + f(psu_id, model = 'iid', hyper= hyper.pc))

# add random effects
its.formula <- update(its.formula, ~ . + f(time.month_id,
                                           model = 'rw1',
                                           hyper= hyper.pc))
its.formula <- update(its.formula, ~ . + f(space_id,
                                           graph = lsoa.mat,
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
family <- 'gaussian'

# model fit
fit <-
  INLA::inla(its.formula, 
             family = family, 
             data = data.final,
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
                       # FUN = function(x) {rbinom(prob = expit(x), n = 1, size = 1)}
                       FUN = function(x) {rnorm(mean = x, sd = 1, n = 1)}
    ) %>%
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
          FUN = function(x) {rnorm(mean = x, sd = 1, n = 1)}
    ) %>%
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
          dplyr::select(LSOA11CD, LAD22CD, LAD22NM,
                        ucStartDate, interviewDate, time, treatment, exposed, 
                        age, edu, ethn, rela, sex, diversity, deprivation), 
        theta.predictor.a) %>% 
  arrange(time, treatment, LSOA11CD, LAD22CD, LAD22NM, exposed)
# observations
colnames(theta.observation.a) <- paste0('theta:', 1:n.sims)
theta.observation <- 
  cbind(data.final %>%  
          dplyr::rename(diversity = DIVERSITY,
                        deprivation = DEPRIVATION) %>% 
          dplyr::select(LSOA11CD, LAD22CD, LAD22NM,
                        ucStartDate, interviewDate, time, treatment, exposed, 
                        age, edu, ethn, rela, sex, diversity, deprivation), 
        theta.observation.a) %>% 
  arrange(time, treatment, LSOA11CD, LAD22CD, LAD22NM, exposed)
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
# theta.space <-
#   cbind(lad.names %>%
#           dplyr::filter(LAD22NM != 'Isles of Scilly'),
#         # we dont have isle of scilly data
#         theta.parameter %>%
#           dplyr::filter(level %in% paste0('space_id:', 1:(nrow(lad.names))-1)))
theta.space <-
  cbind(lsoa.names,
        theta.parameter %>%
          dplyr::filter(level %in% paste0('space_id:', 1:(nrow(lsoa.names)))))
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
