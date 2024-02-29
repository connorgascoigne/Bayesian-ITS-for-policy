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

percentage <- as.matrix(c(NA, 0.25))

createData <- function(percentage, uc.data, survey.data){
  
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
  
  # initially creating the data
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
                  # sampliong design
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
  
  return(data.final)
  
}

all.datas <- apply(X = percentage, 1, FUN = createData, uc.data = stat.xplore.data, survey.data = data.import)

# ITS model fits ----

## hyperparameters ----

pc.u <- 1; pc.alpha <- 0.01; pc.u.phi <- 0.5; pc.alpha.phi <- 2/3
# pc hyper priors
hyper.pc <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)))
hyper.pc.space <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)), 
                       phi = list(prior = 'pc', param = c(pc.u.phi, pc.alpha.phi)))

## formula ----

its.formula <- 
  paste('y ~ ', 
        paste(paste0(c('time', 'treatment', 'timeSinceTreatment', 
                       'exposed', 'time.exposed', 'treatment.exposed', 'timeSinceTreatment.exposed',
                       'age', 'edu', 'ethn', 'rela', 'sex', 'deprivation', 'diversity'), '_id'), 
              collapse = ' + '), sep = ' ') %>%
  as.formula() %>% 
  update(., ~ . 
         + f(strata_id, model = 'iid', hyper= hyper.pc) 
         + f(psu_id, model = 'iid', hyper= hyper.pc)
         + f(time.month_id, model = 'rw1', hyper= hyper.pc)
         + f(space_id, graph = lsoa.mat, model = 'bym2', hyper= hyper.pc.space, scale.model = TRUE, adjust.for.con.comp = TRUE))

## inla arguments ----

inla.mode <- c('classic', 'twostage', 'experimental')[1]
verbose <- FALSE
control.compute <- list(config = TRUE)
control.predictor <- list(compute = TRUE, link = 1)
control.inla <- list(strategy = 'adaptive', int.strategy = 'auto')
family <- 'gaussian'

## different models ----

fit.intro <-
  INLA::inla(its.formula, 
             family = family, 
             data = all.datas[[1]],
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

fit.contAwar <-
  INLA::inla(its.formula,
             family = family, 
             data = all.datas[[2]],
             control.compute = control.compute,
             control.predictor = control.predictor,
             control.inla = control.inla,
             inla.mode = inla.mode,
             verbose = verbose)

## extract posterior ----

theta.predictor.intro <- extractSamples(data = all.datas[[1]], fit = fit.intro, n.sims = 1000)
theta.predictor.contAwar <- extractSamples(data = all.datas[[2]], fit = fit.contAwar, n.sims = 1000)

# results ----

setwd(res.sensitivity.analysis.dir)

## fixed effects -----

### table ----

all.parameter.data <- 
  data.frame(
    group = 
      c(rep('Control ITS terms', times = 4),
        rep('Difference ITS terms', times = 4),
        rep('Relative to [16, 25)', times = 4),
        rep('Relative to Below GCSE and other', times = 2),
        rep('Relative to White', times = 4),
        rep('Relative to Single', times = 1),
        rep('Relative to Male', times = 1),
        rep('Deprivation relative to 10 (Least Deprived)', times = 9),
        rep('Diversity relative to 1 (Least Diverse)', times = 3),
        rep('Random effect', times = 6)),
    parameter = 
      c('Intercept', 'Time', 'Intervention', 'Time+',
        'Intercept', 'Time', 'Intervention', 'Time+',
        '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)',
        'GCSE, A-level or equivalent', 'Degree or higher',
        'Asian', 'Black', 'Mixed', 'Other',
        'Relationship',
        'Female',
        '1 (Most Deprived)', '2', '3', '4', '5', '6', '7', '8', '9',
        '2', '3', '4 (Most Diverse)',
        'Gaussian precision', 'Strata precision', 'Cluster precision', 'Temporal precision', 'Spatial precision', 'Spatial mixing'),
    dataIntro = c(paste0(fit.intro$summary.fixed[,4] %>% round(., digits = 4), ' (', 
                         fit.intro$summary.fixed[,3] %>% round(., digits = 4), ', ', 
                         fit.intro$summary.fixed[,5] %>% round(., digits = 4), ')'),
                  paste0(fit.intro$summary.hyperpar[,4] %>% round(., digits = 4), ' (', 
                         fit.intro$summary.hyperpar[,3] %>% round(., digits = 4), ', ', 
                         fit.intro$summary.hyperpar[,5] %>% round(., digits = 4), ')')),
    dataContAwar = c(paste0(fit.contAwar$summary.fixed[,4] %>% round(., digits = 4), ' (', 
                            fit.contAwar$summary.fixed[,3] %>% round(., digits = 4), ', ', 
                            fit.contAwar$summary.fixed[,5] %>% round(., digits = 4), ')'),
                     paste0(fit.contAwar$summary.hyperpar[,4] %>% round(., digits = 4), ' (', 
                            fit.contAwar$summary.hyperpar[,3] %>% round(., digits = 4), ', ', 
                            fit.contAwar$summary.hyperpar[,5] %>% round(., digits = 4), ')')))

### plot ----

fixed.parameter.data <- 
  dplyr::bind_rows(theta.predictor.intro$theta.parameter %>% 
                     dplyr::filter(!startsWith(parameter, 'strata_id'),
                                   !startsWith(parameter, 'psu_id'),
                                   !startsWith(parameter, 'time.month_id'),
                                   !startsWith(parameter, 'space_id')) %>% 
                     dplyr::mutate(data = 'Introduction'),
                   theta.predictor.contAwar$theta.parameter %>% 
                     dplyr::filter(!startsWith(parameter, 'strata_id'),
                                   !startsWith(parameter, 'psu_id'),
                                   !startsWith(parameter, 'time.month_id'),
                                   !startsWith(parameter, 'space_id')) %>% 
                     dplyr::mutate(data = 'Contextual Awareness')) %>%  
  # relative risk and then summarise
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                data = data %>% factor(., levels = c('Introduction', 'Contextual Awareness'))) %>%
  dplyr::select(-starts_with('theta:')) %>% 
  cbind(., 
        covariateLevel = 
          rep(c('Intercept', 'Time', 'Intervention', 'Time+',
                'Intercept', 'Time', 'Intervention', 'Time+',
                '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)',
                'GCSE, A-level or equivalent', 'Degree or higher',
                'Asian', 'Black', 'Mixed', 'Other',
                'Relationship',
                'Female',
                '1 (Most Deprived)', '2', '3', '4', '5', '6', '7', '8', '9',
                '2', '3', '4 (Most Diverse)'),
              times = 2),
        covariate = 
          rep(c(rep('Control ITS terms', times = 4),
                rep('Difference ITS terms', times = 4),
                rep('Age relative to [16, 25)', times = 4),
                rep('Education relative to Below GCSE and other', times = 2),
                rep('Ethnicity relative to White', times = 4),
                rep('Relationship relative to Single', times = 1),
                rep('Sex relative to Male', times = 1),
                rep('Deprivation relative to 10 (Least Deprived)', times = 9),
                rep('Diversity relative to 1 (Least Diverse)', times = 3)),
              times = 2)) %>% 
  dplyr::mutate(index = rep(1:(n()/2), times = 2))

fixed.parameter.table <- 
  fixed.parameter.data %>% 
  dplyr::mutate(value = paste0(round(median, digits = 4), ' (', 
                               round(lower, digits = 4), ', ', 
                               round(upper, digits = 4), ')')) %>% 
  dplyr::select(covariate, covariateLevel, value, data) %>% 
  tidyr::pivot_wider(., names_from = 'data',  values_from = 'value')

fixed.parameter.plot.axis.names <- 
  fixed.parameter.data %>%
  dplyr::select(covariate, covariateLevel, index) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(label = dplyr::case_when(covariate == 'Control ITS terms' & covariateLevel == 'Intercept' ~ r'(Intercept, $\beta_0$)',
                                         covariate == 'Control ITS terms' & covariateLevel == 'Time' ~ r'(Time, $\beta_1$)',
                                         covariate == 'Control ITS terms' & covariateLevel == 'Intervention' ~ r'(Intervention, $\beta_2$)',
                                         covariate == 'Control ITS terms' & covariateLevel == 'Time+' ~ r'(Time$^{+}$, $\beta_3$)',
                                         covariate == 'Difference ITS terms' & covariateLevel == 'Intercept' ~ r'(Intercept, $\beta_4$)',
                                         covariate == 'Difference ITS terms' & covariateLevel == 'Time' ~ r'(Time, $\beta_5$)',
                                         covariate == 'Difference ITS terms' & covariateLevel == 'Intervention' ~ r'(Intervention, $\beta_6$)',
                                         covariate == 'Difference ITS terms' & covariateLevel == 'Time+' ~ r'(Time$^{+}$, $\beta_7$)',
                                         TRUE ~ covariateLevel))

fixed.parameter.plot <-
  ggplot2::ggplot(fixed.parameter.data,
                  aes(y = median, x = index, group = interaction(covariate, covariateLevel, data), colour = data)) +
  ggplot2::scale_color_manual(values = c('red3', 'blue3')) + 
  ggplot2::geom_hline(yintercept = 0, colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point(position = position_dodge(0.5)) +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), width = .05, position = position_dodge(0.5)) +
  ggplot2::scale_x_continuous(labels = unname(c(latex2exp::TeX(fixed.parameter.plot.axis.names %>% dplyr::filter(covariate %in% c('Control ITS terms', 'Difference ITS terms')) %>% dplyr::pull(label)),
                                                fixed.parameter.plot.axis.names %>% dplyr::filter(!(covariate %in% c('Control ITS terms', 'Difference ITS terms'))) %>% dplyr::pull(label))),
                              breaks = 1:nrow(fixed.parameter.plot.axis.names),
                              trans = 'reverse') +
  ggplot2::labs(x = '', y = 'Parameter') +
  ggplot2::annotate("text",
                    y = rep(-5.5, lenght.out = 9),
                    # angle = 270,
                    x = c(2.5, 6.5, 10.5, 13.5, 16.5, 19, 20, 24.5, 31),
                    label = c('Control', 'Difference', 'Age', 'Eduation', 'Ethnicity', 'Relationship', 'Sex', 'Deprivation', 'Diversity'),
                    fontface = 'bold',
                    size = 4) +
  ggplot2::geom_vline(xintercept = c(8.5, 20.5)) +
  ggplot2::annotate("text",
                    y = rep(12, lenght.out = 3),
                    angle = 270,
                    x = c(4, 14.5, 27),
                    label = c('Interrupted time\nseries', 'Individual level\nconfounders', 'Community level\nconfounders'),
                    fontface = 'bold',
                    size = 4) +
  # ggplot2::coord_cartesian(ylim = c(-1.5, 12), clip = 'off') +
  ggplot2::coord_flip(ylim = c(-1.5, 12), clip = 'off') +
  my.theme(legend.title = element_blank(),
           text = element_text(size = text.size),
           legend.position = 'bottom')
fixed.parameter.plot

fixed.parameter.data.non.its <-
  fixed.parameter.data %>% 
  dplyr::filter(!(covariate %in% c('Control ITS terms', 'Difference ITS terms'))) %>% 
  dplyr::mutate(index = rep(1:(n()/2), times = 2))

fixed.parameter.plot.non.its.axis.names <- 
  fixed.parameter.data.non.its %>%
  dplyr::select(covariate, covariateLevel, index) %>% 
  dplyr::distinct() %>% 
  dplyr::pull(covariateLevel)

fixed.parameter.plot.non.its <-
  ggplot2::ggplot(fixed.parameter.data.non.its,
                  aes(y = median, x = index, group = interaction(covariate, covariateLevel, data), colour = data)) +
  ggplot2::scale_color_manual(values = c('red3', 'blue3')) + 
  ggplot2::geom_hline(yintercept = 0, colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point(position = position_dodge(0.5)) +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), width = .05, position = position_dodge(0.5)) +
  ggplot2::scale_x_continuous(labels = fixed.parameter.plot.non.its.axis.names,
                              breaks = 1:length(fixed.parameter.plot.non.its.axis.names),
                              trans = 'reverse') +
  ggplot2::labs(x = '', y = 'Parameter') +
  ggplot2::annotate("text",
                    y = rep(-2.5, lenght.out = 7),
                    # angle = 270,
                    x = c(2.5, 5.5, 8.5, 11, 12, 16.5, 23),
                    label = c('Age', 'Eduation', 'Ethnicity', 'Relationship', 'Sex', 'Deprivation', 'Diversity'),
                    fontface = 'bold',
                    size = 4) +
  ggplot2::geom_vline(xintercept = 12.5) +
  ggplot2::annotate("text",
                    y = rep(1.5, lenght.out = 2),
                    angle = 270,
                    x = c(6, 18),
                    label = c('Individual level\nconfounders', 'Community level\nconfounders'),
                    fontface = 'bold',
                    size = 4) +
  # ggplot2::coord_cartesian(ylim = c(-1.5, 12), clip = 'off') +
  ggplot2::coord_flip(ylim = c(-1.5, 1.5), clip = 'off') +
  my.theme(legend.title = element_blank(),
           text = element_text(size = text.size),
           legend.position = 'bottom'); fixed.parameter.plot.non.its


## centered time ----

before.after.temporal.data <-
  dplyr::bind_rows(theta.predictor.intro$theta.predictor %>% dplyr::mutate(dataType = 'Introduction'),
                   theta.predictor.contAwar$theta.predictor %>% dplyr::mutate(dataType = 'Contextual Awareness')) %>% 
  dplyr::mutate(dataType =  dataType %>% factor(., levels = c('Introduction', 'Contextual Awareness')),
                time = (lubridate::interval(start = ucStartDate, end = interviewDate) %/% months(3))) %>% 
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), mean),
                   .by = c('exposed', 'time', 'dataType')) %>%
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .)) %>%
  dplyr::select(-starts_with('theta:'))

before.after.temporal.plot <-
  ggplot2::ggplot(data = before.after.temporal.data, aes(x = time/4, y = median,
                                                         group = interaction(exposed, dataType))) +
  ggplot2::geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point(aes(shape = dataType, colour = exposed)) +
  ggplot2::geom_line(aes(linetype = dataType, colour = exposed)) +
  ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper, fill = exposed), alpha = 0.25, colour = NA) +
  ggplot2::scale_colour_manual(values = c('blue3', 'red3')) +
  ggplot2::scale_fill_manual(values = c('blue3', 'red3')) +
  ggplot2::scale_linetype_manual(values= c('dashed', 'solid')) +
  ggplot2::scale_shape_manual(values= c('square', 'circle')) +
  ggplot2::guides(fill = guide_legend(override.aes = list(linetype = 0, color = NA))) + 
  ggplot2::scale_x_continuous(limits = c(-11, 9), breaks = -11:9, labels = c(-11:-1, 'Start', 1:9)) +
  ggplot2::labs(x = 'Time since awareness to Universal Credit (years)',
                y = 'Self reported mental ill health (GHQ-12)') +
  my.theme(legend.title = element_blank(),
           legend.position = 'bottom',
           text = element_text(size = text.size)); before.after.temporal.plot

## its parameter ----

### parameter ----

its.control.parameter.data <-
  dplyr::bind_rows(theta.predictor.intro$theta.parameter %>% dplyr::mutate(dataType = 'Introduction'),
                   theta.predictor.contAwar$theta.parameter %>% dplyr::mutate(dataType = 'Contextual Awareness')) %>% 
  dplyr::filter(parameter %in% c('(Intercept):1', 'time_id:1', 'treatment_id:1', 'timeSinceTreatment_id:1')) %>% 
  dplyr::mutate(parameter = parameter %>% factor(., 
                                                 level = c('(Intercept):1', 'time_id:1', 'treatment_id:1', 'timeSinceTreatment_id:1'),
                                                 label = c('beta0', 'beta1', 'beta2', 'beta3')),
                itsTerm = dplyr::case_when(parameter == 'beta0' ~ 'Intercept',
                                           parameter == 'beta1'~ 'Time',
                                           parameter == 'beta2' ~ 'Intervention',
                                           parameter == 'beta3' ~ 'Time+',
                                           TRUE ~ NA),
                type = 'Control')

its.differece.parameter.data <-
  dplyr::bind_rows(theta.predictor.intro$theta.parameter %>% dplyr::mutate(dataType = 'Introduction'),
                   theta.predictor.contAwar$theta.parameter %>% dplyr::mutate(dataType = 'Contextual Awareness')) %>% 
  dplyr::filter(parameter %in% c('exposed_id2:1', 'time.exposed_id:1', 'treatment.exposed_id:1', 'timeSinceTreatment.exposed_id:1')) %>% 
  dplyr::mutate(parameter = parameter %>% factor(., 
                                                 level = c('exposed_id2:1', 'time.exposed_id:1', 'treatment.exposed_id:1', 'timeSinceTreatment.exposed_id:1'),
                                                 label = c('beta4', 'beta5', 'beta6', 'beta7')),
                itsTerm = dplyr::case_when(parameter == 'beta4' ~ 'Intercept',
                                           parameter == 'beta5'~ 'Time',
                                           parameter == 'beta6' ~ 'Intervention',
                                           parameter == 'beta7' ~ 'Time+',
                                           TRUE ~ NA),
                type = 'Difference')

its.exposed.parameter.data <-
  dplyr::bind_rows(its.control.parameter.data,
                   its.differece.parameter.data) %>% 
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), sum),
                   .by = c('itsTerm', 'dataType')) %>% 
  dplyr::mutate(parameter = dplyr::case_when(itsTerm == 'Intercept' ~ 'beta0 + beta4',
                                             itsTerm == 'Time' ~ 'beta1 + beta5',
                                             itsTerm == 'Intervention' ~ 'beta2 + beta6',
                                             itsTerm == 'Time+' ~ 'beta3 + beta7',
                                             TRUE ~ NA) %>% factor(),
                type = 'Exposed')

its.parameter.data <- 
  dplyr::bind_rows(its.control.parameter.data,
                   its.differece.parameter.data,
                   its.exposed.parameter.data) %>% 
  dplyr::relocate(c('type', 'itsTerm'), .before = parameter) %>% 
  dplyr::mutate(dplyr::select(., starts_with('theta:')) %>%
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>%
                  do.call(rbind, .),
                type = type %>% factor(., levels = c('Control', 'Difference', 'Exposed')),
                itsTerm = itsTerm %>% factor(., levels= c('Intercept', 'Time', 'Intervention', 'Time+')),
                dataType = dataType %>% factor(., level = c('Introduction', 'Contextual Awareness'))) %>%
  dplyr::select(-starts_with('theta:')) %>% 
  dplyr::arrange(dataType, type, itsTerm) %>% 
  dplyr::mutate(index = rep(1:(n()/2), times = 2))

its.parameter.table <- 
  its.parameter.data %>% 
  dplyr::mutate(parameter = paste0(itsTerm, ', ', parameter),
                value = paste0(round(median, digits = 4), ' (', 
                               round(lower, digits = 4), ', ', 
                               round(upper, digits = 4), ')')) %>% 
  dplyr::select(dataType, parameter, value) %>% 
  tidyr::pivot_wider(names_from = 'dataType', values_from = 'value')

its.parameter.plot.axis.names <- 
  its.parameter.data %>% 
  dplyr::select(itsTerm, index, type) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(label = 
                  c(r'(Intercept, $\beta_0$)',
                    r'(Time, $\beta_1$)',
                    r'(Intervention, $\beta_2$)',
                    r'(Time$^{+}$, $\beta_3$)',
                    r'(Intercept, $\beta_4$)',
                    r'(Time, $\beta_5$)',
                    r'(Intervention, $\beta_6$)',
                    r'(Time$^{+}$, $\beta_7$)',
                    r'(Intercept, $\beta_0+\beta_4$)',
                    r'(Time, $\beta_1+\beta_5$)',
                    r'(Intervention, $\beta_2+\beta_6$)',
                    r'(Time$^{+}$, $\beta_3+\beta_7$)')) %>% 
  dplyr::pull(label)


its.parameter.plot <-
  ggplot2::ggplot(its.parameter.data,
                  aes(y = median, x = index, group = interaction(type, itsTerm, dataType), colour = dataType)) +
  ggplot2::scale_color_manual(values = c('red3', 'blue3')) + 
  ggplot2::geom_hline(yintercept = 0, colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point(position = position_dodge(0.5)) +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), width = .05, position = position_dodge(0.5)) +
  ggplot2::scale_x_continuous(labels = unname(latex2exp::TeX(its.parameter.plot.axis.names)),
                              breaks = 1:length(its.parameter.plot.axis.names),
                              trans = 'reverse') +
  ggplot2::labs(x = '', y = '') +
  ggplot2::geom_vline(xintercept = c(4.5, 8.5)) +
  ggplot2::annotate("text",
                    y = rep(15, lenght.out = 3),
                    angle = 270,
                    x = c(2.5, 6.5, 10.5),
                    label = c('Control', 'Difference', 'Exposed'),
                    fontface = 'bold',
                    size = 4) +
  # ggplot2::coord_cartesian(ylim = c(-1.5, 12), clip = 'off') +
  ggplot2::coord_flip(clip = 'off') +
  my.theme(legend.title = element_blank(),
           text = element_text(size = text.size),
           legend.position = 'bottom'); its.parameter.plot

## save ----

setwd(res.sensitivity.analysis.dir)

print(x = xtable::xtable(all.parameter.data, digits = 4), 
      include.rownames = FALSE,
      file = 'sensitivityAnalysis_introductionContextual_allParameterTable.tex')

ggplot2::ggsave(filename = 'sensitivityAnalysis_introductionContextual_fixedParameters.png',
                plot = fixed.parameter.plot,
                height = height, width = 1.25*width)

ggplot2::ggsave(filename = 'sensitivityAnalysis_introductionContextual_fixedParameters_nonITS.png',
                plot = fixed.parameter.plot.non.its,
                height = height, width = 1.25*width)

ggplot2::ggsave(filename = 'sensitivityAnalysis_introductionContextual_beforeAfter_temporalPlot.png',
                plot = before.after.temporal.plot,
                height = height, width = width)

print(x = xtable::xtable(its.parameter.table, digits = 4), 
      include.rownames = FALSE,
      file = 'sensitivityAnalysis_introductionContextual_itsParamters.tex')

ggplot2::ggsave(filename = 'sensitivityAnalysis_introductionContextual_itsParamters_plot.png',
                plot = its.parameter.plot,
                height = height, width = width)

save(fit.intro, fit.contAwar, 
     file = 'sensitivityAnalysis_introductionContextual_modelFits.rds')

save(theta.predictor.intro, theta.predictor.contAwar,
     file = 'sensitivityAnalysis_introductionContextual_posteriorSamples.rds')
