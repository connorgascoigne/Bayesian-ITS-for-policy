# get the adjacency matrix ----

getAmat <- function (geo, names){
  nb.r <- spdep::poly2nb(geo, queen = TRUE, row.names = names)
  mat <- spdep::nb2mat(nb.r, style = "B", zero.policy = TRUE)
  regions <- colnames(mat) <- rownames(mat)
  mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])
  return(mat)
}

# themes for plots ----

# theme for plots
my.theme<-function(...){
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour='black'),
        # legend.title=element_blank(),
        legend.text.align=0,
        legend.key=element_rect(fill=NA),
        ...)
}

my.map.theme <- function(...){
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 legend.text.align=0,
                 legend.key=element_rect(fill=NA),
                 panel.background=element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 ...)
}

# summary ----

# function for summary
## quantiles, mean and varaince
my.summary <- function(x, CI = 0.95) {
  
  # x = theta1[,1]; CI = 0.95; 
  
  # lower and upper CI calculators
  lowerCI <- (1 - CI)/2
  upperCI <- 1 - lowerCI
  
  qntl <- quantile(x, probs = c(lowerCI, 0.5, upperCI))
  data.frame(mean = mean(x), variance = var(x), lower = qntl[1], median = qntl[2], upper = qntl[3])
  
}

# extract posterior samples ----

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
                          age, edu, ethn, rela, sex, diversity, deprivation), 
          theta.predictor.a) %>% 
    arrange(time, treatment, LAD22CD, exposed)
  
  # all model predictor
  colnames(theta.parameter.a) <- paste0('theta:', 1:n.sims)
  theta.parameter <- 
    data.frame(parameter = rownames(samp.all[[1]]$latent)[!startsWith(rownames(samp.all[[1]]$latent), 'Predictor:')]) %>% 
    cbind(., theta.parameter.a)
  
  return(list(theta.predictor = theta.predictor,
              theta.parameter = theta.parameter))
  
}

# expit ----

expit <- function (x){
  return(exp(x)/(1 + exp(x)))
}

# logit ----

logit <- function(x){
  return(log(x/(1 - x)))
}

# multiple model fits ----

inla.model.fit <- function(formula, data){
  
  weightsFinal <- data$sampleWeight
  
  inla.mode = 'experimental';
  control.compute = list(config = TRUE, waic = TRUE)
  control.predictor = list(compute = TRUE, link = 1)
  control.inla = list(strategy = 'adaptive', int.strategy = 'auto')
  family = 'gaussian'
  
  INLA::inla(formula, 
             family = family, 
             data = data, 
             weights = weightsFinal,
             control.compute = control.compute, 
             control.predictor = control.predictor, 
             control.inla = control.inla, 
             inla.mode = inla.mode)
  
}

# model scores ----

model.scores <- function(results, true, alpha = 0.05){
  
  median <- results$summary.fitted.values$`0.5quant`
  lower <- results$summary.fitted.values$`0.025quant`
  upper <- results$summary.fitted.values$`0.975quant`
  
  # variance scores
  mae <- (median - true) %>% abs()
  mse <- (median - true)^2
  
  # distribution score
  dispersion <- (upper - lower)
  overPrediction <- 2/alpha * (lower - true) * (true < lower)
  underPrediction <- 2/alpha * (true - upper) * (true > upper)
  intervalScore <- (dispersion + underPrediction + overPrediction) %>% mean()
  width <- dispersion %>% mean()
  coverage <- ((lower < true & true < upper) %>% sum())/length(true)
  
  return(list(mae = mae, mse = mse,
              intervalScore = intervalScore,
              width = width,
              coverage = coverage))
  
}
