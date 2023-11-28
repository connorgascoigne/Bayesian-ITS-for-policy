# packages ----

library(tidyverse)

# directories ----

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-3)], collapse = "/")
code.dir <- paste0(home.dir, '/Code')
data.dir <- paste0(home.dir, '/Data')
res.dir <- paste0(home.dir, '/Results')
res.data.exploration.dir <- paste0(res.dir, '/Data Exploration')

# results folder
if(!dir.exists(paths = res.dir)) {
  dir.create(path = res.dir)
}

# Data exploration folder
if(!dir.exists(paths = res.data.exploration.dir)) {
  dir.create(path = res.data.exploration.dir)
}

# saving ----

height <- width <- 10
text.size <- 20

# imports ----

## functions ----

source(paste0(code.dir, '/functions.R'))

# basic ITS ----

## data generation ----

set.seed(1234)

timePre.exposed <- -100:0
interceptPre.exposed <- 200
slopePre.exposed <- 0.25 
timePost.exposed <- 1:100
interceptPost.exposed <- 300
slopePost.exposed <- 1 

timePre.control <- -100:0
interceptPre.control <- 1
slopePre.control <- 0.03 
timePost.control <- 1:100
interceptPost.control <- 50
slopePost.control <- 0.5 

data.exposed <-
  data.frame(intercept = c(rep(interceptPre.exposed, times = length(timePre.exposed)), 
                           rep(interceptPost.exposed, times = length(timePost.exposed))),
             slope = c(rep(slopePre.exposed, times = length(timePre.exposed)), 
                       rep(slopePost.exposed, times = length(timePost.exposed))),
             time = c(timePre.exposed, timePost.exposed)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(y = intercept + (slope * time) + rnorm(1, 0, 5)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(T = dplyr::row_number(),
                I = dplyr::if_else(time <= 0, 0, 1),
                P = dplyr::if_else(time <= 0, 0, time))

data.control <-
  data.frame(intercept = c(rep(interceptPre.control, times = length(timePre.control)), 
                           rep(interceptPost.control, times = length(timePost.control))),
             slope = c(rep(slopePre.control, times = length(timePre.control)), 
                       rep(slopePost.control, times = length(timePost.control))),
             time = c(timePre.control, timePost.control)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(y = intercept + (slope * time) + rnorm(1, 0, 5)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(T = dplyr::row_number(),
                I = dplyr::if_else(time <= 0, 0, 1),
                P = dplyr::if_else(time <= 0, 0, time))

## model fit ----

fit.exposed <- lm('y ~ T + I + P', data = data.exposed)
fit.control <- lm('y ~ T + I + P', data = data.control)

overall.time.fit.exposed <- lm('y ~ T', data = data.exposed)
overall.time.fit.control <- lm('y ~ T', data = data.control)

counterfactual.fit.exposed <- lm('y ~ T', data = data.exposed %>% dplyr::filter(I == 0))
counterfactual.fit.control <- lm('y ~ T', data = data.control %>% dplyr::filter(I == 0))

counterfactual.pred.exposed <- 
  predict(object = counterfactual.fit.exposed,
          newdata = data.exposed %>% dplyr::select(T)) %>% 
  as.numeric()
counterfactual.pred.control <- 
  predict(object = counterfactual.fit.control,
          newdata = data.exposed %>% dplyr::select(T)) %>% 
  as.numeric()

## plot ----

### ITS without controls ----

pred.data <- 
  data.frame(time = rep(data.control$time, times = 2),
             T = rep(data.control$T, times = 2),
             I = rep(data.control$I, times = 2),
             P = rep(data.control$P, times = 2)) %>% 
  dplyr::mutate(group = 
                  rep(c('Exposed', 'Control'), each = nrow(data.exposed)) %>% 
                  factor(., levels = c('Exposed', 'Control')),
                y = c(data.exposed$y, data.control$y),
                yPred = c(fit.exposed$fitted.values, fit.control$fitted.values),
                yCounterfactual = c(counterfactual.pred.exposed, counterfactual.pred.control),
                yOverallTime = c(overall.time.fit.exposed$fitted.values, overall.time.fit.control$fitted.values))

basic.its.plot <-
  ggplot2::ggplot(data = pred.data %>% filter(group == 'Control'), aes(x = time)) +
  ggplot2::geom_vline(xintercept = 0, linetype = 'dashed', colour = 'black', linewidth = 1) +
  # ggplot2::geom_point(aes(y = y), alpha = 0.5, color = 'blue3') +
  ggplot2::geom_line(aes(y = yCounterfactual), color = 'blue3', linewidth = 1, linetype = 'dashed') +
  ggplot2::geom_line(aes(y = yPred), color = 'blue3', linewidth = 1) +
  ggplot2::geom_line(aes(y = yOverallTime), color = 'blue3', linewidth = 1, linetype = 'dotted') +
  # ggplot2::geom_ribbon(aes(ymin = yCounterfactual, ymax = yPred), colour = NA, fill = 'blue3', alpha = 0.1) +
  ggplot2::annotate('text', 
                    x = - 100, y = 10, 
                    label = latex2exp::TeX("$\\beta_0", output="character"), 
                    colour = 'blue3', size = 7.5, hjust = 0,
                    parse = TRUE) +
  ggplot2::annotate('text', 
                    x = -20, y = 30, 
                    label = latex2exp::TeX("$\\beta_1", output="character"), 
                    colour = 'blue3', size = 7.5, angle = 35,
                    parse = TRUE) +
  ggplot2::annotate('text', 
                    x = 2.5, y = 25, 
                    label = latex2exp::TeX("$\\beta_2", output="character"), 
                    colour = 'blue3', size = 7.5, hjust = 0,
                    parse = TRUE) +
  ggplot2::annotate('text', 
                    x = 50, y = 85, 
                    label = latex2exp::TeX("$\\beta_3", output="character"), 
                    colour = 'blue3', size = 7.5, angle = 25,
                    parse = TRUE) +
  ggplot2::annotate('text', 
                    x = -2.5, y = 100, 
                    label = 'Pre-Intervention', 
                    colour = 'black', size = 7.5, hjust = 1) +
  ggplot2::annotate('text', 
                    x = 2.5, y = 100, 
                    label = 'Post-Intervention', 
                    colour = 'black', size = 7.5, hjust = 0) +
  ggplot2::labs(y = 'Observed Outcome', x = 'Time') +
  my.theme(legend.title = element_blank(),
           legend.position = c(0.1, 0.9),
           text = element_text(size = text.size)); basic.its.plot

### ITS with controls ----

controlled.its.plot <-
  ggplot2::ggplot(data = pred.data, aes(x = time, group = group, shape = group, colour = group)) +
  ggplot2::geom_vline(xintercept = 0, linetype = 'dashed', colour = 'black', linewidth = 1) +
  # ggplot2::geom_point(aes(y = y), alpha = 0.5) +
  ggplot2::geom_line(aes(y = yCounterfactual), linewidth = 1, linetype = 'dashed') +
  ggplot2::geom_line(aes(y = yPred), linewidth = 1) +
  ggplot2::geom_line(aes(y = yOverallTime), linewidth = 1, linetype = 'dotted') +
  # ggplot2::geom_ribbon(aes(ymin = yCounterfactual, ymax = yPred, fill = group), colour = NA, alpha = 0.1) +
  ggplot2::annotate('text', 
                    x = - 100, y = 200, 
                    label = latex2exp::TeX("$\\beta_0+\\beta_4", output="character"), 
                    colour = 'red3', size = 7.5, hjust = 1,
                    parse = TRUE) +
  ggplot2::annotate('text', 
                    x = - 100, y = 20, 
                    label = latex2exp::TeX("$\\beta_0", output="character"), 
                    colour = 'blue3', size = 7.5, hjust = 1,
                    parse = TRUE) +
  ggplot2::annotate('text', 
                    x = -25, y = 250, 
                    label = latex2exp::TeX("$\\beta_1+\\beta_5", output="character"), 
                    colour = 'red3', size = 7.5, angle = 35,
                    parse = TRUE) +
  ggplot2::annotate('text', 
                    x = -25, y = 35, 
                    label = latex2exp::TeX("$\\beta_1", output="character"), 
                    colour = 'blue3', size = 7.5, angle = 10,
                    parse = TRUE) +
  ggplot2::annotate('text', 
                    x = 5, y = 250, 
                    label = latex2exp::TeX("$\\beta_2+\\beta_6", output="character"), 
                    colour = 'red3', size = 7.5, hjust = 0,
                    parse = TRUE) +
  ggplot2::annotate('text', 
                    x = 5, y = 25, 
                    label = latex2exp::TeX("$\\beta_2", output="character"), 
                    colour = 'blue3', size = 7.5, hjust = 0,
                    parse = TRUE) +
  ggplot2::annotate('text', 
                    x = 50, y = 365, 
                    label = latex2exp::TeX("$\\beta_3+\\beta_7", output="character"), 
                    colour = 'red3', size = 7.5, angle = 30,
                    parse = TRUE) +
  ggplot2::annotate('text', 
                    x = 50, y = 90, 
                    label = latex2exp::TeX("$\\beta_3", output="character"), 
                    colour = 'blue3', size = 7.5, angle = 20,
                    parse = TRUE) +
  ggplot2::annotate('text', 
                    x = -5, y = 450, 
                    label = 'Pre-Intervention', 
                    colour = 'black', size = 7.5, hjust = 1) +
  ggplot2::annotate('text', 
                    x = 5, y = 450, 
                    label = 'Post-Intervention', 
                    colour = 'black', size = 7.5, hjust = 0) +
  ggplot2::scale_colour_manual(values = c('red3', 'blue3')) +
  ggplot2::scale_fill_manual(values = c('red3', 'blue3')) +
  ggplot2::expand_limits(x = c(-125, 100), y = c(-10, 450)) +
  ggplot2::scale_y_continuous(breaks = seq(from = 0, to = 450, by = 50)) +
  ggplot2::scale_x_continuous(breaks = seq(from = -150, to = 100, by = 50)) +
  ggplot2::labs(y = 'Observed Outcome', x = 'Time') +
  my.theme(legend.title = element_blank(),
           legend.position = c(0.1, 0.9),
           text = element_text(size = text.size)); controlled.its.plot

### save ----

setwd(res.data.exploration.dir)

ggplot2::ggsave(basic.its.plot,
                filename = 'basic_its_plot.png',
                width = width, height = height)

ggplot2::ggsave(controlled.its.plot,
                filename = 'controlled_its_plot.png',
                width = width, height = height)



