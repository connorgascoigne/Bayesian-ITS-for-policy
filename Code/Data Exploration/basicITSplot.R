# packages ----

library(tidyverse)

# directories ----

# extract file location of this script
codePath <- rstudioapi::getActiveDocumentContext()$path
codePathSplitted <- strsplit(codePath, "/")[[1]]

# retrieve directories
homeDir <- paste(codePathSplitted[1: (length(codePathSplitted)-3)], collapse = "/")
codeDir <- paste0(homeDir, '/Code')
dataDir <- paste0(homeDir, '/Data')
resDir <- paste0(homeDir, '/Results')
resDir_dataExploration <- paste0(resDir, '/Data Exploration')

# results folder
if(!dir.exists(paths = resDir)) {
  dir.create(path = resDir)
}

# Data exploration folder
if(!dir.exists(paths = resDir_dataExploration)) {
  dir.create(path = resDir_dataExploration)
}

# saving ----

height <- width <- 10
textSize <- 20

# imports ----

## functions ----

source(paste0(codeDir, '/functions.R'))

# basic ITS ----

## data generation ----

set.seed(1234)

timePre.exposed <- -100:0
interceptPre.exposed <- 50
slopePre.exposed <- 0.25 
timePost.exposed <- 1:100
interceptPost.exposed <- 80
slopePost.exposed <- 1 

timePre.control <- -100:0
interceptPre.control <- 1
slopePre.control <- 0 
timePost.control <- 1:100
interceptPost.control <- 2
slopePost.control <- 0.5 

data.exposed <-
  data.frame(intercept = c(rep(interceptPre.exposed, times = length(timePre.exposed)), 
                           rep(interceptPost.exposed, times = length(timePost.exposed))),
             slope = c(rep(slopePre.exposed, times = length(timePre.exposed)), 
                        rep(slopePost.exposed, times = length(timePost.exposed))),
             time = c(timePre.exposed, timePost.exposed)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(y = intercept + slope * time + rnorm(1, 0, 5)) %>% 
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
  dplyr::mutate(y = intercept + slope * time + rnorm(1, 0, 5)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(T = dplyr::row_number(),
                I = dplyr::if_else(time <= 0, 0, 1),
                P = dplyr::if_else(time <= 0, 0, time))

## model fit ----

fit.exposed <- lm('y ~ T + I + P', data = data.exposed)
fit.control <- lm('y ~ T + I + P', data = data.control)

## plot ----

pred <- 
  data.frame(time = rep(data.control$time, times = 2),
             T = rep(data.control$T, times = 2),
             I = rep(data.control$I, times = 2),
             P = rep(data.control$P, times = 2)) %>% 
  dplyr::mutate(group = 
                  rep(c('Exposed', 'Control'), each = nrow(data.exposed)) %>% 
                  factor(., levels = c('Exposed', 'Control')),
                y = c(data.exposed$y, data.control$y),
                yPred = c(fit.exposed$fitted.values, fit.control$fitted.values))

basicITSPlot <-
  ggplot2::ggplot(data = pred %>% filter(group == 'Control'), aes(x = time)) +
  ggplot2::geom_vline(xintercept = 0, linetype = 'dashed', colour = 'black', linewidth = 1) +
  ggplot2::geom_point(aes(y = y), alpha = 0.5, color = 'blue3') +
  ggplot2::geom_line(aes(y = yPred), linewidth = 1, color = 'blue3') +
  ggplot2::annotate('text', x = 32, y = 172, label = 'Intervention Occurs', colour = 'black', size = 7.5) +
  ggplot2::labs(y = 'Observed Outcome', x = 'Time') +
  my.theme(legend.title = element_blank(),
           legend.position = c(0.1, 0.9),
           text = element_text(size = textSize)); basicITSPlot

ggplot2::ggsave(basicITSPlot,
                filename = paste0(resDir_dataExploration, '/basicITSPlot.png'),
                width = width, height = height)

basicControlExposedITSPlot <-
  ggplot2::ggplot(data = pred, aes(x = time, group = group, shape = group, colour = group)) +
  ggplot2::geom_vline(xintercept = 0, linetype = 'dashed', colour = 'black', linewidth = 1) +
  ggplot2::geom_point(aes(y = y), alpha = 0.5) +
  ggplot2::geom_line(aes(y = yPred), linewidth = 1) +
  ggplot2::scale_colour_manual(values = c('red3', 'blue3')) +
  ggplot2::annotate('text', x = 32, y = 172, label = 'Intervention Occurs', colour = 'black', size = 7.5) +
  ggplot2::labs(y = 'Observed Outcome', x = 'Time') +
  my.theme(legend.title = element_blank(),
           legend.position = c(0.1, 0.9),
           text = element_text(size = textSize)); basicControlExposedITSPlot

ggplot2::ggsave(basicControlExposedITSPlot,
                filename = paste0(resDir_dataExploration, '/basicControlExposedITSPlot.png'),
                width = width, height = height)



