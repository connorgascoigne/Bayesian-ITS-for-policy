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

# expit ----

expit <- function (x){
  return(exp(x)/(1 + exp(x)))
}

# logit ----

logit <- function(x){
  return(log(x/(1 - x)))
}