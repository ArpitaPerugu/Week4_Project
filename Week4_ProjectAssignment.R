---
title: "Investigate the exponential distribution in R and compare it with the Central
Limit Theorem"
author: "Arpita"
date: "04/09/2020"
output: pdf_document
---

 
#Load packages
 
  library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')
library(data.table)
library(ggplot2)
n <- 40 # number of exponentials (sample size)
lambda <- 0.2 # lambda for rexp (limiting factor) (rate)
nosim <- 1000 # number of simulations
quantile <- 1.96 # 95th % quantile to be used in Confidence Interval
set.seed(234) # set the seed value for reproducibility
# Use rexp() and matrix() to generate 40 samples creating a matrix with 1000 rows and 40 columns.
simData <- matrix(rexp(n * nosim, rate = lambda), nosim)
simMeans <- rowMeans(simData) # Matrix Mean
sampleMean <- mean(simMeans) # Mean of sample means
sampleMean
theoMean <- 1 / lambda # Theoretical Mean
theoMean
sampleVar <- var(simMeans)
sampleVar
theoVar  <- (1 / lambda)^2 / (n) 
theoVar
sampleSD <- sd(simMeans)
sampleSD
theoSD <- 1/(lambda * sqrt(n))
theoSD
plotdata <- data.frame(simMeans)
m <- ggplot(plotdata, aes(x =simMeans))
plotdata <- data.frame(simMeans)
m <- ggplot(plotdata, aes(x =simMeans))
m <- m + geom_histogram(aes(y=..density..), colour="black",
                        fill = "lightblue",bins=10)
m <- m + labs(title = "Distribution of averages of 40 Samples", x = "Mean of 40 Samples", y = "Density")
m <- m + geom_vline(aes(xintercept = sampleMean, colour = "green"))
m <- m + geom_vline(aes(xintercept = theoMean, colour = "violet"))
m <- m + stat_function(fun = dnorm, args = list(mean = sampleMean, sd = sampleSD), color = "blue", size = 1.0)
m <- m + stat_function(fun = dnorm, args = list(mean = theoMean, sd = theoSD), colour = "red", size = 1.0)
m

