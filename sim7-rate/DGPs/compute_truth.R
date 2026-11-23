args = commandArgs(trailingOnly=T)
truth.f.name=args[1] # require specifying truth function, e.g. truth.f.name <- "0-truth-binary.R"
dgp.f.name=args[2] # the dgp.R function, e.g. dgp.f.name <- "0-dgp-binary.R"
out.name=args[3] # require specifying the name for the output file, e.g. out.name <- "0-truth-binary.Rdata"
N=as.integer(args[4]) # require specifying the sample size for computing the truth numerically, e.g. N <- 100000

library(ggplot2)
library(ggpubr)
library(latex2exp)
library(reshape2)
library(stats)
library(xtable)
library(cubature)
library(MASS)
library(mvtnorm)
library(Matrix)

set.seed(7)

#################################################
# Functions
#################################################
source(truth.f.name) # compute_truth(n)
source(dgp.f.name) # generate_data(n)


#################################################
# True parameters
#################################################

## true psi and variance
truth_output <- compute_truth(n = N)

# to check that the mean of EIF=0
mean(truth_output$EIF_ATE)

ATE = truth_output$ATE
VAR.ATE = truth_output$VAR.ATE

paste("ATE = ", round(ATE, 4), ", VAR(ATE) = ", round(VAR.ATE, 4))

save(list = c("ATE","VAR.ATE"),file = out.name)
