args = commandArgs(trailingOnly=T)
n=as.integer(args[1]) # sample size for the simulation
seed=as.integer(args[2])  # seeds for replication
dgp.f.name=args[3] # name for the DGP function
truth=args[4] # name for the truth.Rdata
out.path.tmle= args[5] # path for the output folder
out.path.onestep= args[6] # path for the output folder
a = args[7] # treatment level
vertices = args[8] # vertices
di_edges = args[9] # directed edges
bi_edges = args[10] # bidirected edges
treatment = args[11] # name of the treatment variable
outcome = args[12] # name of the outcome variable
multivariate.variables = args[13] # name of the multivariate variables
ratio.method.L = args[14] # method for estimating the density ratio associated with M
ratio.method.M = args[15] # method for estimating the density ratio associated with L
lib.seq = args[16] # superlearner lib for sequential regression
lib.L = args[17] # superlearner lib for L via bayes
lib.M = args[18] # superlearner lib for M via bayes
lib.Y = args[19] # superlearner lib for Y
lib.A = args[20] # superlearner lib for A
superlearner.seq = as.logical(args[21]) # whether to use superlearner for sequential regression
superlearner.L = as.logical(args[22]) # whether to use superlearner for L
superlearner.M = as.logical(args[23]) # whether to use superlearner for M
superlearner.Y = as.logical(args[24]) # whether to use superlearner for Y
superlearner.A = as.logical(args[25]) # whether to use superlearner for A
crossfit = as.logical(args[26]) # number of folds for crossfit
zerodiv.avoid=as.numeric(args[27]) # sample size for the simulation
dnorm.formula.L = args[28] # formula for the density ratio associated with L
dnorm.formula.M = args[29] # formula for the density ratio associated with M


SL.ranger.1k <- function(...,num.trees = 1000){
  SL.ranger(...,num.trees=num.trees)
}

# by default the density ratio method is bayes

if(file.exists(paste0(out.path.tmle,"output_",n,"_",seed,".Rdata")) & file.exists(paste0(out.path.onestep,"output_",n,"_",seed,".Rdata"))){
  stop("File exists!")
}

library(ggplot2)
library(ggpubr)
library(latex2exp)
library(reshape2)
library(stats)
library(haldensify)
library(np)
library(xtable)
library(SuperLearner)
library(densratio)
library(MASS)
library(mvtnorm)
library(ADMGtmle)
library(utils)





#################################################
# Functions
#################################################
source(paste0("../../../DGPs/",dgp.f.name)) # generate_data(n)


#################################################
# Load truth
#################################################
load(paste0("../../../DGPs/", truth))

  
set.seed(seed)

# Generate data
dat_output <- generate_data(n)
data <- dat_output$data
attach(data, warn.conflicts = FALSE)



output <- ADMGtmle(a = eval(parse(text = a)), data = data, vertices = eval(parse(text = vertices)),
                   bi_edges = eval(parse(text = bi_edges)),
                   di_edges = eval(parse(text = di_edges)),
                   treatment = treatment, outcome = outcome,
                   multivariate.variables = eval(parse(text = multivariate.variables)),
                   ratio.method.L = ratio.method.L,
                   ratio.method.M = ratio.method.M,
                   dnorm.formula.L = eval(parse(text=dnorm.formula.L)),
                   dnorm.formula.M = eval(parse(text=dnorm.formula.M)),
                   lib.seq = eval(parse(text = lib.seq)),
                   lib.L = eval(parse(text = lib.L)),
                   lib.M = eval(parse(text = lib.M)),
                   lib.Y = eval(parse(text = lib.Y)),
                   lib.A = eval(parse(text = lib.A)),
                   superlearner.seq = superlearner.seq,
                   superlearner.L = superlearner.L,
                   superlearner.M = superlearner.M,
                   superlearner.Y = superlearner.Y,
                   superlearner.A = superlearner.A,
                   crossfit = crossfit, zerodiv.avoid = zerodiv.avoid)


  
  




print("Estimation done")


# estimate E[Y(1)], E[Y(0)], and ATE
hat_ATE = output$TMLE$ATE

# lower CI
lower.ci_ATE = output$TMLE$lower.ci
# lower.ci_Y1 = hat_E.Y1-1.96*sqrt(mean(tmle_output_Y1$EIF^2-tmle_output_Y1$EIF)/n)
# lower.ci_Y0 = hat_E.Y0-1.96*sqrt(mean(tmle_output_Y0$EIF^2-tmle_output_Y0$EIF)/n)
# lower.ci_ATE = hat_ATE - 1.96*sqrt(mean((tmle_output_Y1$EIF-tmle_output_Y0$EIF)^2-(tmle_output_Y1$EIF-tmle_output_Y0$EIF))/n)

# upper CI
upper.ci_ATE = output$TMLE$upper.ci
# lower.ci_Y1 = hat_E.Y1+1.96*sqrt(mean(tmle_output_Y1$EIF^2-tmle_output_Y1$EIF)/n)
# lower.ci_Y0 = hat_E.Y0+1.96*sqrt(mean(tmle_output_Y0$EIF^2-tmle_output_Y0$EIF)/n)
# upper.ci_ATE = hat_ATE + 1.96*sqrt(mean((tmle_output_Y1$EIF-tmle_output_Y0$EIF)^2-(tmle_output_Y1$EIF-tmle_output_Y0$EIF))/n)

# compute bias
bias_ATE = hat_ATE - ATE

save(list = c("bias_ATE","hat_ATE","lower.ci_ATE","upper.ci_ATE"),file = paste0(out.path.tmle,"output_",n,"_",seed,".Rdata"))


# estimate E[Y(1)], E[Y(0)], and ATE
hat_ATE = output$Onestep$ATE

# lower CI
lower.ci_ATE = output$Onestep$lower.ci
# lower.ci_Y1 = hat_E.Y1-1.96*sqrt(mean(tmle_output_Y1$EIF^2-tmle_output_Y1$EIF)/n)
# lower.ci_Y0 = hat_E.Y0-1.96*sqrt(mean(tmle_output_Y0$EIF^2-tmle_output_Y0$EIF)/n)
# lower.ci_ATE = hat_ATE - 1.96*sqrt(mean((tmle_output_Y1$EIF-tmle_output_Y0$EIF)^2-(tmle_output_Y1$EIF-tmle_output_Y0$EIF))/n)

# upper CI
upper.ci_ATE = output$Onestep$upper.ci
# lower.ci_Y1 = hat_E.Y1+1.96*sqrt(mean(tmle_output_Y1$EIF^2-tmle_output_Y1$EIF)/n)
# lower.ci_Y0 = hat_E.Y0+1.96*sqrt(mean(tmle_output_Y0$EIF^2-tmle_output_Y0$EIF)/n)
# upper.ci_ATE = hat_ATE + 1.96*sqrt(mean((tmle_output_Y1$EIF-tmle_output_Y0$EIF)^2-(tmle_output_Y1$EIF-tmle_output_Y0$EIF))/n)

# compute bias
bias_ATE = hat_ATE - ATE

save(list = c("bias_ATE","hat_ATE","lower.ci_ATE","upper.ci_ATE"),file = paste0(out.path.onestep,"output_",n,"_",seed,".Rdata"))
