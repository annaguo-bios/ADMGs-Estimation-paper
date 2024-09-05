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
library(flexCausal)
library(utils)

# superlearner.seq = F; # whether run superlearner for sequential regression
# superlearner.Y=F; # whether run superlearner for outcome regression
# superlearner.A=F; # whether run superlearner for propensity score
# superlearner.M=F; # whether run superlearner for estimating densratio for M using bayes method
# superlearner.L=F; # whether run superlearner for estimating densratio for L using bayes method
# crossfit=F; K=5;
# lib.seq = c("SL.glm","SL.earth","SL.ranger","SL.mean"); # superlearner library for sequential regression
# lib.L = c("SL.glm","SL.earth","SL.ranger","SL.mean"); # superlearner library for density ratio estimation via bayes rule for variables in L
# lib.M = c("SL.glm","SL.earth","SL.ranger","SL.mean"); # superlearner library for density ratio estimation via bayes rule for variables in M
# lib.Y = c("SL.glm","SL.earth","SL.ranger","SL.mean"); # superlearner library for outcome regression
# lib.A = c("SL.glm","SL.earth","SL.ranger","SL.mean"); # superlearner library for propensity score
# formulaY="Y ~ ."; formulaA="A ~ ."; # regression formula for outcome regression and propensity score if superlearner is not used
# linkY_binary="logit"; linkA="logit"; # link function for outcome regression and propensity score if superlearner is not used
# n.iter=500; cvg.criteria=0.01;
# truncate_lower=0; truncate_upper=1; zerodiv.avoid=0
# 
# dgp.f.name="YnotL-dgp.R"
# truth="YnotL-truth.Rdata"
# n = 4000
# a = "c(1,0)"
# vertices = 'c(\"A\", \"M\", \"L\", \"Y\", \"X\")' # vertices
# di_edges =  'list(c(\"X\", \"A\"), c(\"X\", \"M\"), c(\"X\", \"L\"), c(\"X\", \"Y\"), c(\"A\", \"M\"), c(\"M\", \"L\"), c(\"L\", \"Y\"), c(\"A\", \"Y\"))' # directed edges
# bi_edges = 'list(c(\"A\", \"L\"), c(\"M\", \"Y\"))' # bidirected edges
# 
# # vertices=eval(parse(text = vertices))
# # bi_edges=eval(parse(text = bi_edges))
# # di_edges=eval(parse(text = di_edges))
# # multivariate.variables = eval(parse(text = multivariate.variables))
# 
# treatment = "A" # name of the treatment variable
# outcome = "Y" # name of the outcome variable
# multivariate.variables = 'list(M = c(\"M.1\", \"M.2\"))'  # name of the multivariate variables
# ratio.method.L="densratio" # method for estimating the density ratio associated with M
# ratio.method.M="densratio" # method for estimating the density ratio associated with L
# 
# (output$TMLE.Y1$EIF.Y - output$TMLE.Y0$EIF.Y)^2 %>% mean()
# (output$Onestep.Y1$EIF.Y - output$Onestep.Y0$EIF.Y)^2 %>% mean()
# 
# (output$TMLE.Y1$EIF.A - output$TMLE.Y0$EIF.A)^2 %>% mean()
# (output$Onestep.Y1$EIF.A - output$Onestep.Y0$EIF.A)^2 %>% mean()
# 
# (output$TMLE.Y1$EIF.v - output$TMLE.Y0$EIF.v)^2 %>% mean()
# (output$Onestep.Y1$EIF.v - output$Onestep.Y0$EIF.v)^2 %>% mean()
# 
# 
# 
# a.tmle <- output$TMLE.Y1$EIF.A - output$TMLE.Y0$EIF.A
# b.tmle <- (output$TMLE.Y1$EIF.v - output$TMLE.Y0$EIF.v)
# c.tmle <- (output$TMLE.Y1$EIF.Y - output$TMLE.Y0$EIF.Y)
# 
# a.one <- output$Onestep.Y1$EIF.A - output$Onestep.Y0$EIF.A
# b.one <- (output$Onestep.Y1$EIF.v - output$Onestep.Y0$EIF.v)
# c.one <- (output$Onestep.Y1$EIF.Y - output$Onestep.Y0$EIF.Y)
# 
# a.tmle^2 %>% mean()
# b.tmle^2 %>% mean()
# c.tmle^2 %>% mean()
# 
# a.tmle %>% mean(); a.one %>% mean()
# b.tmle %>% mean(); b.one %>% mean()
# c.tmle %>% mean(); c.one %>% mean()
# 
# output$TMLE.Y1$estimated_psi; output$Onestep.Y1$estimated_psi
# output$TMLE.Y0$estimated_psi; output$Onestep.Y0$estimated_psi
# output$TMLE.Y1$estimated_psi-output$TMLE.Y0$estimated_psi; output$Onestep.Y1$estimated_psi-output$Onestep.Y0$estimated_psi
# 
# 
# (2*a.tmle*b.tmle) %>% mean()
# (2*a.tmle*c.tmle) %>% mean()
# (2*b.tmle*c.tmle) %>% mean()
# 
# a.one^2 %>% mean()
# b.one^2 %>% mean()
# c.one^2 %>% mean()
# 
# (2*a.one*b.one) %>% mean()
# (2*a.one*c.one) %>% mean()
# (2*b.one*c.one) %>% mean()
# 
# output$TMLE$EIF^2 %>% mean()
# output$Onestep$EIF^2 %>% mean()

set.seed(seed)
#################################################
# Functions
#################################################
source(paste0("../DGPs/",dgp.f.name)) # generate_data(n)


#################################################
# Load truth
#################################################
load(paste0("../DGPs/",truth))

# generate data
dat_output = generate_data(n)
data = dat_output$data
attach(data, warn.conflicts=FALSE)


# run TMLE
output <- ADMGtmle(a=eval(parse(text=a)),data=data, vertices=eval(parse(text = vertices)),
                           bi_edges=eval(parse(text = bi_edges)),
                           di_edges=eval(parse(text = di_edges)),
                           treatment=treatment, outcome=outcome,
                           multivariate.variables = eval(parse(text = multivariate.variables)),
                   ratio.method.L=ratio.method.L,
                   ratio.method.M=ratio.method.M)

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
