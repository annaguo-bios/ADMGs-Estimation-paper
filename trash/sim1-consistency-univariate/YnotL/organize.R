args = commandArgs(trailingOnly=T)
out.path =args[1] # path for the output folder
out.name=args[2] # require specifying output file name, e.g. out.name <- "continuous_dat.Rdata"
n.vec.ind=args[3] # sample size indicator, 1: the long sample size vector, 0: the short sample size vector
nsim=as.integer(args[4]) # require specifying number of simulations, e.g. nsim <- 1000
truth=args[5] # path+name for the truth.Rdata
method=args[6] # method for estimation, e.g. method <- "Onestep/" or method <- "TMLE/"

library(ggplot2)
library(ggpubr)
library(latex2exp)
library(reshape2)
library(stats)
library(xtable)
library(here)

if (n.vec.ind=="1"){
  n.vec <- c(250,500,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
} else if (n.vec.ind=="0"){
  n.vec <- c(250,500,1000,2000,4000)
} else if (n.vec.ind=="2"){
  n.vec <- c(500,1000,2000,4000)
} else if (n.vec.ind=="3"){
  n.vec <- c(500,1000,2000)
} else if (n.vec.ind=="4"){
  n.vec <- c(250,500,1000,2000,4000,8000)
}else {
  n.vec <- c(as.integer(n.vec.ind))
}

#################################################
# Load truth
#################################################
load(truth)


############################
# Organize results
#
############################

# record results
bias_matrix_ate <- matrix(nrow = nsim, ncol = length(n.vec))

est_matrix_ate <- matrix(nrow = nsim, ncol = length(n.vec))

ci_matrix_ate_lower <- matrix(nrow = nsim, ncol = length(n.vec))

ci_matrix_ate_upper <- matrix(nrow = nsim, ncol = length(n.vec))



# 95% CI coverage

ci_coverage_ATE <- data.frame(n=n.vec, # sample size
                              coverage=vector(mode="integer",length=length(n.vec)), # 95% CI coverage
                              coverage.lower=vector(mode="integer",length=length(n.vec)),coverage.upper=vector(mode="integer",length=length(n.vec))) # 95% CI for the above coverage
# average point estimate
avg.ate <- data.frame(n=n.vec, # sample size
                      est=vector(mode="integer",length=length(n.vec)), # E(phi_hat)
                      sd=vector(mode="integer",length=length(n.vec)), # sd
                      upper=vector(mode="integer",length=length(n.vec)), # E(phi_hat)-1.96*sd
                      lower=vector(mode="integer",length=length(n.vec))) # E(phi_hat)+1.96*sd

avg.bias_ate <- data.frame(n=n.vec, # sample size
                           bias=vector(mode="integer",length=length(n.vec)), # sqrt(n)*E{E(phi_hat-phi_0)}
                           upper=vector(mode="integer",length=length(n.vec)), # bias-1.96*sd
                           lower=vector(mode="integer",length=length(n.vec))) # bias+1.96*sd

avg.variance_ate <- data.frame(n=n.vec, # sample size
                               variance=vector(mode="integer",length=length(n.vec)), # n*E{E(phi_hat-E(phi_hat))^2}
                               upper=vector(mode="integer",length=length(n.vec)), # var-1.96*sd
                               lower=vector(mode="integer",length=length(n.vec))) # var+1.96*sd

avg.MSE_ate <- data.frame(n=n.vec,  # sample size
                          mse=vector(mode="integer",length=length(n.vec)), # n*E(phi_hat-phi_0)^2
                          upper=vector(mode="integer",length=length(n.vec)), # mse-1.96*sd
                          lower=vector(mode="integer",length=length(n.vec))) # mse+1.96*sd


for (i in seq_along(n.vec)){

  # sample size
  n <- n.vec[i]

  for (t in 1:nsim){

    load(paste0(method,out.path,"output_",n,"_",t,".Rdata"))

    # record bias
    bias_matrix_ate[t,i] <- hat_ATE - ATE

    # record point estimate
    est_matrix_ate[t,i] <- hat_ATE

    # record lower CI
    ci_matrix_ate_lower[t,i] <- lower.ci_ATE

    # record upper CI
    ci_matrix_ate_upper[t,i] <- upper.ci_ATE

  }

  # record CI coverage
  ci_coverage_ATE[i,"coverage"] <- mean((ci_matrix_ate_lower[,i] < ATE) & (ci_matrix_ate_upper[,i] > ATE))

  ci_coverage_ATE[i,"coverage.lower"] <- ci_coverage_ATE[i,"coverage"]-1.96*sqrt(ci_coverage_ATE[i,"coverage"]*(1-ci_coverage_ATE[i,"coverage"])/n)

  ci_coverage_ATE[i,"coverage.upper"] <- ci_coverage_ATE[i,"coverage"]+1.96*sqrt(ci_coverage_ATE[i,"coverage"]*(1-ci_coverage_ATE[i,"coverage"])/n)

  # record average point estimate
  avg.ate[i,"est"] <- mean(est_matrix_ate[,i])

  avg.ate[i,"sd"] <- sqrt(var(est_matrix_ate[,i])/nsim)

  avg.ate[i,"upper"] <- avg.ate[i,"est"]+1.96*sqrt(var(est_matrix_ate[,i])/nsim)

  avg.ate[i,"lower"] <- avg.ate[i,"est"]-1.96*sqrt(var(est_matrix_ate[,i])/nsim)

  # record average bias
  avg.bias_ate[i,"bias"] <- sqrt(n)*mean(bias_matrix_ate[,i])

  avg.bias_ate[i,"upper"] <- avg.bias_ate[i,"bias"]+1.96*sqrt(var(sqrt(n)*bias_matrix_ate[,i])/nsim)

  avg.bias_ate[i,"lower"] <- avg.bias_ate[i,"bias"]-1.96*sqrt(var(sqrt(n)*bias_matrix_ate[,i])/nsim)

  # record var
  avg.variance_ate[i,"variance"] <- n*mean((bias_matrix_ate[,i]-mean(bias_matrix_ate[,i]))^2)

  avg.variance_ate[i,"upper"] <- avg.variance_ate[i,"variance"]+1.96*sqrt(var(n*(bias_matrix_ate[,i]-mean(bias_matrix_ate[,i]))^2)/nsim)

  avg.variance_ate[i,"lower"] <- avg.variance_ate[i,"variance"]-1.96*sqrt(var(n*(bias_matrix_ate[,i]-mean(bias_matrix_ate[,i]))^2)/nsim)

  # record MSE
  avg.MSE_ate[i,"mse"] <- n*mean(bias_matrix_ate[,i]^2)

  avg.MSE_ate[i,"upper"] <- avg.MSE_ate[i,"mse"]+1.96*sqrt(var(n*bias_matrix_ate[,i]^2)/nsim)

  avg.MSE_ate[i,"lower"] <- avg.MSE_ate[i,"mse"]-1.96*sqrt(var(n*bias_matrix_ate[,i]^2)/nsim)

}

# save data
save(list = c("bias_matrix_ate",
              "est_matrix_ate",
              "ci_matrix_ate_lower",
              "ci_matrix_ate_upper",
              "ci_coverage_ATE",
              "avg.ate",
              "avg.bias_ate",
              "avg.variance_ate",
              "avg.MSE_ate"),file = paste0(method, out.name))

