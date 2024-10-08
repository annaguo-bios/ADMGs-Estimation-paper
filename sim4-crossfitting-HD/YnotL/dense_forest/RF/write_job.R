# sample size
n.vec <- c(500,1000,2000)

# number of simulations
nsim <- 1000

dgp.f.name="YnotL-dgp.R" # name for the DGP function
truth="YnotL-truth.Rdata" # name for the truth.Rdata
out.path.tmle= "TMLE-bayes/output/" # path for the output folder
out.path.onestep= "Onestep-bayes/output/" # path for the output folder
a =  "'c(1,0)'"# treatment level
vertices = "'c(\"A\", \"M\", \"L\", \"Y\", \"X\")'" # vertices
di_edges =  "'list(c(\"X\", \"A\"), c(\"X\", \"M\"), c(\"X\", \"L\"), c(\"X\", \"Y\"), c(\"A\", \"M\"), c(\"M\", \"L\"), c(\"L\", \"Y\"), c(\"A\", \"Y\"))'"
# directed edges
bi_edges = "'list(c(\"A\", \"L\"), c(\"M\", \"Y\"))'" # bidirected edges
treatment = "A" # name of the treatment variable
outcome = "Y" # name of the outcome variable
multivariate.variables = "'list(M = c(\"M.1\", \"M.2\"), X=c(\"X.1\",\"X.2\",\"X.3\",\"X.4\",\"X.5\",\"X.6\",\"X.7\",\"X.8\",\"X.9\",\"X.10\"))'"  # name of the multivariate variables
ratio.method.L="bayes" # method for estimating the density ratio associated with M
ratio.method.M="bayes" # method for estimating the density ratio associated with L
lib.seq = "\"c('SL.ranger')\"" # superlearner lib for sequential regression
lib.L = "\"c('SL.ranger')\"" # superlearner lib for L via bayes
lib.M = "\"c('SL.ranger')\"" # superlearner lib for M via bayes
lib.Y = "\"c('SL.ranger')\"" # superlearner lib for Y
lib.A = "\"c('SL.ranger')\"" # superlearner lib for A
superlearner.seq = "T" # whether to use superlearner for sequential regression
superlearner.L = "T" # whether to use superlearner for L
superlearner.M = "T" # whether to use superlearner for M
superlearner.Y = "T" # whether to use superlearner for Y
superlearner.A = "T" # whether to use superlearner for A
crossfit = "F" # number of folds for crossfit
zerodiv.avoid="0"
dnorm.formula.L = "'list(L = \"L ~ M.1 + M.2 + X.1 + X.2 + X.3 + X.4 + X.5 + X.6 + X.7 + X.8 + X.9 + X.10 + I(X.5^2) + I(X.6^2) + I(X.7^2) + I(X.8^2) + I(X.9^2) + I(X.10^2) + A + I(A*X.1)\" )'" # formula for L
dnorm.formula.M = "'list(M.1 = \"M.1 ~ A + X.1 + X.2 + X.3 + X.4 + X.5 + X.6 + X.7 + X.8 + X.9 + X.10 + I(A*X.1) + I(A*X.2) + I(A*X.3) + I(A*X.4) + I(A*X.5)+ I(X.5^2) + I(X.6^2) + I(X.7^2) + I(X.8^2) + I(X.9^2) + I(X.10^2)\", M.2 = \"M.2 ~ A + X.1 + X.2 + X.3 + X.4 + X.5 + X.6 + X.7 + X.8 + X.9 + X.10 + I(A*X.1) + I(A*X.2) + I(A*X.3) + I(A*X.4) + I(A*X.5)+ I(X.5^2) + I(X.6^2) + I(X.7^2) + I(X.8^2) + I(X.9^2) + I(X.10^2)\")'" # formula for M


for (i in seq_along(n.vec)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",n.vec[i]," ",t," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",a," ",
                  vertices," ",di_edges," ",bi_edges," ",treatment," ",outcome," ",
                  multivariate.variables," ",ratio.method.L," ",ratio.method.M," ",lib.seq," ",lib.L," ",lib.M," ",lib.Y," ",lib.A," ",
                  superlearner.seq," ",superlearner.L," ",superlearner.M," ",superlearner.Y," ",superlearner.A," ",crossfit," ", zerodiv.avoid," ",dnorm.formula.L," ", dnorm.formula.M)
    
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("bayes_joblist_n",i,".txt") ,quote = F, col.names = F, row.names = F)
}


out.path.tmle= "TMLE-dnorm/output/" # path for the output folder
out.path.onestep= "Onestep-dnorm/output/" # path for the output folder
ratio.method.L="dnorm" # method for estimating the density ratio associated with M
ratio.method.M="dnorm" # method for estimating the density ratio associated with L

for (i in seq_along(n.vec)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",n.vec[i]," ",t," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",a," ",
                  vertices," ",di_edges," ",bi_edges," ",treatment," ",outcome," ",
                  multivariate.variables," ",ratio.method.L," ",ratio.method.M," ",lib.seq," ",lib.L," ",lib.M," ",lib.Y," ",lib.A," ",
                  superlearner.seq," ",superlearner.L," ",superlearner.M," ",superlearner.Y," ",superlearner.A," ",crossfit," ", zerodiv.avoid," ",dnorm.formula.L," ", dnorm.formula.M)
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("dnorm_joblist_n",i,".txt") ,quote = F, col.names = F, row.names = F)
}


out.path.tmle= "TMLE-densratio/output/" # path for the output folder
out.path.onestep= "Onestep-densratio/output/" # path for the output folder
ratio.method.L="densratio" # method for estimating the density ratio associated with M
ratio.method.M="densratio" # method for estimating the density ratio associated with L

for (i in seq_along(n.vec)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",n.vec[i]," ",t," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",a," ",
                  vertices," ",di_edges," ",bi_edges," ",treatment," ",outcome," ",
                  multivariate.variables," ",ratio.method.L," ",ratio.method.M," ",lib.seq," ",lib.L," ",lib.M," ",lib.Y," ",lib.A," ",
                  superlearner.seq," ",superlearner.L," ",superlearner.M," ",superlearner.Y," ",superlearner.A," ",crossfit," ", zerodiv.avoid," ",dnorm.formula.L," ", dnorm.formula.M)
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("densratio_joblist_n",i,".txt") ,quote = F, col.names = F, row.names = F)
}