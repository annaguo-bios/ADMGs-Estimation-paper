# sample size
n.vec <- c(250,500,1000,2000,4000,8000)

# number of simulations
nsim <- 1000

dgp.f.name="YinL-dgp.R" # name for the DGP function
truth="YinL-truth.Rdata" # name for the truth.Rdata
out.path.tmle= "TMLE-bayes/output/" # path for the output folder
out.path.onestep= "Onestep-bayes/output/" # path for the output folder
a =  "'c(1,0)'"# treatment level
vertices = "'c(\"A\", \"ML\", \"Y\", \"X\")'" # vertices
di_edges =  "'list(c(\"X\", \"A\"), c(\"X\", \"ML\"),  c(\"X\", \"Y\"),  c(\"A\", \"ML\"), c(\"ML\", \"Y\"))'" # directed edges
bi_edges = "'list(c(\"A\", \"Y\"))'" # bidirected edges
treatment = "A" # name of the treatment variable
outcome = "Y" # name of the outcome variable
multivariate.variables = "'list(ML = c(\"M.1\", \"M.2\", \"L\"))'"  # name of the multivariate variables
ratio.method.L="bayes" # method for estimating the density ratio associated with M
ratio.method.M="bayes" # method for estimating the density ratio associated with L
# superlearner.L = "T"
# superlearner.M = "T"
# lib.L = "\"c('SL.glm','SL.bayesglm', 'SL.gam','SL.earth','SL.ranger','SL.svm','SL.xgboost','SL.mean')\"" # superlearner lib for L via bayes
# lib.M = "\"c('SL.glm','SL.bayesglm', 'SL.gam','SL.earth','SL.ranger','SL.svm','SL.xgboost','SL.mean')\"" # superlearner lib for M via bayes

for (i in seq_along(n.vec)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",n.vec[i]," ",t," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",a," ",
                  vertices," ",di_edges," ",bi_edges," ",treatment," ",outcome," ",
                  multivariate.variables," ",ratio.method.L," ",ratio.method.M) # , " ", superlearner.L, " ", superlearner.M," ",lib.L," ",lib.M
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
                  multivariate.variables," ",ratio.method.L," ",ratio.method.M)
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
                  multivariate.variables," ",ratio.method.L," ",ratio.method.M)
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("densratio_joblist_n",i,".txt") ,quote = F, col.names = F, row.names = F)
}