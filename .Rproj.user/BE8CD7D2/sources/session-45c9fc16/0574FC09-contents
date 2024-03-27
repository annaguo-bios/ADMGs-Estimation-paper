# sample size
n.vec <- c(250,500,1000,2000,4000,8000)

# number of simulations
nsim <- 1000

dgp.f.name="../DGPs/1-dgp-binary.R" # name for the DGP function
truth= "../DGPs/1-truth-binary.Rdata" # path+name for the truth.Rdata
out.path.tmle= "TMLE-est1/output/" # path for the output folder
out.path.onestep= "Onestep-est1/output/" # path for the output folder
mediator.method="bayes"
superlearner="F"
crossfit="F"
K="5"
treatment="A"
mediators="'c(\"M\")'"
outcome="Y"
covariates="'c(\"X\")'"
lib="\"c('SL.ranger')\""
linkA="identity"


dgp.f.name="YinL-dgp.R" # name for the DGP function
truth="YinL-truth.Rdata" # name for the truth.Rdata
out.path.tmle= "TMLE/output" # path for the output folder
out.path.onestep= "Onestep/output" # path for the output folder
a =  "'c(1,0)'"# treatment level
vertices = "'c(\"A\", \"M\", \"L\", \"Y\", \"X\")'" # vertices
di_edges =  "'list(c(\"X\", \"A\"), c(\"X\", \"M\"), c(\"X\", \"L\"), c(\"X\", \"Y\"), c(\"M\", \"Y\"), c(\"A\", \"M\"), c(\"A\", \"L\"), c(\"M\", \"L\"), c(\"L\", \"Y\"))'" # directed edges
bi_edges = "'list(c(\"A\", \"Y\"))'" # bidirected edges
treatment = "A" # name of the treatment variable
outcome = "Y" # name of the outcome variable
multivariate.variables = "'list(M = c(\"M1\", \"M2\"))'"  # name of the multivariate variables
ratio.method.L="bayes" # method for estimating the density ratio associated with M
ratio.method.M="bayes" # method for estimating the density ratio associated with L

for (i in seq_along(n.vec)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",n.vec[i]," ",t," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",a," ",
                  vertices," ",di_edges," ",bi_edges," ",treatment," ",outcome," ",
                  multivariate.variables," ",ratio.method.L," ",ratio.method.M)
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("joblist_n",i,".txt"),quote = F, col.names = F, row.names = F)
}
