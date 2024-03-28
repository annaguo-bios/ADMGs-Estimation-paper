# sample size
n.vec <- c(250,500,1000,2000,4000,8000)

# number of simulations
nsim <- 1000

dgp.f.name="YnotL-dgp.R" # name for the DGP function
truth="YnotL-truth.Rdata" # name for the truth.Rdata
out.path.tmle= "TMLE/output/" # path for the output folder
out.path.onestep= "Onestep/output/" # path for the output folder
a =  "'c(1,0)'"# treatment level
vertices = "'c(\"A\", \"M\", \"L\", \"Y\", \"X\")'" # vertices
di_edges =  "'list(c(\"X\", \"A\"), c(\"X\", \"M\"), c(\"X\", \"L\"), c(\"X\", \"Y\"), c(\"A\", \"M\"), c(\"M\", \"L\"), c(\"L\", \"Y\"), c(\"A\", \"Y\"))'"
 # directed edges
bi_edges = "'list(c(\"A\", \"L\"), c(\"M\", \"Y\"))'" # bidirected edges
treatment = "A" # name of the treatment variable
outcome = "Y" # name of the outcome variable
multivariate.variables = "'list(M = c(\"M.1\", \"M.2\"))'"  # name of the multivariate variables
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
  write.table(joblist, file = paste0("joblist_n",i,".txt") ,quote = F, col.names = F, row.names = F)
}
