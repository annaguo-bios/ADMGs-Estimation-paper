setwd("/Users/apple/Library/CloudStorage/Dropbox/primal-fixability/code/Simulations/sim4-crossfitting/YinL/sparse_forest")
# sample size
n= c(500,1000,2000)
n.vec <- c(500,1000,2000)
n.ind <- which(n.vec %in% n)

# round the numbers
decimal <- 3

######################################################################
############## Simulation 3: cross fitting - ranger ##############
######################################################################
objs <- c("ci_coverage_ATE","avg.MSE_ate","bias_matrix_ate","ci_matrix_ate_lower","ci_matrix_ate_upper")


load("RF/Onestep-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("Onestep.bayes.SL_", obj)
  assign(new_name, get(obj))
}

load("RF/Onestep-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("Onestep.dnorm.SL_", obj)
  assign(new_name, get(obj))
}

load("RF/Onestep-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("Onestep.densratio.SL_", obj)
  assign(new_name, get(obj))
}

load("RF/TMLE-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("TMLE.bayes.SL_", obj)
  assign(new_name, get(obj))
}

load("RF/TMLE-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("TMLE.dnorm.SL_", obj)
  assign(new_name, get(obj))
}

load("RF/TMLE-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("TMLE.densratio.SL_", obj)
  assign(new_name, get(obj))
}


load("CF/Onestep-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("Onestep.bayes.CF_", obj)
  assign(new_name, get(obj))
}

load("CF/Onestep-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("Onestep.dnorm.CF_", obj)
  assign(new_name, get(obj))
}

load("CF/Onestep-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("Onestep.densratio.CF_", obj)
  assign(new_name, get(obj))
}

load("CF/TMLE-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("TMLE.bayes.CF_", obj)
  assign(new_name, get(obj))
}

load("CF/TMLE-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("TMLE.dnorm.CF_", obj)
  assign(new_name, get(obj))
}

load("CF/TMLE-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("TMLE.densratio.CF_", obj)
  assign(new_name, get(obj))
}

Onestep.bayes.RF <- cbind(colMeans(Onestep.bayes.SL_bias_matrix_ate[,n.ind]),apply(Onestep.bayes.SL_bias_matrix_ate[,n.ind],2,sd),Onestep.bayes.SL_avg.MSE_ate[n.ind,2]/n,Onestep.bayes.SL_ci_coverage_ATE[n.ind,2],colMeans((Onestep.bayes.SL_ci_matrix_ate_upper-Onestep.bayes.SL_ci_matrix_ate_lower)[,n.ind]))
Onestep.dnorm.RF <- cbind(colMeans(Onestep.dnorm.SL_bias_matrix_ate[,n.ind]),apply(Onestep.dnorm.SL_bias_matrix_ate[,n.ind],2,sd),Onestep.dnorm.SL_avg.MSE_ate[n.ind,2]/n,Onestep.dnorm.SL_ci_coverage_ATE[n.ind,2],colMeans((Onestep.dnorm.SL_ci_matrix_ate_upper-Onestep.dnorm.SL_ci_matrix_ate_lower)[,n.ind]))
Onestep.densratio.RF <- cbind(colMeans(Onestep.densratio.SL_bias_matrix_ate[,n.ind]),apply(Onestep.densratio.SL_bias_matrix_ate[,n.ind],2,sd),Onestep.densratio.SL_avg.MSE_ate[n.ind,2]/n,Onestep.densratio.SL_ci_coverage_ATE[n.ind,2],colMeans((Onestep.densratio.SL_ci_matrix_ate_upper-Onestep.densratio.SL_ci_matrix_ate_lower)[,n.ind]))

TMLE.bayes.RF <- cbind(colMeans(TMLE.bayes.SL_bias_matrix_ate[,n.ind]),apply(TMLE.bayes.SL_bias_matrix_ate[,n.ind],2,sd),TMLE.bayes.SL_avg.MSE_ate[n.ind,2]/n,TMLE.bayes.SL_ci_coverage_ATE[n.ind,2],colMeans((TMLE.bayes.SL_ci_matrix_ate_upper-TMLE.bayes.SL_ci_matrix_ate_lower)[,n.ind]))
TMLE.dnorm.RF <- cbind(colMeans(TMLE.dnorm.SL_bias_matrix_ate[,n.ind]),apply(TMLE.dnorm.SL_bias_matrix_ate[,n.ind],2,sd),TMLE.dnorm.SL_avg.MSE_ate[n.ind,2]/n,TMLE.dnorm.SL_ci_coverage_ATE[n.ind,2],colMeans((TMLE.dnorm.SL_ci_matrix_ate_upper-TMLE.dnorm.SL_ci_matrix_ate_lower)[,n.ind]))
TMLE.densratio.RF <- cbind(colMeans(TMLE.densratio.SL_bias_matrix_ate[,n.ind]),apply(TMLE.densratio.SL_bias_matrix_ate[,n.ind],2,sd),TMLE.densratio.SL_avg.MSE_ate[n.ind,2]/n,TMLE.densratio.SL_ci_coverage_ATE[n.ind,2],colMeans((TMLE.densratio.SL_ci_matrix_ate_upper-TMLE.densratio.SL_ci_matrix_ate_lower)[,n.ind]))

Onestep.bayes.CF <- cbind(colMeans(Onestep.bayes.CF_bias_matrix_ate[,n.ind]),apply(Onestep.bayes.CF_bias_matrix_ate[,n.ind],2,sd),Onestep.bayes.CF_avg.MSE_ate[n.ind,2]/n,Onestep.bayes.CF_ci_coverage_ATE[n.ind,2],colMeans((Onestep.bayes.CF_ci_matrix_ate_upper-Onestep.bayes.CF_ci_matrix_ate_lower)[,n.ind]))
Onestep.dnorm.CF <- cbind(colMeans(Onestep.dnorm.CF_bias_matrix_ate[,n.ind]),apply(Onestep.dnorm.CF_bias_matrix_ate[,n.ind],2,sd),Onestep.dnorm.CF_avg.MSE_ate[n.ind,2]/n,Onestep.dnorm.CF_ci_coverage_ATE[n.ind,2],colMeans((Onestep.dnorm.CF_ci_matrix_ate_upper-Onestep.dnorm.CF_ci_matrix_ate_lower)[,n.ind]))
Onestep.densratio.CF <- cbind(colMeans(Onestep.densratio.CF_bias_matrix_ate[,n.ind]),apply(Onestep.densratio.CF_bias_matrix_ate[,n.ind],2,sd),Onestep.densratio.CF_avg.MSE_ate[n.ind,2]/n,Onestep.densratio.CF_ci_coverage_ATE[n.ind,2],colMeans((Onestep.densratio.CF_ci_matrix_ate_upper-Onestep.densratio.CF_ci_matrix_ate_lower)[,n.ind]))

TMLE.bayes.CF <- cbind(colMeans(TMLE.bayes.CF_bias_matrix_ate[,n.ind]),apply(TMLE.bayes.CF_bias_matrix_ate[,n.ind],2,sd),TMLE.bayes.CF_avg.MSE_ate[n.ind,2]/n,TMLE.bayes.CF_ci_coverage_ATE[n.ind,2],colMeans((TMLE.bayes.CF_ci_matrix_ate_upper-TMLE.bayes.CF_ci_matrix_ate_lower)[,n.ind]))
TMLE.dnorm.CF <- cbind(colMeans(TMLE.dnorm.CF_bias_matrix_ate[,n.ind]),apply(TMLE.dnorm.CF_bias_matrix_ate[,n.ind],2,sd),TMLE.dnorm.CF_avg.MSE_ate[n.ind,2]/n,TMLE.dnorm.CF_ci_coverage_ATE[n.ind,2],colMeans((TMLE.dnorm.CF_ci_matrix_ate_upper-TMLE.dnorm.CF_ci_matrix_ate_lower)[,n.ind]))
TMLE.densratio.CF <- cbind(colMeans(TMLE.densratio.CF_bias_matrix_ate[,n.ind]),apply(TMLE.densratio.CF_bias_matrix_ate[,n.ind],2,sd),TMLE.densratio.CF_avg.MSE_ate[n.ind,2]/n,TMLE.densratio.CF_ci_coverage_ATE[n.ind,2],colMeans((TMLE.densratio.CF_ci_matrix_ate_upper-TMLE.densratio.CF_ci_matrix_ate_lower)[,n.ind]))


dat <- list(TMLE.dnorm.RF,TMLE.dnorm.CF, TMLE.densratio.RF, TMLE.densratio.CF, TMLE.bayes.RF, TMLE.bayes.CF, Onestep.dnorm.RF, Onestep.dnorm.CF, Onestep.densratio.RF, Onestep.densratio.CF, Onestep.bayes.RF, Onestep.bayes.CF)


for (i in seq_along(n)) {
  # Extract the column from each data frame and cbind
  columns <- lapply(dat,function(x) x[i,])  # Extract the first column from each data frame
  assign(paste0("dt",i),round(do.call(cbind, columns),decimal))   # Combine the columns using cbind

  df <- get(paste0("dt",i))  # Get the data frame by name
  df.char <- trimws(format(df,nsmall=decimal))
  
  df[4, ] <- paste0(df[4, ] * 100, "%")  # Modify the value in the data frame
  
  df.char[4,] <- df[4,]
  assign(paste0("dt",i), df.char)  # Update the data frame with the modified value
  
}

dat <- rbind(dt1,dt2,dt3)

# make table
library(huxtable)
library(dplyr)
options(scipen = 999)

table1 <- as_hux(dat) %>% 
  insert_row("TMLEs","","","","","","One-step estimators","","","","","", after = 0) %>% 
  merge_cells(1, 1:6) %>% 
  merge_cells(1, 7:12) %>% 
  insert_row("\\(\\psi_{dnorm}(\\hat{Q}^\\star)\\)","","\\(\\psi_{densratio}(\\hat{Q}^\\star)\\)","","\\(\\psi_{bayes}(\\hat{Q}^\\star)\\)","","\\(\\psi_{dnorm}^{+}(\\hat{Q})\\)","","\\(\\psi_{densratio}^{+}(\\hat{Q})\\)","","\\(\\psi_{bayes}^{+}(\\hat{Q})\\)","", after = 1) %>% 
  insert_row("RF","CF","RF","CF","RF","CF","RF","CF","RF","CF","RF","CF", after = 2) %>% 
  merge_cells(2, 1:2) %>%
  merge_cells(2, 3:4) %>%
  merge_cells(2, 5:6) %>%
  merge_cells(2, 7:8) %>%
  merge_cells(2, 9:10) %>%
  merge_cells(2, 11:12) %>%
  insert_row("","","","","","","","","","","","", after = 3) %>% 
  insert_row("","","","","","","","","","","","", after = 9) %>% 
  insert_row("","","","","","","","","","","","", after = 15) %>% 
  insert_column(c("","","","n=500","Bias","SD","MSE","CI coverage","CI width","n=1000","Bias","SD","MSE","CI coverage","CI width","n=2000","Bias","SD","MSE","CI coverage","CI width"), after = 0) %>%
  set_escape_contents(2, 1:ncol(.), FALSE) %>%
  set_align(col=1, everywhere, "left") %>%
  set_align(col=2:ncol(.),everywhere,"center") %>%
  set_all_padding(0)  %>% 
  set_bold(1, everywhere) %>%
  set_bold(c(4,10,16), everywhere) %>%
  set_italic(2,everywhere) %>%
  set_bottom_border(row = 1, col =2:ncol(.)) %>% 
  set_bottom_border(row = 3, col =2:ncol(.)) %>% 
  set_bottom_border(row = 2, col =2:ncol(.)) %>% 
  set_right_border(4:nrow(.), 3, brdr(0.6,"solid")) %>%
  set_right_border(4:nrow(.), 5, brdr(0.4,"solid")) %>%
  set_right_border(4:nrow(.), 7, brdr(0.6,"double")) %>%
  set_right_border(4:nrow(.), 9, brdr(0.6,"solid")) %>%
  set_right_border(4:nrow(.), 11, brdr(0.4,"solid")) %>%
  set_top_border(row=1,col=everywhere,brdr(1, "solid")) %>% set_bottom_border(row = nrow(.),col = everywhere,brdr(1, "solid")) %>%
  set_font_size(6.5) %>% set_escape_contents(nrow(.), 1:ncol(.), FALSE) %>% 
  set_caption("Comparative analysis for the impact of cross-fitting on TMLEs and one-step estimators in conjunction with the use of random forests.
RF refers to random forest with 200 trees and a minimum node size of 1, and CF denotes random forest with cross fitting using 5 folds.") %>% set_col_width(1/14*c(2,rep(1,12)))

table1
quick_latex(table1, file="table-in-dense.tex")
y
