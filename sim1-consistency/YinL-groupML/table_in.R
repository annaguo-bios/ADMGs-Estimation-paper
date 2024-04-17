setwd("/Users/apple/Library/CloudStorage/Dropbox/primal-fixability/code/Simulations/sim1-consistency/")
# sample size
n= c(500,1000,2000,8000)
n.vec <- c(500,1000,2000,8000)
n.ind <- which(n.vec %in% n)

# round the numbers
decimal <- 3

######################################################################
##############  Simulation 1: Consistency  ##############
######################################################################
objs <- c("ci_coverage_ATE","avg.MSE_ate","bias_matrix_ate","ci_matrix_ate_lower","ci_matrix_ate_upper")


## TMLE-M&L
load("YinL/TMLE-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.MnL.tmle.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YinL/TMLE-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.MnL.tmle.densratio_", obj)
  assign(new_name, get(obj))
}

load("YinL/TMLE-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.MnL.tmle.bayes_", obj)
  assign(new_name, get(obj))
}


## Onestep-M&L
load("YinL/Onestep-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.MnL.Onestep.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YinL/Onestep-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.MnL.Onestep.densratio_", obj)
  assign(new_name, get(obj))
}

load("YinL/Onestep-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.MnL.Onestep.bayes_", obj)
  assign(new_name, get(obj))
}


## TMLE-ML
load("YinL-groupML/TMLE-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.ML.tmle.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YinL-groupML/TMLE-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.ML.tmle.densratio_", obj)
  assign(new_name, get(obj))
}

load("YinL-groupML/TMLE-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.ML.tmle.bayes_", obj)
  assign(new_name, get(obj))
}


## Onestep-ML
load("YinL-groupML/Onestep-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.ML.Onestep.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YinL-groupML/Onestep-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.ML.Onestep.densratio_", obj)
  assign(new_name, get(obj))
}

load("YinL-groupML/Onestep-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.ML.Onestep.bayes_", obj)
  assign(new_name, get(obj))
}


# MnL, TMLE, dnorm
YinL.MnL.tmle.dnorm <- cbind(colMeans(YinL.MnL.tmle.dnorm_bias_matrix_ate[,n.ind]),apply(YinL.MnL.tmle.dnorm_bias_matrix_ate[,n.ind],2,sd),YinL.MnL.tmle.dnorm_avg.MSE_ate[n.ind,2]/n,YinL.MnL.tmle.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YinL.MnL.tmle.dnorm_ci_matrix_ate_upper-YinL.MnL.tmle.dnorm_ci_matrix_ate_lower)[,n.ind]))

# MnL, TMLE, densratio
YinL.MnL.tmle.densratio <- cbind(colMeans(YinL.MnL.tmle.densratio_bias_matrix_ate[,n.ind]),apply(YinL.MnL.tmle.densratio_bias_matrix_ate[,n.ind],2,sd),YinL.MnL.tmle.densratio_avg.MSE_ate[n.ind,2]/n,YinL.MnL.tmle.densratio_ci_coverage_ATE[n.ind,2],colMeans((YinL.MnL.tmle.densratio_ci_matrix_ate_upper-YinL.MnL.tmle.densratio_ci_matrix_ate_lower)[,n.ind]))

# MnL, TMLE, bayes
YinL.MnL.tmle.bayes <- cbind(colMeans(YinL.MnL.tmle.bayes_bias_matrix_ate[,n.ind]),apply(YinL.MnL.tmle.bayes_bias_matrix_ate[,n.ind],2,sd),YinL.MnL.tmle.bayes_avg.MSE_ate[n.ind,2]/n,YinL.MnL.tmle.bayes_ci_coverage_ATE[n.ind,2],colMeans((YinL.MnL.tmle.bayes_ci_matrix_ate_upper-YinL.MnL.tmle.bayes_ci_matrix_ate_lower)[,n.ind]))

# MnL, Onestep, dnorm
YinL.MnL.Onestep.dnorm <- cbind(colMeans(YinL.MnL.Onestep.dnorm_bias_matrix_ate[,n.ind]),apply(YinL.MnL.Onestep.dnorm_bias_matrix_ate[,n.ind],2,sd),YinL.MnL.Onestep.dnorm_avg.MSE_ate[n.ind,2]/n,YinL.MnL.Onestep.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YinL.MnL.Onestep.dnorm_ci_matrix_ate_upper-YinL.MnL.Onestep.dnorm_ci_matrix_ate_lower)[,n.ind]))

# MnL, Onestep, densratio
YinL.MnL.Onestep.densratio <- cbind(colMeans(YinL.MnL.Onestep.densratio_bias_matrix_ate[,n.ind]),apply(YinL.MnL.Onestep.densratio_bias_matrix_ate[,n.ind],2,sd),YinL.MnL.Onestep.densratio_avg.MSE_ate[n.ind,2]/n,YinL.MnL.Onestep.densratio_ci_coverage_ATE[n.ind,2],colMeans((YinL.MnL.Onestep.densratio_ci_matrix_ate_upper-YinL.MnL.Onestep.densratio_ci_matrix_ate_lower)[,n.ind]))

# MnL, Onestep, bayes
YinL.MnL.Onestep.bayes <- cbind(colMeans(YinL.MnL.Onestep.bayes_bias_matrix_ate[,n.ind]),apply(YinL.MnL.Onestep.bayes_bias_matrix_ate[,n.ind],2,sd),YinL.MnL.Onestep.bayes_avg.MSE_ate[n.ind,2]/n,YinL.MnL.Onestep.bayes_ci_coverage_ATE[n.ind,2],colMeans((YinL.MnL.Onestep.bayes_ci_matrix_ate_upper-YinL.MnL.Onestep.bayes_ci_matrix_ate_lower)[,n.ind]))

# ML, TMLE, dnorm
YinL.ML.tmle.dnorm <- cbind(colMeans(YinL.ML.tmle.dnorm_bias_matrix_ate[,n.ind]),apply(YinL.ML.tmle.dnorm_bias_matrix_ate[,n.ind],2,sd),YinL.ML.tmle.dnorm_avg.MSE_ate[n.ind,2]/n,YinL.ML.tmle.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YinL.ML.tmle.dnorm_ci_matrix_ate_upper-YinL.ML.tmle.dnorm_ci_matrix_ate_lower)[,n.ind]))

# ML, TMLE, densratio
YinL.ML.tmle.densratio <- cbind(colMeans(YinL.ML.tmle.densratio_bias_matrix_ate[,n.ind]),apply(YinL.ML.tmle.densratio_bias_matrix_ate[,n.ind],2,sd),YinL.ML.tmle.densratio_avg.MSE_ate[n.ind,2]/n,YinL.ML.tmle.densratio_ci_coverage_ATE[n.ind,2],colMeans((YinL.ML.tmle.densratio_ci_matrix_ate_upper-YinL.ML.tmle.densratio_ci_matrix_ate_lower)[,n.ind]))

# ML, TMLE, bayes
YinL.ML.tmle.bayes <- cbind(colMeans(YinL.ML.tmle.bayes_bias_matrix_ate[,n.ind]),apply(YinL.ML.tmle.bayes_bias_matrix_ate[,n.ind],2,sd),YinL.ML.tmle.bayes_avg.MSE_ate[n.ind,2]/n,YinL.ML.tmle.bayes_ci_coverage_ATE[n.ind,2],colMeans((YinL.ML.tmle.bayes_ci_matrix_ate_upper-YinL.ML.tmle.bayes_ci_matrix_ate_lower)[,n.ind]))

# ML, Onestep, dnorm
YinL.ML.Onestep.dnorm <- cbind(colMeans(YinL.ML.Onestep.dnorm_bias_matrix_ate[,n.ind]),apply(YinL.ML.Onestep.dnorm_bias_matrix_ate[,n.ind],2,sd),YinL.ML.Onestep.dnorm_avg.MSE_ate[n.ind,2]/n,YinL.ML.Onestep.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YinL.ML.Onestep.dnorm_ci_matrix_ate_upper-YinL.ML.Onestep.dnorm_ci_matrix_ate_lower)[,n.ind]))

# ML, Onestep, densratio
YinL.ML.Onestep.densratio <- cbind(colMeans(YinL.ML.Onestep.densratio_bias_matrix_ate[,n.ind]),apply(YinL.ML.Onestep.densratio_bias_matrix_ate[,n.ind],2,sd),YinL.ML.Onestep.densratio_avg.MSE_ate[n.ind,2]/n,YinL.ML.Onestep.densratio_ci_coverage_ATE[n.ind,2],colMeans((YinL.ML.Onestep.densratio_ci_matrix_ate_upper-YinL.ML.Onestep.densratio_ci_matrix_ate_lower)[,n.ind]))

# ML, Onestep, bayes
YinL.ML.Onestep.bayes <- cbind(colMeans(YinL.ML.Onestep.bayes_bias_matrix_ate[,n.ind]),apply(YinL.ML.Onestep.bayes_bias_matrix_ate[,n.ind],2,sd),YinL.ML.Onestep.bayes_avg.MSE_ate[n.ind,2]/n,YinL.ML.Onestep.bayes_ci_coverage_ATE[n.ind,2],colMeans((YinL.ML.Onestep.bayes_ci_matrix_ate_upper-YinL.ML.Onestep.bayes_ci_matrix_ate_lower)[,n.ind]))


dat <- list(YinL.MnL.tmle.dnorm, YinL.ML.tmle.dnorm, 
            YinL.MnL.tmle.densratio, YinL.ML.tmle.densratio, 
            YinL.MnL.tmle.bayes, YinL.ML.tmle.bayes,
            YinL.MnL.Onestep.dnorm, YinL.ML.Onestep.dnorm, 
            YinL.MnL.Onestep.densratio, YinL.ML.Onestep.densratio,
            YinL.MnL.Onestep.bayes, YinL.ML.Onestep.bayes)

for (i in seq_along(n)) {
  # Extract the column from each data frame and cbind
  columns <- lapply(dat,function(x) x[i,])  # Extract the first column from each data frame
  assign(paste0("dt",i),round(do.call(cbind, columns),decimal))   # Combine the columns using cbind
  
  # turn df into character table to keep decimal place 
  df <- get(paste0("dt",i))  # Get the data frame by name
  df.char <- trimws(format(df,nsmall=decimal))
  
  df[4, ] <- paste0(df[4, ] * 100, "%")  # Modify the value in the data frame
  
  df.char[4,] <- df[4,]
  assign(paste0("dt",i), df.char)  # Update the data frame with the modified value
  
}

dat <- rbind(dt1,dt2,dt3, dt4)

# make table
library(huxtable)
library(dplyr)
options(scipen = 999)

table1 <- as_hux(dat) %>% 
  insert_row("TMLEs","","","","","","One-step estimators","","","","","", after = 0) %>% 
  merge_cells(1, 1:6) %>% 
  merge_cells(1, 7:12) %>% 
  insert_row("\\(\\psi_{dnorm}(\\hat{Q}^\\star)\\)","","\\(\\psi_{densratio}(\\hat{Q}^\\star)\\)","","\\(\\psi_{bayes}(\\hat{Q}^\\star)\\)","",
             "\\(\\psi^{+}_{dnorm}(\\hat{Q})\\)","","\\(\\psi^{+}_{densratio}(\\hat{Q})\\)","","\\(\\psi^{+}_{bayes}(\\hat{Q})\\)","", after = 1) %>% 
  merge_cells(2, 1:2) %>%
  merge_cells(2, 3:4) %>%
  merge_cells(2, 5:6) %>%
  merge_cells(2, 7:8) %>%
  merge_cells(2, 9:10) %>%
  merge_cells(2, 11:12) %>%
  insert_row("M+L","ML","M+L","ML","M+L","ML","M+L","ML","M+L","ML","M+L","ML", after = 2) %>% 
  insert_row("","","","","","","","","","","","", after = 3) %>% 
  insert_row("","","","","","","","","","","","", after = 9) %>% 
  insert_row("","","","","","","","","","","","", after = 15) %>% 
  insert_row("","","","","","","","","","","","", after = 21) %>%
  insert_column(c("","","","n=500","Bias","SD","MSE","CI coverage","CI width","n=1000","Bias","SD","MSE","CI coverage","CI width","n=2000","Bias","SD","MSE","CI coverage","CI width",
                  "n=8000","Bias","SD","MSE","CI coverage","CI width"), after = 0) %>%
  set_escape_contents(3, 1:ncol(.), FALSE) %>%
  set_align(col=1, everywhere, "left") %>%
  set_align(col=2:ncol(.),everywhere,"center") %>%
  set_all_padding(-1)  %>% 
  set_bold(1, everywhere) %>%
  set_bold(c(4,10,16,22), everywhere) %>%
  set_italic(2,everywhere) %>%
  set_bottom_border(row = 1, col =2:ncol(.)) %>% 
  set_bottom_border(row = 3, col =2:ncol(.)) %>% 
  set_bottom_border(row = 2, col =2:ncol(.)) %>% 
  set_right_border(4:nrow(.), 3, brdr(0.4,"solid")) %>%
  set_right_border(4:nrow(.), 5, brdr(0.4,"solid")) %>%
  set_right_border(4:nrow(.), 7, brdr(0.4,"double")) %>%
  set_right_border(4:nrow(.), 9, brdr(0.4,"solid")) %>%
  set_right_border(4:nrow(.), 11, brdr(0.4,"solid")) %>%
  set_top_border(row=1,col=everywhere,brdr(1, "solid")) %>% set_bottom_border(row = nrow(.),col = everywhere,brdr(1, "solid")) %>%
  set_font_size(7) %>% set_escape_contents(2, 1:ncol(.), FALSE) %>%  set_caption("Comparative analysis of TMLEs and one-step estimators when variables M and L are combined verse not.") 

table1
quick_latex(table1, file = "table.tex")
y
