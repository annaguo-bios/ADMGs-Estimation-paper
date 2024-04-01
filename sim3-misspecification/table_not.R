setwd("/Users/apple/Library/CloudStorage/Dropbox/primal-fixability/code/Simulations/sim3-misspecification")
# sample size
n= c(500,1000,2000)
n.vec <- c(500,1000,2000)
n.ind <- which(n.vec %in% n)

# round the numbers
decimal <- 3

######################################################################
##############  Simulation 2: Complex DGP  ##############
######################################################################
objs <- c("ci_coverage_ATE","avg.MSE_ate","bias_matrix_ate","ci_matrix_ate_lower","ci_matrix_ate_upper")


## TMLE-Linear
load("YnotL/Linear/TMLE-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.linear.tmle.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YnotL/Linear/TMLE-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.linear.tmle.densratio_", obj)
  assign(new_name, get(obj))
}

load("YnotL/Linear/TMLE-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.linear.tmle.bayes_", obj)
  assign(new_name, get(obj))
}

## TMLE-SL
load("YnotL/SL/TMLE-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.SL.tmle.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YnotL/SL/TMLE-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.SL.tmle.densratio_", obj)
  assign(new_name, get(obj))
}

load("YnotL/SL/TMLE-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.SL.tmle.bayes_", obj)
  assign(new_name, get(obj))
}

## TMLE-CF
load("YnotL/CF/TMLE-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.CF.tmle.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YnotL/CF/TMLE-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.CF.tmle.densratio_", obj)
  assign(new_name, get(obj))
}

load("YnotL/CF/TMLE-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.CF.tmle.bayes_", obj)
  assign(new_name, get(obj))
}

## Onestep-Linear
load("YnotL/Linear/Onestep-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.linear.Onestep.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YnotL/Linear/Onestep-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.linear.Onestep.densratio_", obj)
  assign(new_name, get(obj))
}

load("YnotL/Linear/Onestep-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.linear.Onestep.bayes_", obj)
  assign(new_name, get(obj))
}

## Onestep-SL
load("YnotL/SL/Onestep-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.SL.Onestep.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YnotL/SL/Onestep-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.SL.Onestep.densratio_", obj)
  assign(new_name, get(obj))
}

load("YnotL/SL/Onestep-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.SL.Onestep.bayes_", obj)
  assign(new_name, get(obj))
}

## Onestep-CF
load("YnotL/CF/Onestep-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.CF.Onestep.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YnotL/CF/Onestep-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.CF.Onestep.densratio_", obj)
  assign(new_name, get(obj))
}

load("YnotL/CF/Onestep-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YnotL.CF.Onestep.bayes_", obj)
  assign(new_name, get(obj))
}


# Linear, TMLE, dnorm
YnotL.linear.tmle.dnorm <- cbind(colMeans(YnotL.linear.tmle.dnorm_bias_matrix_ate[,n.ind]),apply(YnotL.linear.tmle.dnorm_bias_matrix_ate[,n.ind],2,sd),YnotL.linear.tmle.dnorm_avg.MSE_ate[n.ind,2]/n,YnotL.linear.tmle.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YnotL.linear.tmle.dnorm_ci_matrix_ate_upper-YnotL.linear.tmle.dnorm_ci_matrix_ate_lower)[,n.ind]))

# Linear, TMLE, densratio
YnotL.linear.tmle.densratio <- cbind(colMeans(YnotL.linear.tmle.densratio_bias_matrix_ate[,n.ind]),apply(YnotL.linear.tmle.densratio_bias_matrix_ate[,n.ind],2,sd),YnotL.linear.tmle.densratio_avg.MSE_ate[n.ind,2]/n,YnotL.linear.tmle.densratio_ci_coverage_ATE[n.ind,2],colMeans((YnotL.linear.tmle.densratio_ci_matrix_ate_upper-YnotL.linear.tmle.densratio_ci_matrix_ate_lower)[,n.ind]))

# Linear, TMLE, bayes
YnotL.linear.tmle.bayes <- cbind(colMeans(YnotL.linear.tmle.bayes_bias_matrix_ate[,n.ind]),apply(YnotL.linear.tmle.bayes_bias_matrix_ate[,n.ind],2,sd),YnotL.linear.tmle.bayes_avg.MSE_ate[n.ind,2]/n,YnotL.linear.tmle.bayes_ci_coverage_ATE[n.ind,2],colMeans((YnotL.linear.tmle.bayes_ci_matrix_ate_upper-YnotL.linear.tmle.bayes_ci_matrix_ate_lower)[,n.ind]))

# SL, TMLE, dnorm
YnotL.SL.tmle.dnorm <- cbind(colMeans(YnotL.SL.tmle.dnorm_bias_matrix_ate[,n.ind]),apply(YnotL.SL.tmle.dnorm_bias_matrix_ate[,n.ind],2,sd),YnotL.SL.tmle.dnorm_avg.MSE_ate[n.ind,2]/n,YnotL.SL.tmle.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YnotL.SL.tmle.dnorm_ci_matrix_ate_upper-YnotL.SL.tmle.dnorm_ci_matrix_ate_lower)[,n.ind]))

# SL, TMLE, densratio
YnotL.SL.tmle.densratio <- cbind(colMeans(YnotL.SL.tmle.densratio_bias_matrix_ate[,n.ind]),apply(YnotL.SL.tmle.densratio_bias_matrix_ate[,n.ind],2,sd),YnotL.SL.tmle.densratio_avg.MSE_ate[n.ind,2]/n,YnotL.SL.tmle.densratio_ci_coverage_ATE[n.ind,2],colMeans((YnotL.SL.tmle.densratio_ci_matrix_ate_upper-YnotL.SL.tmle.densratio_ci_matrix_ate_lower)[,n.ind]))

# SL, TMLE, bayes
YnotL.SL.tmle.bayes <- cbind(colMeans(YnotL.SL.tmle.bayes_bias_matrix_ate[,n.ind]),apply(YnotL.SL.tmle.bayes_bias_matrix_ate[,n.ind],2,sd),YnotL.SL.tmle.bayes_avg.MSE_ate[n.ind,2]/n,YnotL.SL.tmle.bayes_ci_coverage_ATE[n.ind,2],colMeans((YnotL.SL.tmle.bayes_ci_matrix_ate_upper-YnotL.SL.tmle.bayes_ci_matrix_ate_lower)[,n.ind]))

# CF, TMLE, dnorm
YnotL.CF.tmle.dnorm <- cbind(colMeans(YnotL.CF.tmle.dnorm_bias_matrix_ate[,n.ind]),apply(YnotL.CF.tmle.dnorm_bias_matrix_ate[,n.ind],2,sd),YnotL.CF.tmle.dnorm_avg.MSE_ate[n.ind,2]/n,YnotL.CF.tmle.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YnotL.CF.tmle.dnorm_ci_matrix_ate_upper-YnotL.CF.tmle.dnorm_ci_matrix_ate_lower)[,n.ind]))

# CF, TMLE, densratio
YnotL.CF.tmle.densratio <- cbind(colMeans(YnotL.CF.tmle.densratio_bias_matrix_ate[,n.ind]),apply(YnotL.CF.tmle.densratio_bias_matrix_ate[,n.ind],2,sd),YnotL.CF.tmle.densratio_avg.MSE_ate[n.ind,2]/n,YnotL.CF.tmle.densratio_ci_coverage_ATE[n.ind,2],colMeans((YnotL.CF.tmle.densratio_ci_matrix_ate_upper-YnotL.CF.tmle.densratio_ci_matrix_ate_lower)[,n.ind]))

# CF, TMLE, bayes
YnotL.CF.tmle.bayes <- cbind(colMeans(YnotL.CF.tmle.bayes_bias_matrix_ate[,n.ind]),apply(YnotL.CF.tmle.bayes_bias_matrix_ate[,n.ind],2,sd),YnotL.CF.tmle.bayes_avg.MSE_ate[n.ind,2]/n,YnotL.CF.tmle.bayes_ci_coverage_ATE[n.ind,2],colMeans((YnotL.CF.tmle.bayes_ci_matrix_ate_upper-YnotL.CF.tmle.bayes_ci_matrix_ate_lower)[,n.ind]))

# Linear, Onestep, dnorm
YnotL.linear.Onestep.dnorm <- cbind(colMeans(YnotL.linear.Onestep.dnorm_bias_matrix_ate[,n.ind]),apply(YnotL.linear.Onestep.dnorm_bias_matrix_ate[,n.ind],2,sd),YnotL.linear.Onestep.dnorm_avg.MSE_ate[n.ind,2]/n,YnotL.linear.Onestep.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YnotL.linear.Onestep.dnorm_ci_matrix_ate_upper-YnotL.linear.Onestep.dnorm_ci_matrix_ate_lower)[,n.ind]))

# Linear, Onestep, densratio
YnotL.linear.Onestep.densratio <- cbind(colMeans(YnotL.linear.Onestep.densratio_bias_matrix_ate[,n.ind]),apply(YnotL.linear.Onestep.densratio_bias_matrix_ate[,n.ind],2,sd),YnotL.linear.Onestep.densratio_avg.MSE_ate[n.ind,2]/n,YnotL.linear.Onestep.densratio_ci_coverage_ATE[n.ind,2],colMeans((YnotL.linear.Onestep.densratio_ci_matrix_ate_upper-YnotL.linear.Onestep.densratio_ci_matrix_ate_lower)[,n.ind]))

# Linear, Onestep, bayes
YnotL.linear.Onestep.bayes <- cbind(colMeans(YnotL.linear.Onestep.bayes_bias_matrix_ate[,n.ind]),apply(YnotL.linear.Onestep.bayes_bias_matrix_ate[,n.ind],2,sd),YnotL.linear.Onestep.bayes_avg.MSE_ate[n.ind,2]/n,YnotL.linear.Onestep.bayes_ci_coverage_ATE[n.ind,2],colMeans((YnotL.linear.Onestep.bayes_ci_matrix_ate_upper-YnotL.linear.Onestep.bayes_ci_matrix_ate_lower)[,n.ind]))

# SL, Onestep, dnorm
YnotL.SL.Onestep.dnorm <- cbind(colMeans(YnotL.SL.Onestep.dnorm_bias_matrix_ate[,n.ind]),apply(YnotL.SL.Onestep.dnorm_bias_matrix_ate[,n.ind],2,sd),YnotL.SL.Onestep.dnorm_avg.MSE_ate[n.ind,2]/n,YnotL.SL.Onestep.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YnotL.SL.Onestep.dnorm_ci_matrix_ate_upper-YnotL.SL.Onestep.dnorm_ci_matrix_ate_lower)[,n.ind]))

# SL, Onestep, densratio
YnotL.SL.Onestep.densratio <- cbind(colMeans(YnotL.SL.Onestep.densratio_bias_matrix_ate[,n.ind]),apply(YnotL.SL.Onestep.densratio_bias_matrix_ate[,n.ind],2,sd),YnotL.SL.Onestep.densratio_avg.MSE_ate[n.ind,2]/n,YnotL.SL.Onestep.densratio_ci_coverage_ATE[n.ind,2],colMeans((YnotL.SL.Onestep.densratio_ci_matrix_ate_upper-YnotL.SL.Onestep.densratio_ci_matrix_ate_lower)[,n.ind]))

# SL, Onestep, bayes
YnotL.SL.Onestep.bayes <- cbind(colMeans(YnotL.SL.Onestep.bayes_bias_matrix_ate[,n.ind]),apply(YnotL.SL.Onestep.bayes_bias_matrix_ate[,n.ind],2,sd),YnotL.SL.Onestep.bayes_avg.MSE_ate[n.ind,2]/n,YnotL.SL.Onestep.bayes_ci_coverage_ATE[n.ind,2],colMeans((YnotL.SL.Onestep.bayes_ci_matrix_ate_upper-YnotL.SL.Onestep.bayes_ci_matrix_ate_lower)[,n.ind]))

# CF, Onestep, dnorm
YnotL.CF.Onestep.dnorm <- cbind(colMeans(YnotL.CF.Onestep.dnorm_bias_matrix_ate[,n.ind]),apply(YnotL.CF.Onestep.dnorm_bias_matrix_ate[,n.ind],2,sd),YnotL.CF.Onestep.dnorm_avg.MSE_ate[n.ind,2]/n,YnotL.CF.Onestep.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YnotL.CF.Onestep.dnorm_ci_matrix_ate_upper-YnotL.CF.Onestep.dnorm_ci_matrix_ate_lower)[,n.ind]))

# CF, Onestep, densratio
YnotL.CF.Onestep.densratio <- cbind(colMeans(YnotL.CF.Onestep.densratio_bias_matrix_ate[,n.ind]),apply(YnotL.CF.Onestep.densratio_bias_matrix_ate[,n.ind],2,sd),YnotL.CF.Onestep.densratio_avg.MSE_ate[n.ind,2]/n,YnotL.CF.Onestep.densratio_ci_coverage_ATE[n.ind,2],colMeans((YnotL.CF.Onestep.densratio_ci_matrix_ate_upper-YnotL.CF.Onestep.densratio_ci_matrix_ate_lower)[,n.ind]))

# CF, Onestep, bayes
YnotL.CF.Onestep.bayes <- cbind(colMeans(YnotL.CF.Onestep.bayes_bias_matrix_ate[,n.ind]),apply(YnotL.CF.Onestep.bayes_bias_matrix_ate[,n.ind],2,sd),YnotL.CF.Onestep.bayes_avg.MSE_ate[n.ind,2]/n,YnotL.CF.Onestep.bayes_ci_coverage_ATE[n.ind,2],colMeans((YnotL.CF.Onestep.bayes_ci_matrix_ate_upper-YnotL.CF.Onestep.bayes_ci_matrix_ate_lower)[,n.ind]))

dat <- list(YnotL.linear.tmle.dnorm, YnotL.SL.tmle.dnorm, YnotL.CF.tmle.dnorm, YnotL.linear.tmle.densratio, YnotL.SL.tmle.densratio, YnotL.CF.tmle.densratio, YnotL.linear.tmle.bayes, YnotL.SL.tmle.bayes, YnotL.CF.tmle.bayes, 
            YnotL.linear.Onestep.dnorm, YnotL.SL.Onestep.dnorm, YnotL.CF.Onestep.dnorm, YnotL.linear.Onestep.densratio, YnotL.SL.Onestep.densratio, YnotL.CF.Onestep.densratio, YnotL.linear.Onestep.bayes, YnotL.SL.Onestep.bayes, YnotL.CF.Onestep.bayes)

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

dat <- rbind(dt1,dt2,dt3)

# make table
library(huxtable)
library(dplyr)
options(scipen = 999)

table1 <- as_hux(dat) %>% 
  insert_row("TMLEs","","","","","","","","","One-step estimators","","","","","","","","", after = 0) %>% 
  merge_cells(1, 1:9) %>% 
  merge_cells(1, 10:18) %>% 
  insert_row("\\(\\psi_{dnorm}(\\hat{Q}^\\star)\\)","","","\\(\\psi_{densratio}(\\hat{Q}^\\star)\\)","","","\\(\\psi_{bayes}(\\hat{Q}^\\star)\\)","","",
             "\\(\\psi^{+}_{dnorm}(\\hat{Q})\\)","","","\\(\\psi^{+}_{densratio}(\\hat{Q})\\)","","","\\(\\psi^{+}_{bayes}(\\hat{Q})\\)","","", after = 1) %>% 
  merge_cells(2, 1:3) %>%
  merge_cells(2, 4:6) %>%
  merge_cells(2, 7:9) %>%
  merge_cells(2, 10:12) %>%
  merge_cells(2, 13:15) %>%
  merge_cells(2, 16:18) %>%
  insert_row("Linear","SL","CF","Linear","SL","CF","Linear","SL","CF","Linear","SL","CF","Linear","SL","CF","Linear","SL","CF", after = 2) %>% 
  insert_row("","","","","","","","","","","","","","","","","","", after = 3) %>% 
  insert_row("","","","","","","","","","","","","","","","","","", after = 9) %>% 
  insert_row("","","","","","","","","","","","","","","","","","", after = 15) %>% 
  insert_column(c("","","","n=500","Bias","SD","MSE","CI coverage","CI width","n=1000","Bias","SD","MSE","CI coverage","CI width","n=2000","Bias","SD","MSE","CI coverage","CI width"), after = 0) %>%
  set_escape_contents(3, 1:ncol(.), FALSE) %>%
  set_align(col=1, everywhere, "left") %>%
  set_align(col=2:ncol(.),everywhere,"center") %>%
  set_all_padding(-1)  %>% 
  set_bold(1, everywhere) %>%
  set_bold(c(4,10,16), everywhere) %>%
  set_italic(2,everywhere) %>%
  set_bottom_border(row = 1, col =2:ncol(.)) %>% 
  set_bottom_border(row = 3, col =2:ncol(.)) %>% 
  set_bottom_border(row = 2, col =2:ncol(.)) %>% 
  set_right_border(4:nrow(.), 4, brdr(0.4,"solid")) %>%
  set_right_border(4:nrow(.), 7, brdr(0.4,"solid")) %>%
  set_right_border(4:nrow(.), 10, brdr(0.4,"double")) %>%
  set_right_border(4:nrow(.), 13, brdr(0.4,"solid")) %>%
  set_right_border(4:nrow(.), 16, brdr(0.4,"solid")) %>%
  set_top_border(row=1,col=everywhere,brdr(1, "solid")) %>% set_bottom_border(row = nrow(.),col = everywhere,brdr(1, "solid")) %>%
  set_font_size(7) %>% set_escape_contents(2, 1:ncol(.), FALSE) %>% set_caption("Comparative analysis of TMLEs and one-step estimators under model misspecifications when the outcome is not in the district of the treatment.") 

table1
quick_latex(table1, file = "table-not.tex")
y
