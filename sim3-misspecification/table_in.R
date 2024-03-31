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
load("YinL/Linear/TMLE-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.linear.tmle.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YinL/Linear/TMLE-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.linear.tmle.densratio_", obj)
  assign(new_name, get(obj))
}

load("YinL/Linear/TMLE-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.linear.tmle.bayes_", obj)
  assign(new_name, get(obj))
}

## TMLE-SL
load("YinL/SL/TMLE-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.SL.tmle.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YinL/SL/TMLE-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.SL.tmle.densratio_", obj)
  assign(new_name, get(obj))
}

load("YinL/SL/TMLE-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.SL.tmle.bayes_", obj)
  assign(new_name, get(obj))
}

## TMLE-CF
load("YinL/CF/TMLE-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.CF.tmle.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YinL/CF/TMLE-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.CF.tmle.densratio_", obj)
  assign(new_name, get(obj))
}

load("YinL/CF/TMLE-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.CF.tmle.bayes_", obj)
  assign(new_name, get(obj))
}

## Onestep-Linear
load("YinL/Linear/Onestep-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.linear.Onestep.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YinL/Linear/Onestep-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.linear.Onestep.densratio_", obj)
  assign(new_name, get(obj))
}

load("YinL/Linear/Onestep-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.linear.Onestep.bayes_", obj)
  assign(new_name, get(obj))
}

## Onestep-SL
load("YinL/SL/Onestep-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.SL.Onestep.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YinL/SL/Onestep-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.SL.Onestep.densratio_", obj)
  assign(new_name, get(obj))
}

load("YinL/SL/Onestep-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.SL.Onestep.bayes_", obj)
  assign(new_name, get(obj))
}

## Onestep-CF
load("YinL/CF/Onestep-dnorm/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.CF.Onestep.dnorm_", obj)
  assign(new_name, get(obj))
}

load("YinL/CF/Onestep-densratio/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.CF.Onestep.densratio_", obj)
  assign(new_name, get(obj))
}

load("YinL/CF/Onestep-bayes/result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("YinL.CF.Onestep.bayes_", obj)
  assign(new_name, get(obj))
}


# Linear, TMLE, dnorm
YinL.linear.tmle.dnorm <- cbind(colMeans(YinL.linear.tmle.dnorm_bias_matrix_ate[,n.ind]),apply(YinL.linear.tmle.dnorm_bias_matrix_ate[,n.ind],2,sd),YinL.linear.tmle.dnorm_avg.MSE_ate[n.ind,2]/n,YinL.linear.tmle.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YinL.linear.tmle.dnorm_ci_matrix_ate_upper-YinL.linear.tmle.dnorm_ci_matrix_ate_lower)[,n.ind]))

# Linear, TMLE, densratio
YinL.linear.tmle.densratio <- cbind(colMeans(YinL.linear.tmle.densratio_bias_matrix_ate[,n.ind]),apply(YinL.linear.tmle.densratio_bias_matrix_ate[,n.ind],2,sd),YinL.linear.tmle.densratio_avg.MSE_ate[n.ind,2]/n,YinL.linear.tmle.densratio_ci_coverage_ATE[n.ind,2],colMeans((YinL.linear.tmle.densratio_ci_matrix_ate_upper-YinL.linear.tmle.densratio_ci_matrix_ate_lower)[,n.ind]))

# Linear, TMLE, bayes
YinL.linear.tmle.bayes <- cbind(colMeans(YinL.linear.tmle.bayes_bias_matrix_ate[,n.ind]),apply(YinL.linear.tmle.bayes_bias_matrix_ate[,n.ind],2,sd),YinL.linear.tmle.bayes_avg.MSE_ate[n.ind,2]/n,YinL.linear.tmle.bayes_ci_coverage_ATE[n.ind,2],colMeans((YinL.linear.tmle.bayes_ci_matrix_ate_upper-YinL.linear.tmle.bayes_ci_matrix_ate_lower)[,n.ind]))

# SL, TMLE, dnorm
YinL.SL.tmle.dnorm <- cbind(colMeans(YinL.SL.tmle.dnorm_bias_matrix_ate[,n.ind]),apply(YinL.SL.tmle.dnorm_bias_matrix_ate[,n.ind],2,sd),YinL.SL.tmle.dnorm_avg.MSE_ate[n.ind,2]/n,YinL.SL.tmle.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YinL.SL.tmle.dnorm_ci_matrix_ate_upper-YinL.SL.tmle.dnorm_ci_matrix_ate_lower)[,n.ind]))

# SL, TMLE, densratio
YinL.SL.tmle.densratio <- cbind(colMeans(YinL.SL.tmle.densratio_bias_matrix_ate[,n.ind]),apply(YinL.SL.tmle.densratio_bias_matrix_ate[,n.ind],2,sd),YinL.SL.tmle.densratio_avg.MSE_ate[n.ind,2]/n,YinL.SL.tmle.densratio_ci_coverage_ATE[n.ind,2],colMeans((YinL.SL.tmle.densratio_ci_matrix_ate_upper-YinL.SL.tmle.densratio_ci_matrix_ate_lower)[,n.ind]))

# SL, TMLE, bayes
YinL.SL.tmle.bayes <- cbind(colMeans(YinL.SL.tmle.bayes_bias_matrix_ate[,n.ind]),apply(YinL.SL.tmle.bayes_bias_matrix_ate[,n.ind],2,sd),YinL.SL.tmle.bayes_avg.MSE_ate[n.ind,2]/n,YinL.SL.tmle.bayes_ci_coverage_ATE[n.ind,2],colMeans((YinL.SL.tmle.bayes_ci_matrix_ate_upper-YinL.SL.tmle.bayes_ci_matrix_ate_lower)[,n.ind]))

# CF, TMLE, dnorm
YinL.CF.tmle.dnorm <- cbind(colMeans(YinL.CF.tmle.dnorm_bias_matrix_ate[,n.ind]),apply(YinL.CF.tmle.dnorm_bias_matrix_ate[,n.ind],2,sd),YinL.CF.tmle.dnorm_avg.MSE_ate[n.ind,2]/n,YinL.CF.tmle.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YinL.CF.tmle.dnorm_ci_matrix_ate_upper-YinL.CF.tmle.dnorm_ci_matrix_ate_lower)[,n.ind]))

# CF, TMLE, densratio
YinL.CF.tmle.densratio <- cbind(colMeans(YinL.CF.tmle.densratio_bias_matrix_ate[,n.ind]),apply(YinL.CF.tmle.densratio_bias_matrix_ate[,n.ind],2,sd),YinL.CF.tmle.densratio_avg.MSE_ate[n.ind,2]/n,YinL.CF.tmle.densratio_ci_coverage_ATE[n.ind,2],colMeans((YinL.CF.tmle.densratio_ci_matrix_ate_upper-YinL.CF.tmle.densratio_ci_matrix_ate_lower)[,n.ind]))

# CF, TMLE, bayes
YinL.CF.tmle.bayes <- cbind(colMeans(YinL.CF.tmle.bayes_bias_matrix_ate[,n.ind]),apply(YinL.CF.tmle.bayes_bias_matrix_ate[,n.ind],2,sd),YinL.CF.tmle.bayes_avg.MSE_ate[n.ind,2]/n,YinL.CF.tmle.bayes_ci_coverage_ATE[n.ind,2],colMeans((YinL.CF.tmle.bayes_ci_matrix_ate_upper-YinL.CF.tmle.bayes_ci_matrix_ate_lower)[,n.ind]))

# Linear, Onestep, dnorm
YinL.linear.Onestep.dnorm <- cbind(colMeans(YinL.linear.Onestep.dnorm_bias_matrix_ate[,n.ind]),apply(YinL.linear.Onestep.dnorm_bias_matrix_ate[,n.ind],2,sd),YinL.linear.Onestep.dnorm_avg.MSE_ate[n.ind,2]/n,YinL.linear.Onestep.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YinL.linear.Onestep.dnorm_ci_matrix_ate_upper-YinL.linear.Onestep.dnorm_ci_matrix_ate_lower)[,n.ind]))

# Linear, Onestep, densratio
YinL.linear.Onestep.densratio <- cbind(colMeans(YinL.linear.Onestep.densratio_bias_matrix_ate[,n.ind]),apply(YinL.linear.Onestep.densratio_bias_matrix_ate[,n.ind],2,sd),YinL.linear.Onestep.densratio_avg.MSE_ate[n.ind,2]/n,YinL.linear.Onestep.densratio_ci_coverage_ATE[n.ind,2],colMeans((YinL.linear.Onestep.densratio_ci_matrix_ate_upper-YinL.linear.Onestep.densratio_ci_matrix_ate_lower)[,n.ind]))

# Linear, Onestep, bayes
YinL.linear.Onestep.bayes <- cbind(colMeans(YinL.linear.Onestep.bayes_bias_matrix_ate[,n.ind]),apply(YinL.linear.Onestep.bayes_bias_matrix_ate[,n.ind],2,sd),YinL.linear.Onestep.bayes_avg.MSE_ate[n.ind,2]/n,YinL.linear.Onestep.bayes_ci_coverage_ATE[n.ind,2],colMeans((YinL.linear.Onestep.bayes_ci_matrix_ate_upper-YinL.linear.Onestep.bayes_ci_matrix_ate_lower)[,n.ind]))

# SL, Onestep, dnorm
YinL.SL.Onestep.dnorm <- cbind(colMeans(YinL.SL.Onestep.dnorm_bias_matrix_ate[,n.ind]),apply(YinL.SL.Onestep.dnorm_bias_matrix_ate[,n.ind],2,sd),YinL.SL.Onestep.dnorm_avg.MSE_ate[n.ind,2]/n,YinL.SL.Onestep.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YinL.SL.Onestep.dnorm_ci_matrix_ate_upper-YinL.SL.Onestep.dnorm_ci_matrix_ate_lower)[,n.ind]))

# SL, Onestep, densratio
YinL.SL.Onestep.densratio <- cbind(colMeans(YinL.SL.Onestep.densratio_bias_matrix_ate[,n.ind]),apply(YinL.SL.Onestep.densratio_bias_matrix_ate[,n.ind],2,sd),YinL.SL.Onestep.densratio_avg.MSE_ate[n.ind,2]/n,YinL.SL.Onestep.densratio_ci_coverage_ATE[n.ind,2],colMeans((YinL.SL.Onestep.densratio_ci_matrix_ate_upper-YinL.SL.Onestep.densratio_ci_matrix_ate_lower)[,n.ind]))

# SL, Onestep, bayes
YinL.SL.Onestep.bayes <- cbind(colMeans(YinL.SL.Onestep.bayes_bias_matrix_ate[,n.ind]),apply(YinL.SL.Onestep.bayes_bias_matrix_ate[,n.ind],2,sd),YinL.SL.Onestep.bayes_avg.MSE_ate[n.ind,2]/n,YinL.SL.Onestep.bayes_ci_coverage_ATE[n.ind,2],colMeans((YinL.SL.Onestep.bayes_ci_matrix_ate_upper-YinL.SL.Onestep.bayes_ci_matrix_ate_lower)[,n.ind]))

# CF, Onestep, dnorm
YinL.CF.Onestep.dnorm <- cbind(colMeans(YinL.CF.Onestep.dnorm_bias_matrix_ate[,n.ind]),apply(YinL.CF.Onestep.dnorm_bias_matrix_ate[,n.ind],2,sd),YinL.CF.Onestep.dnorm_avg.MSE_ate[n.ind,2]/n,YinL.CF.Onestep.dnorm_ci_coverage_ATE[n.ind,2],colMeans((YinL.CF.Onestep.dnorm_ci_matrix_ate_upper-YinL.CF.Onestep.dnorm_ci_matrix_ate_lower)[,n.ind]))

# CF, Onestep, densratio
YinL.CF.Onestep.densratio <- cbind(colMeans(YinL.CF.Onestep.densratio_bias_matrix_ate[,n.ind]),apply(YinL.CF.Onestep.densratio_bias_matrix_ate[,n.ind],2,sd),YinL.CF.Onestep.densratio_avg.MSE_ate[n.ind,2]/n,YinL.CF.Onestep.densratio_ci_coverage_ATE[n.ind,2],colMeans((YinL.CF.Onestep.densratio_ci_matrix_ate_upper-YinL.CF.Onestep.densratio_ci_matrix_ate_lower)[,n.ind]))

# CF, Onestep, bayes
YinL.CF.Onestep.bayes <- cbind(colMeans(YinL.CF.Onestep.bayes_bias_matrix_ate[,n.ind]),apply(YinL.CF.Onestep.bayes_bias_matrix_ate[,n.ind],2,sd),YinL.CF.Onestep.bayes_avg.MSE_ate[n.ind,2]/n,YinL.CF.Onestep.bayes_ci_coverage_ATE[n.ind,2],colMeans((YinL.CF.Onestep.bayes_ci_matrix_ate_upper-YinL.CF.Onestep.bayes_ci_matrix_ate_lower)[,n.ind]))

dat <- list(YinL.linear.tmle.dnorm, YinL.SL.tmle.dnorm, YinL.CF.tmle.dnorm, YinL.linear.tmle.densratio, YinL.SL.tmle.densratio, YinL.CF.tmle.densratio, YinL.linear.tmle.bayes, YinL.SL.tmle.bayes, YinL.CF.tmle.bayes, 
            YinL.linear.Onestep.dnorm, YinL.SL.Onestep.dnorm, YinL.CF.Onestep.dnorm, YinL.linear.Onestep.densratio, YinL.SL.Onestep.densratio, YinL.CF.Onestep.densratio, YinL.linear.Onestep.bayes, YinL.SL.Onestep.bayes, YinL.CF.Onestep.bayes)

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
  set_bottom_border(row = 4, col =2:ncol(.)) %>% 
  set_bottom_border(row = 2, col =2:ncol(.)) %>% 
  set_right_border(4:nrow(.), 4, brdr(0.4,"solid")) %>%
  set_right_border(4:nrow(.), 7, brdr(0.4,"solid")) %>%
  set_right_border(4:nrow(.), 10, brdr(0.4,"double")) %>%
  set_right_border(4:nrow(.), 13, brdr(0.4,"solid")) %>%
  set_right_border(4:nrow(.), 16, brdr(0.4,"solid")) %>%
  set_top_border(row=1,col=everywhere,brdr(1, "solid")) %>% set_bottom_border(row = nrow(.),col = everywhere,brdr(1, "solid")) %>%
  set_font_size(7) %>% set_escape_contents(2, 1:ncol(.), FALSE) %>% set_caption("Comparative analysis of TMLEs and one-step estimators under model misspecifications when the outcome is in the district of the treatment.") 

table1
quick_latex(table1)
y
