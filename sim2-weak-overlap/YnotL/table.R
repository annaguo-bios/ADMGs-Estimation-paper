setwd("/Users/apple/Library/CloudStorage/Dropbox/primal-fixability/code/Simulations/sim2-weak-overlap/YnotL")

library(outliers) #containing function outlier
library(dplyr)

# sample size
n= c(500,1000,2000)
n.vec <- c(500,1000,2000)
n.ind <- which(n.vec %in% n)

# round the numbers
decimal <- 3
nsim <- 1000

######################################################################
############## Simulation 2: positivity violation ##############
######################################################################
objs <- c("ci_coverage_ATE","avg.MSE_ate","bias_matrix_ate","ci_matrix_ate_lower","ci_matrix_ate_upper")

# Continuous M, Onestep-dnorm-----------------
load("Onestep-dnorm/result.Rdata")

for (obj in objs) {
  new_name <- paste0("onestep.dnorm_", obj)
  assign(new_name, get(obj))
}

# Continuous M, dnorm-----------------
load("TMLE-dnorm/result.Rdata")

for (obj in objs) {
  new_name <- paste0("dnorm_", obj)
  assign(new_name, get(obj))
}

# Continuous M, Onestep-densratio-----------------
load("Onestep-densratio/result.Rdata")

for (obj in objs) {
  new_name <- paste0("onestep.densratio_", obj)
  assign(new_name, get(obj))
}

# Continuous M, Estimator2-----------------
load("TMLE-densratio/result.Rdata")

for (obj in objs) {
  new_name <- paste0("densratio_", obj)
  assign(new_name, get(obj))
}

# Continuous M, Onestep-bayes-----------------
load("Onestep-bayes/result.Rdata")

for (obj in objs) {
  new_name <- paste0("onestep.bayes_", obj)
  assign(new_name, get(obj))
}

# Continuous M, Estimator3-----------------
load("TMLE-bayes/result.Rdata")

for (obj in objs) {
  new_name <- paste0("bayes_", obj)
  assign(new_name, get(obj))
}


dnorm.ate <- cbind(colMeans(dnorm_bias_matrix_ate[,n.ind]),apply(dnorm_bias_matrix_ate[,n.ind],2,sd),dnorm_avg.MSE_ate[n.ind,2]/n,dnorm_ci_coverage_ATE[n.ind,2],colMeans((dnorm_ci_matrix_ate_upper-dnorm_ci_matrix_ate_lower)[,n.ind]))


# continuous est2
densratio.ate <- cbind(colMeans(densratio_bias_matrix_ate[,n.ind]),apply(densratio_bias_matrix_ate[,n.ind],2,sd),densratio_avg.MSE_ate[n.ind,2]/n,densratio_ci_coverage_ATE[n.ind,2],colMeans((densratio_ci_matrix_ate_upper-densratio_ci_matrix_ate_lower)[,n.ind]))


# continuous est3
bayes.ate <- cbind(colMeans(bayes_bias_matrix_ate[,n.ind]),apply(bayes_bias_matrix_ate[,n.ind],2,sd),bayes_avg.MSE_ate[n.ind,2]/n,bayes_ci_coverage_ATE[n.ind,2],colMeans((bayes_ci_matrix_ate_upper-bayes_ci_matrix_ate_lower)[,n.ind]))


# continuous Onestep-dnorm
onestep.dnorm.ate <- cbind(colMeans(onestep.dnorm_bias_matrix_ate[,n.ind]),apply(onestep.dnorm_bias_matrix_ate[,n.ind],2,sd),onestep.dnorm_avg.MSE_ate[n.ind,2]/n,onestep.dnorm_ci_coverage_ATE[n.ind,2],colMeans((onestep.dnorm_ci_matrix_ate_upper-onestep.dnorm_ci_matrix_ate_lower)[,n.ind]))

# continuous onestep-den
onestep.densratio.ate <- cbind(colMeans(onestep.densratio_bias_matrix_ate[,n.ind]),apply(onestep.densratio_bias_matrix_ate[,n.ind],2,sd),onestep.densratio_avg.MSE_ate[n.ind,2]/n,onestep.densratio_ci_coverage_ATE[n.ind,2],colMeans((onestep.densratio_ci_matrix_ate_upper-onestep.densratio_ci_matrix_ate_lower)[,n.ind]))

# continuous onestep-bayes
onestep.bayes.ate <- cbind(colMeans(onestep.bayes_bias_matrix_ate[,n.ind]),apply(onestep.bayes_bias_matrix_ate[,n.ind],2,sd),onestep.bayes_avg.MSE_ate[n.ind,2]/n,onestep.bayes_ci_coverage_ATE[n.ind,2],colMeans((onestep.bayes_ci_matrix_ate_upper-onestep.bayes_ci_matrix_ate_lower)[,n.ind]))



positivity.dat <- list(onestep.dnorm.ate,dnorm.ate,
                       onestep.densratio.ate,densratio.ate,
                       onestep.bayes.ate,bayes.ate)

for (i in seq_along(n)) {
  
  # Extract the column from each data frame and cbind
  columns <- lapply(positivity.dat,function(x) x[i,])  # Extract the first column from each data frame
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
  insert_row("\\(\\psi_{dnorm}^{+}(\\hat{Q})\\)","\\(\\psi_{dnorm}(\\hat{Q}^\\star)\\)","\\(\\psi_{densratio}^{+}(\\hat{Q})\\)","\\(\\psi_{densratio}(\\hat{Q}^\\star)\\)","\\(\\psi_{bayes}^{+}(\\hat{Q})\\)","\\(\\psi_{bayes}(\\hat{Q}^\\star)\\)", after = 0) %>% 
  insert_row("","","","","","",after=1) %>%
  insert_row("","","","","","",after=7) %>%
  insert_row("","","","","","",after=13) %>%
  insert_column(c("","n=500","Bias","SD","MSE","CI coverage","CI width","n=1000","Bias","SD","MSE","CI coverage","CI width","n=2000","Bias","SD","MSE","CI coverage","CI width"), after = 0) %>%
  set_escape_contents(1, 1:ncol(.), FALSE) %>%
  set_align(col=1, everywhere, "left") %>%
  set_align(col=2:ncol(.),everywhere,"center") %>%
  set_tb_padding(1, everywhere, 0) %>% 
  set_bold(1, everywhere) %>%
  set_bold(c(2,8,14), everywhere) %>%
  # set_italic(2,everywhere) %>%
  set_bottom_border(row = 1, col =2:ncol(.)) %>% 
  # set_bottom_border(row = 2, col =2:ncol(.)) %>% 
  set_right_border(2:nrow(.), 3, brdr(0.4,"dotted")) %>%
  set_right_border(2:nrow(.), 5, brdr(0.4,"dotted")) %>%
  set_escape_contents(nrow(.), 1:ncol(.), FALSE) %>%
  set_top_border(row=1,col=everywhere,brdr(1, "solid")) %>% set_bottom_border(row = nrow(.),col = everywhere,brdr(1, "solid")) %>%
  set_escape_contents(nrow(.), 1:ncol(.), FALSE) %>% set_caption("Comparison between TMLE estimators and one-step EIF estimator under positivity assumption violation when the outcome is not in the district of the treatment.") %>%
  set_all_padding(1) %>% set_font_size(8) 

table1
quick_latex(table1, file="table-not.tex")


