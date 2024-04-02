# packages
library(ggplot2)
library(ggpubr)
library(latex2exp)
library(reshape2)
library(stats)
library(xtable)
library(gridExtra)
library(cowplot)

# plot function
source("plot-sub.R")

# sample size
n.vec <- c(250,500,1000,2000,4000,8000)

# number of simulation
nsim <- 1000

### binary case ====
# the truth
load("../DGPs/YinL-truth.Rdata")

## bayes-tmle====
load("TMLE-bayes/result.Rdata")
p.bayes.tmle <- plot.tmle(r'($\psi(\hat{Q}^*)$ - bayes)', ylim.var = c(190,300))

## bayes-onestep====
load("Onestep-bayes/result.Rdata")
p.bayes.one <- plot.tmle(r'($\psi^{+}(\hat{Q})$ - bayes)', ylim.var = c(190,300))


## dnorm-tmle====
load("TMLE-dnorm/result.Rdata")
p.dnorm.tmle <- plot.tmle(r'($\psi(\hat{Q}^*)$ - dnorm)',ylim.bias=c(-2,1.5), ylim.var=c(180,260))

## dnorm-onestep====
load("Onestep-dnorm/result.Rdata")
p.dnorm.one <- plot.tmle(r'($\psi^{+}(\hat{Q})$ - dnorm)',ylim.bias=c(-2,1.5), ylim.var=c(180,260))

## densratio-tmle====
load("TMLE-densratio/result.Rdata")
p.densratio.tmle <- plot.tmle(r'($\psi(\hat{Q}^*)$ - densratio)')

## densratio-onestep====
load("Onestep-densratio/result.Rdata")
p.densratio.one <- plot.tmle(r'($\psi^{+}(\hat{Q})$ - densratio)')

p.tmle <- plot_grid(
  p.bayes.tmle,p.dnorm.tmle,p.densratio.tmle,
  align = "v", ncol = 1
)

p.one <- plot_grid(
  p.bayes.one,p.dnorm.one,p.densratio.one,
  align = "v", ncol = 1
)

p.final <- plot_grid(
p.tmle,p.one,
  align = "h",
  ncol = 2
)


ggsave("plot.pdf", plot = p.final, width = 16, height = 18, units = "in")
