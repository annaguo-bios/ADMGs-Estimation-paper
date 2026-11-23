library(ggplot2)
library(ggpubr)
library(latex2exp)
library(stats)
library(xtable)
library(gridExtra)
library(cowplot)

setwd(here::here("sim5-semieff"))

source("plot-sub.R")

n.vec <- c(250, 500, 1000, 2000, 4000, 8000)

load("DGPs/semieff-truth.Rdata")

load("TMLE/result.Rdata")
p.tmle <- plot.estimator(r'($\psi(\hat{Q}^*)$)')

load("Onestep/result.Rdata")
p.onestep <- plot.estimator(r'($\psi^{+}(\hat{Q})$)')

legend.data <- data.frame(n = rep(n.vec[1:2], 2),
                          value = c(0, 1, 0, 1),
                          estimator = factor(rep(c("Nonparametric EIF", "Semiparametric EIF"), each = 2),
                                             levels = c("Nonparametric EIF", "Semiparametric EIF")))
p.legend <- ggplot(legend.data, aes(x = n, y = value, color = estimator, shape = estimator)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Nonparametric EIF" = "#F8766D", "Semiparametric EIF" = "#00BFC4")) +
  guides(color = guide_legend(override.aes = list(size = 4, linewidth = 1))) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.key.width = grid::unit(1.5, "cm"),
        legend.key.height = grid::unit(0.7, "cm"))
shared.legend <- get_legend(p.legend)

p.panels <- plot_grid(p.tmle, p.onestep, align = "h", ncol = 2)
p.final <- plot_grid(p.panels, shared.legend, ncol = 1, rel_heights = c(1, 0.08))

ggsave("sim5-semieff.pdf", plot = p.final, width = 16, height = 9, units = "in")
