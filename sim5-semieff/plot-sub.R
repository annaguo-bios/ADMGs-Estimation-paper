plot.estimator <- function(plot_title,
                           ycoord.bias = NULL,
                           ycoord.var = NULL,
                           ylim.bias = NULL,
                           ylim.var = NULL) {
  if (exists("avg.bias_ate.by_estimator") && exists("avg.variance_ate.by_estimator")) {
    bias <- avg.bias_ate.by_estimator[avg.bias_ate.by_estimator$n %in% n.vec, ]
    variance <- avg.variance_ate.by_estimator[avg.variance_ate.by_estimator$n %in% n.vec, ]
  } else {
    bias <- avg.bias_ate[avg.bias_ate$n %in% n.vec, ]
    variance <- avg.variance_ate[avg.variance_ate$n %in% n.vec, ]
    bias$estimator <- "semiparametric IF"
    variance$estimator <- "semiparametric IF"
  }

	  bias$estimator <- factor(bias$estimator, levels = c("np EIF", "semiparametric IF"))
	  variance$estimator <- factor(variance$estimator, levels = c("np EIF", "semiparametric IF"))
	  estimator.colors <- c("np EIF" = "#F8766D", "semiparametric IF" = "#00BFC4")

  yaxis_bias <- TeX(r'($\sqrt{n}$-Bias)')
  yaxis_var <- TeX(r'($n$-Variance)')

  p.bias <- ggplot(data = bias, aes(x = n, y = bias, color = estimator, shape = estimator)) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey35", linewidth = 0.8) +
    coord_cartesian(ylim = ycoord.bias) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
	          panel.background = element_blank(),
	          axis.text = element_text(size = 12),
	          axis.title = element_text(size = 13, face = "bold"),
	          legend.position = "none",
	          legend.title = element_blank()) +
	    theme(axis.text.x = element_text(angle = -45)) +
	    scale_x_continuous(breaks = n.vec) +
	    scale_color_manual(values = estimator.colors) +
	    geom_errorbar(aes(ymin = lower, ymax = upper), width = 25,
	                  position = position_dodge(0.05), linewidth = 0.5) +
    geom_point(size = 3) +
    labs(title = "", x = "", y = yaxis_bias)

  if (!is.null(ylim.bias)) {
    p.bias <- p.bias + ylim(ylim.bias[1], ylim.bias[2])
  }

	  p.variance <- ggplot(data = variance, aes(x = n, y = variance, color = estimator, shape = estimator)) +
	    geom_line(linewidth = 0.7) +
	    geom_hline(yintercept = VAR.ATE, linetype = "dashed", color = estimator.colors[["np EIF"]], linewidth = 0.8) +
	    geom_hline(yintercept = VAR_eff.ATE, linetype = "dashed", color = estimator.colors[["semiparametric IF"]], linewidth = 0.8) +
    coord_cartesian(ylim = ycoord.var) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
	          panel.background = element_blank(),
	          axis.text = element_text(size = 12),
	          axis.title = element_text(size = 13, face = "bold"),
	          legend.position = "none",
	          legend.title = element_blank()) +
	    theme(axis.text.x = element_text(angle = -45)) +
	    scale_x_continuous(breaks = n.vec) +
	    scale_color_manual(values = estimator.colors) +
	    geom_errorbar(aes(ymin = lower, ymax = upper), width = 25,
                  position = position_dodge(0.05), linewidth = 0.5) +
    geom_point(size = 3) +
    labs(title = "", x = "Sample size n", y = yaxis_var)

  if (!is.null(ylim.var)) {
    p.variance <- p.variance + ylim(ylim.var[1], ylim.var[2])
  }

  p <- plot_grid(p.bias, p.variance, align = "hv", ncol = 1)
  annotate_figure(p, left = text_grob(TeX(plot_title),
                                      color = "black",
                                      face = "bold",
                                      size = 17,
                                      rot = 90))
}
