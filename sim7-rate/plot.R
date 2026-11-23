library(ggplot2)
library(cowplot)


source("organize.R", local = TRUE)

load_plot_data <- function(scenario, method) {
  local <- new.env()
  load(file.path(scenario, method, "result.Rdata"), envir = local)
  estimates <- local$est_matrix_ate
  n.vec <- local$n.vec
  
  list(n.vec = n.vec, estimates = estimates,
       ATE = local$ATE, VAR.ATE = local$VAR.ATE)
}

plot_scenarios <- function() {
  scenarios <- c("seq2over5","seq1over3", "seq1over6", "seq1over9", "rate1over10")
  methods <- c(TMLE = r'($\psi(\hat{Q}^*)$ - bayes)',
               Onestep = r'($\psi^{+}(\hat{Q})$ - bayes)')
  scenario.colors <- setNames(c("#F8766D", "#00BFC4", "#7CAE00", "#C77CFF", "#E68613"),
                              scenarios)
  scenario.labels <- expression(
    paste(o[p](n^{-2/5}), ", ", o[p](n^{-1/5})),
    paste(o[p](n^{-1/3}), ", ", o[p](n^{-1/6})),
    paste(o[p](n^{-1/6}), ", ", o[p](n^{-1/8})),
    paste(o[p](n^{-1/9}), ", ", o[p](n^{-1/10})),
    paste(o[p](n^{-1/10}), ", ", o[p](n^{-1/15}))
  )
  
  summaries <- list()
  for (method in names(methods)) {
    for (scenario in scenarios) {
      result <- load_plot_data(scenario, method)
      true_asyvar <- result$VAR.ATE
      errors <- summarize_errors(result$estimates, result$ATE, result$n.vec)
      for (metric in c("bias", "variance")) {
        summaries[[length(summaries) + 1L]] <- data.frame(
          errors[[metric]], metric = metric, scenario = scenario, estimator = method
        )
      }
    }
  }
  summaries <- do.call(rbind, summaries)
  summaries$scenario <- factor(summaries$scenario, levels = scenarios)
  x.breaks <- sort(unique(summaries$n))
  
  plot_panel <- function(method, metric, label, reference) {
    ggplot(summaries[summaries$estimator == method & summaries$metric == metric, ],
           aes(n, value, color = scenario)) +
      geom_line(linewidth = 0.7) +
      geom_hline(yintercept = reference, linetype = "dashed",
                 color = "gray", linewidth = 0.8) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 25, linewidth = 0.5) +
      geom_point(size = 2.5, shape = 16) +
      scale_color_manual(values = scenario.colors, breaks = scenarios, labels = scenario.labels) +
      scale_x_continuous(breaks = x.breaks) +
      labs(x = NULL, y = label,
           color = "Perturbation rates for sequential regressions and density ratios") +
      guides(color = guide_legend(nrow = 1, byrow = TRUE,
                                  title.position = "top", title.hjust = 0.5)) +
      theme_bw() +
      theme(axis.line = element_line(color = "black"),
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.text = element_text(size = 11),
            axis.text.x = element_text(angle = -45, hjust = 0),
            axis.title = element_text(size = 13, face = "bold"),
            legend.position = "bottom",
            legend.text = element_text(size = 12))
  }
  
  columns <- list()
  for (method in names(methods)) {
    bias <- plot_panel(method, "bias", expression(sqrt(n) * "-Bias"), 0)
    variance <- plot_panel(method, "variance", expression(n * "-Variance"), true_asyvar) +
      labs(x = "Sample size n")
    if (method == "TMLE") legend <- cowplot::get_legend(bias)
    
    panels <- plot_grid(bias + theme(legend.position = "none"),
                        variance + theme(legend.position = "none"),
                        ncol = 1, align = "hv", axis = "lr")
    columns[[method]] <- ggdraw() +
      draw_plot(panels, x = 0.07, width = 0.93) +
      draw_label(latex2exp::TeX(methods[[method]]), x = 0.032, y = 0.5, angle = 90,
                 fontface = "bold", size = 16)
  }
  
  panels <- plot_grid(plotlist = columns, ncol = 2, align = "h")
  plot_grid(panels, legend, ncol = 1, rel_heights = c(1, 0.14))
}

p.final <- plot_scenarios()
ggsave("rate.pdf", plot = p.final,
       width = 14, height = 7, units = "in", bg = "white")
