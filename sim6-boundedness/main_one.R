setwd(here::here('sim6-boundedness'))
source('DGPs/dgp.R')
source('DGPs/truth.R')
source('estimate.R')

scenario_list <- list(
  
  ## No or nearly no out-of-bound estimates
  Favorable = list(
    parA = c(0.44, 0.04, 0.08),
    parM = c(-0.50, 0.50, 0.30),
    parL = c(-0.75, 0.40, 0.50, 0.30),
    parY = c(0.65, 0.05, 0.05, 0.05, 0.05)
  ),
  
  ## Your former S5: low but detectable OOB probability
  Moderate = list(
    parA = c(0.04, 0.02, 0.04),
    parM = c(-1.50, 3.25, 0.50),
    parL = c(-1.75, 0.60, 3.25, 0.50),
    parY = c(0.85, 0.02, 0.02, 0.02, 0.02)
  ),
  
  ## Your former S6: clear OOB probability
  Severe = list(
    parA = c(0.015, 0.01, 0.02),
    parM = c(-1.75, 4.00, 0.50),
    parL = c(-2.00, 0.60, 4.00, 0.50),
    parY = c(0.88, 0.015, 0.015, 0.015, 0.015)
  ),
  
  ## More difficult than former S6
  Extreme = list(
    parA = c(0.005, 0.005, 0.01),
    parM = c(-2.00, 4.50, 0.50),
    parL = c(-2.25, 0.60, 4.50, 0.50),
    parY = c(0.89, 0.0125, 0.0125, 0.0125, 0.0125)
  )
)
scenario_names <- names(scenario_list)

n.vec <- c(250)
nsim <- 1000

EY1_truth <- vector("numeric", length(scenario_list))
VAR.EY1_truth <- vector("numeric", length(scenario_list))

for(s in scenario_names){
  cat("Scenario:", s, "\n")
  pars <- scenario_list[[s]]
  
  output_truth <- compute_truth(10^5,
                                parA = pars$parA,
                                parM = pars$parM,
                                parL = pars$parL,
                                parY = pars$parY)
  EY1_truth[which(scenario_names == s)] <- output_truth$E.Y1
  VAR.EY1_truth[which(scenario_names == s)] <- output_truth$VAR.Y1
}

truth_df <- data.frame(
  scenario = factor(
    scenario_names,
    levels = scenario_names
  ),
  truth = EY1_truth
)
truth_df$x <- seq_along(scenario_names)

df_A <- est_A_onestep_a1 <- est_A_tmle_a1 <- array(
  NA_real_,
  dim = c(
    simulation = nsim,
    sample_size = length(n.vec),
    scenario = length(scenario_names)
  ),
  dimnames = list(
    simulation = seq_len(nsim),
    sample_size = as.character(n.vec),
    scenario = scenario_names
  )
)



## multiple simulation
set.seed(7)

for (s in scenario_names){
  
  cat("Scenario:", s, "\n")
  pars <- scenario_list[[s]]
  
  for (i in seq_along(n.vec)) {
    n <- n.vec[i]
    
    for (t in 1:nsim) {
      if (t %% 100 == 0 || nsim <= 10) {
        cat("n =", n, "rep =", t, "\n")
      }
      
      repeat {
        data <- generate_data(n,
                              parA = pars$parA,
                              parM = pars$parM,
                              parL = pars$parL,
                              parY = pars$parY)$data
        output <- try(estimate_eff_single(data = data, fit_eff_nuisance(data), a = 1), silent = TRUE)
        if (!inherits(output, "try-error")) break
      }
      
      est_A_onestep_a1[t, i, s] <- output$Onestep$estimated_psi
      est_A_tmle_a1[t, i, s] <- output$TMLE$estimated_psi
      
      df_A[t, i, s] <- as.integer(
        est_A_onestep_a1[t, i, s] < 0 |
          est_A_onestep_a1[t, i, s] > 1
      )
      
    }
  }
}

prop_oob_A <- apply(
  df_A,
  MARGIN = c(2, 3),
  FUN = mean
)
prop_oob_A

library(ggplot2)

plot_A_a1 <- data.frame(
  scenario = rep(scenario_names, each = nsim),
  OneStep = as.vector(est_A_onestep_a1[, 1, ]),
  TMLE = as.vector(est_A_tmle_a1[, 1, ])
) |>
  tidyr::pivot_longer(
    -scenario,
    names_to = "method",
    values_to = "estimate"
  ) |>
  transform(
    scenario = factor(scenario, levels = scenario_names),
    method = factor(
      method,
      levels = c("OneStep", "TMLE"),
      labels = c("One-step", "TMLE")
  )
)

plot_A_a1_mean <- aggregate(
  estimate ~ scenario + method,
  data = plot_A_a1,
  FUN = mean
)

p <- ggplot(plot_A_a1, aes(scenario, estimate, color = method)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 1,
           fill = "grey80", alpha = 0.35) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.5),
    alpha = 0.5, size = 1.2
  ) +
  # geom_point(
  #   data = plot_A_a1_mean,
  #   aes(scenario, estimate, group = method),
  #   inherit.aes = FALSE,
  #   position = position_dodge(width = 0.5),
  #   color = "black", shape = 16, size = 4
  # ) +
  geom_segment(
    data = truth_df,
    aes(x = x - 0.25, xend = x + 0.25, y = truth, yend = truth),
    inherit.aes = FALSE, linewidth = 0.3, linetype = "dashed", color = "black"
  ) +
  geom_text(
    data = data.frame(
      scenario = factor(scenario_names, levels = scenario_names),
      label = paste0("OOB: ", scales::percent(prop_oob_A[1, ], accuracy = 0.1))
    ),
    aes(scenario, -0.1, label = label),
    inherit.aes = FALSE, size = 3
  ) +
  labs(
    x = "Treatment positivity scenario",
    y = expression(paste("Estimates of ", E(Y^1))),
    color = "Estimator"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    axis.title.x = element_text(margin = margin(t = 12)),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

p

ggsave("OOB.pdf", plot = p, width = 10, height = 6, units = "in")

# OOB table
oob_table_pct <- cbind(
  sample_size = n.vec,
  apply(df_A, c(2, 3), mean, na.rm = TRUE)
) |>
  as.data.frame()

oob_table_pct[-1] <- lapply(oob_table_pct[-1], scales::percent, accuracy = 0.1)
oob_table_pct

# weak overlapping table
summarize_density_ratios <- function(pars, digits = 3) {
  parA <- pars$parA
  parM <- pars$parM
  parL <- pars$parL
  
  ## p(A = a1 | X) / p(A = a0 | X), with a0 = 1 and a1 = 0
  X <- 0:1
  pA1 <- parA[1] + parA[2] * X + parA[3] / 2
  ratio_A <- (1 - pA1) / pA1
  
  ## p(M | A = a0, X) / p(M | A = a1, X)
  ## Here a0 = 1 and a1 = 0
  grid_M <- expand.grid(M = 0:1, X = 0:1)
  
  pM_a0 <- dbinom(
    grid_M$M, 1,
    plogis(parM[1] + parM[2] * 1 + parM[3] * grid_M$X)
  )
  
  pM_a1 <- dbinom(
    grid_M$M, 1,
    plogis(parM[1] + parM[2] * 0 + parM[3] * grid_M$X)
  )
  
  ratio_M <- pM_a0 / pM_a1
  
  ## p(L | M, A = a0, X) / p(L | M, A = a1, X)
  grid_L <- expand.grid(L = 0:1, M = 0:1, X = 0:1)
  
  pL_a0 <- dbinom(
    grid_L$L, 1,
    plogis(
      parL[1] +
        parL[2] * grid_L$M +
        parL[3] * 1 +
        parL[4] * grid_L$X
    )
  )
  
  pL_a1 <- dbinom(
    grid_L$L, 1,
    plogis(
      parL[1] +
        parL[2] * grid_L$M +
        parL[3] * 0 +
        parL[4] * grid_L$X
    )
  )
  
  ratio_L <- pL_a0 / pL_a1
  
  format_range <- function(x) {
    paste0(
      "[",
      paste(round(range(x), digits), collapse = ", "),
      "]"
    )
  }
  
  data.frame(
    A_ratio_range = format_range(ratio_A),
    A_denominator_range = format_range(pA1),
    M_ratio_range = format_range(ratio_M),
    M_denominator_range = format_range(pM_a1),
    L_ratio_range = format_range(ratio_L),
    L_denominator_range = format_range(pL_a1)
  )
}

ratio_table <- do.call(
  rbind,
  lapply(scenario_list, summarize_density_ratios)
)

ratio_table$scenario <- rownames(ratio_table)
rownames(ratio_table) <- NULL

ratio_table <- ratio_table[
  ,
  c(
    "scenario",
    "A_ratio_range",
    "A_denominator_range",
    "M_ratio_range",
    "M_denominator_range",
    "L_ratio_range",
    "L_denominator_range"
  )
]

writeLines(
  capture.output(ratio_table),
  con = "weak-overlapping.txt"
)

save(list = c("prop_oob_A", 
              "est_A_onestep_a1", "est_A_tmle_a1", 
              "EY1_truth", "VAR.EY1_truth",
              "scenario_list","oob_table_pct","ratio_table"), file = 'results.Rdata')
