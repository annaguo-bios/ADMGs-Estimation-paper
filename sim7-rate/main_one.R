set.seed(7)

sim7.dir <- getwd()
source(file.path(sim7.dir, "DGPs", "YinL-dgp.R"), local = TRUE)
load(file.path(sim7.dir, "DGPs", "YinL-truth.Rdata"))
source(file.path(sim7.dir, "estimate.R"), local = TRUE)
source(file.path(sim7.dir, "organize.R"), local = TRUE)

seq.rates <- c(seq2over5 = 2/5,seq1over3 = 1/3, seq1over6 = 1/6, seq1over9 = 1/9, rate1over10 = 1/10)
ratio.rates <- c(seq2over5 = 1/5, seq1over3 = 1/6, seq1over6 = 1/8, seq1over9 = 1/10, rate1over10 = 1/15)


# Run from this folder with Rscript main_one.R.
run_scenario <- function(scenario, seq.rate, ratio.rate,
                         n.vec = c(250, 500, 1000, 2000, 4000, 8000),
                         nsim = 1000L, ncores = 6) {
  log.power <- 1/30

  est_matrix_ate_tmle <- matrix(NA_real_, nsim, length(n.vec))
  ci_matrix_ate_lower_tmle <- matrix(NA_real_, nsim, length(n.vec))
  ci_matrix_ate_upper_tmle <- matrix(NA_real_, nsim, length(n.vec))
  ifvar_matrix_ate_tmle <- matrix(NA_real_, nsim, length(n.vec))
  score_matrix <- matrix(NA_real_, nsim, length(n.vec))
  iteration_matrix <- matrix(NA_integer_, nsim, length(n.vec))
  est_matrix_ate_onestep <- matrix(NA_real_, nsim, length(n.vec))
  ci_matrix_ate_lower_onestep <- matrix(NA_real_, nsim, length(n.vec))
  ci_matrix_ate_upper_onestep <- matrix(NA_real_, nsim, length(n.vec))
  ifvar_matrix_ate_onestep <- matrix(NA_real_, nsim, length(n.vec))

  for (i in seq_along(n.vec)) {
    n <- n.vec[i]
    cat("Scenario:", scenario, " n =", n, " replications =", nsim, "\n")

    run_one_simulation <- function(t) {
      simulated <- generate_data(n)
      data <- simulated$data
      output <- estimate_eff(data, seq.rate = seq.rate, ratio.rate = ratio.rate,
                               log.power = log.power)
        
      # Keep scalar summaries rather than every observation's EIF in memory.
      output$TMLE$ifvar <- mean(output$TMLE$EIF^2)
      output$Onestep$ifvar <- mean(output$Onestep$EIF^2)
      output$TMLE$EIF <- NULL
      output$Onestep$EIF <- NULL
      list(TMLE = output$TMLE,
           Onestep = output$Onestep)
    }

    outputs <- parallel::mclapply(seq_len(nsim), run_one_simulation,
                                  mc.cores = ncores)

    for (t in seq_len(nsim)) {
      output <- outputs[[t]]
      score_matrix[t, i] <- output$TMLE$max.score
      iteration_matrix[t, i] <- output$TMLE$iter

      est_matrix_ate_tmle[t, i] <- output$TMLE$ACE
      ci_matrix_ate_lower_tmle[t, i] <- output$TMLE$lower.ci
      ci_matrix_ate_upper_tmle[t, i] <- output$TMLE$upper.ci
      ifvar_matrix_ate_tmle[t, i] <- output$TMLE$ifvar

      est_matrix_ate_onestep[t, i] <- output$Onestep$ACE
      ci_matrix_ate_lower_onestep[t, i] <- output$Onestep$lower.ci
      ci_matrix_ate_upper_onestep[t, i] <- output$Onestep$upper.ci
      ifvar_matrix_ate_onestep[t, i] <- output$Onestep$ifvar
    }
  }

  scenario.dir <- file.path(sim7.dir, scenario)
  dir.create(file.path(scenario.dir, "TMLE"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(scenario.dir, "Onestep"), recursive = TRUE, showWarnings = FALSE)

  save_results(est_matrix_ate_tmle,
               ci_matrix_ate_lower_tmle, ci_matrix_ate_upper_tmle,
               ifvar_matrix_ate_tmle,
               score_matrix, iteration_matrix,
               file.path(scenario.dir, "TMLE", "result.Rdata"),
               n.vec, nsim, ATE, VAR.ATE, seq.rate, ratio.rate,
               log.power = log.power)

  save_results(est_matrix_ate_onestep,
               ci_matrix_ate_lower_onestep, ci_matrix_ate_upper_onestep,
               ifvar_matrix_ate_onestep,
               score_matrix, iteration_matrix,
               file.path(scenario.dir, "Onestep", "result.Rdata"),
               n.vec, nsim, ATE, VAR.ATE, seq.rate, ratio.rate,
               log.power = log.power)

}

cases <- names(seq.rates)

for (scenario in cases) {
  run_scenario(scenario, seq.rate = seq.rates[[scenario]], ratio.rate = ratio.rates[[scenario]])
}
