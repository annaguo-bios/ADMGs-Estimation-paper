args <- commandArgs(trailingOnly = TRUE)

n.vec <- c(250, 500, 1000, 2000, 4000, 8000)
nsim <- 1000

dgp.f.name <- "semieff-dgp.R"
truth <- "semieff-truth.Rdata"
a <- c(1, 0)

vertices <- c("X", "A", "L", "M", "Y")
di_edges <- list(c("X", "A"), c("X", "M"), c("X", "L"), c("X", "Y"),
                 c("A", "M"), c("M", "L"), c("M", "Y"), c("L", "Y"))
bi_edges <- list(c("A", "Y"))
treatment <- "A"
outcome <- "Y"
ratio.method.L <- "dnorm"
ratio.method.M <- "dnorm"

suppressPackageStartupMessages({
  library(flexCausal)
})

source(file.path("DGPs", dgp.f.name))
source("estimate-eff.R")
load(file.path("DGPs", truth))

run_one_estimate <- function(data) {
  # run package estimators
  time.np <- system.time({
    suppressMessages(
      output.np <- estADMG(a = a, data = data, vertices = vertices,
                           bi_edges = bi_edges,
                           di_edges = di_edges,
                           treatment = treatment, outcome = outcome,
                           ratio.method.L = ratio.method.L,
                           ratio.method.M = ratio.method.M,
                           formulaA = "A ~ X",
                           formulaY = "Y ~ L + M + A * X")
    )
  })[["elapsed"]]

  # run custom estimators using the semiparametric efficient EIF
  time.eff <- system.time({
    output.eff <- estimate_eff(data = data, a = a)
  })[["elapsed"]]

  # TMLE estimates from nonparametric package estimator and custom script
  tmle <- list(
    hat_ATE.np = output.np$TMLE$ACE,
    lower.ci_ATE.np = output.np$TMLE$lower.ci,
    upper.ci_ATE.np = output.np$TMLE$upper.ci,
    bias_ATE.np = output.np$TMLE$ACE - ATE,
    ifvar_ATE.np = mean(output.np$TMLE$EIF^2),

    hat_ATE.eff = output.eff$TMLE$ACE,
    lower.ci_ATE.eff = output.eff$TMLE$lower.ci,
    upper.ci_ATE.eff = output.eff$TMLE$upper.ci,
    bias_ATE.eff = output.eff$TMLE$ACE - ATE,
    ifvar_ATE.eff = mean(output.eff$TMLE$EIF^2),

    time.np = time.np,
    time.eff = time.eff
  )

  tmle$hat_ATE <- tmle$hat_ATE.eff
  tmle$lower.ci_ATE <- tmle$lower.ci_ATE.eff
  tmle$upper.ci_ATE <- tmle$upper.ci_ATE.eff
  tmle$bias_ATE <- tmle$bias_ATE.eff

  # one-step estimates from nonparametric package estimator and custom script
  onestep <- list(
    hat_ATE.np = output.np$Onestep$ACE,
    lower.ci_ATE.np = output.np$Onestep$lower.ci,
    upper.ci_ATE.np = output.np$Onestep$upper.ci,
    bias_ATE.np = output.np$Onestep$ACE - ATE,
    ifvar_ATE.np = mean(output.np$Onestep$EIF^2),

    hat_ATE.eff = output.eff$Onestep$ACE,
    lower.ci_ATE.eff = output.eff$Onestep$lower.ci,
    upper.ci_ATE.eff = output.eff$Onestep$upper.ci,
    bias_ATE.eff = output.eff$Onestep$ACE - ATE,
    ifvar_ATE.eff = mean(output.eff$Onestep$EIF^2),

    time.np = time.np,
    time.eff = time.eff
  )

  onestep$hat_ATE <- onestep$hat_ATE.eff
  onestep$lower.ci_ATE <- onestep$lower.ci_ATE.eff
  onestep$upper.ci_ATE <- onestep$upper.ci_ATE.eff
  onestep$bias_ATE <- onestep$bias_ATE.eff

  return(list(TMLE = tmle, Onestep = onestep))
}

make_empty_matrix <- function() {
  matrix(NA_real_, nrow = nsim, ncol = length(n.vec))
}

initialize_output <- function() {
  list(
    bias_matrix_ate = make_empty_matrix(),
    bias_matrix_ate_np = make_empty_matrix(),
    bias_matrix_ate_eff = make_empty_matrix(),

    est_matrix_ate = make_empty_matrix(),
    est_matrix_ate_np = make_empty_matrix(),
    est_matrix_ate_eff = make_empty_matrix(),

    ci_matrix_ate_lower = make_empty_matrix(),
    ci_matrix_ate_lower_np = make_empty_matrix(),
    ci_matrix_ate_lower_eff = make_empty_matrix(),

    ci_matrix_ate_upper = make_empty_matrix(),
    ci_matrix_ate_upper_np = make_empty_matrix(),
    ci_matrix_ate_upper_eff = make_empty_matrix(),

    runtime_matrix_np = make_empty_matrix(),
    runtime_matrix_eff = make_empty_matrix(),

    ifvar_matrix_ate_np = make_empty_matrix(),
    ifvar_matrix_ate_eff = make_empty_matrix()
  )
}

save_one_result <- function(out, result.file) {
  bias_matrix_ate <- out$bias_matrix_ate
  bias_matrix_ate_np <- out$bias_matrix_ate_np
  bias_matrix_ate_eff <- out$bias_matrix_ate_eff

  est_matrix_ate <- out$est_matrix_ate
  est_matrix_ate_np <- out$est_matrix_ate_np
  est_matrix_ate_eff <- out$est_matrix_ate_eff

  ci_matrix_ate_lower <- out$ci_matrix_ate_lower
  ci_matrix_ate_lower_np <- out$ci_matrix_ate_lower_np
  ci_matrix_ate_lower_eff <- out$ci_matrix_ate_lower_eff

  ci_matrix_ate_upper <- out$ci_matrix_ate_upper
  ci_matrix_ate_upper_np <- out$ci_matrix_ate_upper_np
  ci_matrix_ate_upper_eff <- out$ci_matrix_ate_upper_eff

  runtime_matrix_np <- out$runtime_matrix_np
  runtime_matrix_eff <- out$runtime_matrix_eff
  ifvar_matrix_ate_np <- out$ifvar_matrix_ate_np
  ifvar_matrix_ate_eff <- out$ifvar_matrix_ate_eff

  summary_np <- summarize_estimator("np EIF", bias_matrix_ate_np, est_matrix_ate_np,
                                    ci_matrix_ate_lower_np, ci_matrix_ate_upper_np)
  summary_eff <- summarize_estimator("semiparametric IF", bias_matrix_ate_eff, est_matrix_ate_eff,
                                     ci_matrix_ate_lower_eff, ci_matrix_ate_upper_eff)

  ci_coverage_ATE.by_estimator <- rbind(summary_np$coverage, summary_eff$coverage)
  avg.ate.by_estimator <- rbind(summary_np$avg.ate, summary_eff$avg.ate)
  avg.bias_ate.by_estimator <- rbind(summary_np$avg.bias_ate, summary_eff$avg.bias_ate)
  avg.variance_ate.by_estimator <- rbind(summary_np$avg.variance_ate, summary_eff$avg.variance_ate)
  avg.MSE_ate.by_estimator <- rbind(summary_np$avg.MSE_ate, summary_eff$avg.MSE_ate)

  ci_coverage_ATE <- summary_eff$coverage[, c("n", "coverage", "coverage.lower", "coverage.upper")]
  avg.ate <- summary_eff$avg.ate[, c("n", "est", "sd", "upper", "lower")]
  avg.bias_ate <- summary_eff$avg.bias_ate[, c("n", "bias", "upper", "lower")]
  avg.variance_ate <- summary_eff$avg.variance_ate[, c("n", "variance", "upper", "lower")]
  avg.MSE_ate <- summary_eff$avg.MSE_ate[, c("n", "mse", "upper", "lower")]

  avg.ifvariance_ate <- data.frame(
    n = rep(n.vec, 2),
    estimator = rep(c("np EIF", "semiparametric IF"), each = length(n.vec)),
    variance = c(colMeans(ifvar_matrix_ate_np, na.rm = TRUE),
                 colMeans(ifvar_matrix_ate_eff, na.rm = TRUE))
  )

  avg.runtime <- rbind("np EIF" = colMeans(runtime_matrix_np, na.rm = TRUE),
                       "semiparametric IF" = colMeans(runtime_matrix_eff, na.rm = TRUE))
  colnames(avg.runtime) <- n.vec

  save(list = c("bias_matrix_ate",
                "bias_matrix_ate_np",
                "bias_matrix_ate_eff",
                "est_matrix_ate",
                "est_matrix_ate_np",
                "est_matrix_ate_eff",
                "ci_matrix_ate_lower",
                "ci_matrix_ate_lower_np",
                "ci_matrix_ate_lower_eff",
                "ci_matrix_ate_upper",
                "ci_matrix_ate_upper_np",
                "ci_matrix_ate_upper_eff",
                "runtime_matrix_np",
                "runtime_matrix_eff",
                "ifvar_matrix_ate_np",
                "ifvar_matrix_ate_eff",
                "ci_coverage_ATE",
                "ci_coverage_ATE.by_estimator",
                "avg.ate",
                "avg.ate.by_estimator",
                "avg.bias_ate",
                "avg.bias_ate.by_estimator",
                "avg.variance_ate",
                "avg.variance_ate.by_estimator",
                "avg.MSE_ate",
                "avg.MSE_ate.by_estimator",
                "avg.ifvariance_ate",
                "avg.runtime"),
       file = result.file)
}

summarize_estimator <- function(estimator.name, bias_matrix, est_matrix, ci_lower, ci_upper) {
  coverage <- data.frame(n = n.vec, estimator = estimator.name,
                         coverage = NA_real_, coverage.lower = NA_real_, coverage.upper = NA_real_)
  avg.ate <- data.frame(n = n.vec, estimator = estimator.name,
                        est = NA_real_, sd = NA_real_, upper = NA_real_, lower = NA_real_)
  avg.bias_ate <- data.frame(n = n.vec, estimator = estimator.name,
                             bias = NA_real_, upper = NA_real_, lower = NA_real_)
  avg.variance_ate <- data.frame(n = n.vec, estimator = estimator.name,
                                 variance = NA_real_, upper = NA_real_, lower = NA_real_)
  avg.MSE_ate <- data.frame(n = n.vec, estimator = estimator.name,
                            mse = NA_real_, upper = NA_real_, lower = NA_real_)

  for (i in seq_along(n.vec)) {
    n <- n.vec[i]
    nsim.ok <- sum(!is.na(bias_matrix[, i]))

    coverage[i, "coverage"] <- mean((ci_lower[, i] < ATE) & (ci_upper[, i] > ATE), na.rm = TRUE)
    coverage[i, "coverage.lower"] <- coverage[i, "coverage"] -
      1.96 * sqrt(coverage[i, "coverage"] * (1 - coverage[i, "coverage"]) / nsim.ok)
    coverage[i, "coverage.upper"] <- coverage[i, "coverage"] +
      1.96 * sqrt(coverage[i, "coverage"] * (1 - coverage[i, "coverage"]) / nsim.ok)

    avg.ate[i, "est"] <- mean(est_matrix[, i], na.rm = TRUE)
    avg.ate[i, "sd"] <- sqrt(var(est_matrix[, i], na.rm = TRUE) / nsim.ok)
    avg.ate[i, "upper"] <- avg.ate[i, "est"] + 1.96 * avg.ate[i, "sd"]
    avg.ate[i, "lower"] <- avg.ate[i, "est"] - 1.96 * avg.ate[i, "sd"]

    avg.bias_ate[i, "bias"] <- sqrt(n) * mean(bias_matrix[, i], na.rm = TRUE)
    avg.bias_ate[i, "upper"] <- avg.bias_ate[i, "bias"] +
      1.96 * sqrt(var(sqrt(n) * bias_matrix[, i], na.rm = TRUE) / nsim.ok)
    avg.bias_ate[i, "lower"] <- avg.bias_ate[i, "bias"] -
      1.96 * sqrt(var(sqrt(n) * bias_matrix[, i], na.rm = TRUE) / nsim.ok)

    bias.centered <- bias_matrix[, i] - mean(bias_matrix[, i], na.rm = TRUE)
    avg.variance_ate[i, "variance"] <- n * mean(bias.centered^2, na.rm = TRUE)
    avg.variance_ate[i, "upper"] <- avg.variance_ate[i, "variance"] +
      1.96 * sqrt(var(n * bias.centered^2, na.rm = TRUE) / nsim.ok)
    avg.variance_ate[i, "lower"] <- avg.variance_ate[i, "variance"] -
      1.96 * sqrt(var(n * bias.centered^2, na.rm = TRUE) / nsim.ok)

    avg.MSE_ate[i, "mse"] <- n * mean(bias_matrix[, i]^2, na.rm = TRUE)
    avg.MSE_ate[i, "upper"] <- avg.MSE_ate[i, "mse"] +
      1.96 * sqrt(var(n * bias_matrix[, i]^2, na.rm = TRUE) / nsim.ok)
    avg.MSE_ate[i, "lower"] <- avg.MSE_ate[i, "mse"] -
      1.96 * sqrt(var(n * bias_matrix[, i]^2, na.rm = TRUE) / nsim.ok)
  }

  return(list(coverage = coverage,
              avg.ate = avg.ate,
              avg.bias_ate = avg.bias_ate,
              avg.variance_ate = avg.variance_ate,
              avg.MSE_ate = avg.MSE_ate))
}

record_result <- function(out, est, t, i) {
  out$bias_matrix_ate[t, i] <- est$bias_ATE
  out$bias_matrix_ate_np[t, i] <- est$bias_ATE.np
  out$bias_matrix_ate_eff[t, i] <- est$bias_ATE.eff

  out$est_matrix_ate[t, i] <- est$hat_ATE
  out$est_matrix_ate_np[t, i] <- est$hat_ATE.np
  out$est_matrix_ate_eff[t, i] <- est$hat_ATE.eff

  out$ci_matrix_ate_lower[t, i] <- est$lower.ci_ATE
  out$ci_matrix_ate_lower_np[t, i] <- est$lower.ci_ATE.np
  out$ci_matrix_ate_lower_eff[t, i] <- est$lower.ci_ATE.eff

  out$ci_matrix_ate_upper[t, i] <- est$upper.ci_ATE
  out$ci_matrix_ate_upper_np[t, i] <- est$upper.ci_ATE.np
  out$ci_matrix_ate_upper_eff[t, i] <- est$upper.ci_ATE.eff

  out$runtime_matrix_np[t, i] <- est$time.np
  out$runtime_matrix_eff[t, i] <- est$time.eff

  out$ifvar_matrix_ate_np[t, i] <- est$ifvar_ATE.np
  out$ifvar_matrix_ate_eff[t, i] <- est$ifvar_ATE.eff

  return(out)
}

tmle_output <- initialize_output()
onestep_output <- initialize_output()

set.seed(7)

for (i in seq_along(n.vec)) {
  n <- n.vec[i]

  for (t in 1:nsim) {
    if (t %% 100 == 0 || nsim <= 10) {
      cat("n =", n, "rep =", t, "\n")
    }

    repeat {
      data <- generate_data(n)$data
      est <- try(run_one_estimate(data), silent = TRUE)
      if (!inherits(est, "try-error")) {
        break
      }
    }

    tmle_output <- record_result(tmle_output, est$TMLE, t, i)
    onestep_output <- record_result(onestep_output, est$Onestep, t, i)
  }
}

dir.create("TMLE", showWarnings = FALSE)
dir.create("Onestep", showWarnings = FALSE)

save_one_result(tmle_output, file.path("TMLE", "result.Rdata"))
save_one_result(onestep_output, file.path("Onestep", "result.Rdata"))

cat("Saved TMLE/result.Rdata and Onestep/result.Rdata\n")
