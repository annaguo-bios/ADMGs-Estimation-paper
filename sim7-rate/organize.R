summarize_errors <- function(estimates, truth, n.vec) {
  bias <- variance <- mse <- data.frame(n = n.vec, value = NA_real_,
                                        upper = NA_real_, lower = NA_real_)

  interval <- function(values) {
    estimate <- mean(values)
    margin <- 1.96 * sd(values) / sqrt(length(values))
    c(estimate, estimate + margin, estimate - margin)
  }

  for (i in seq_along(n.vec)) {
    errors <- estimates[, i] - truth
    errors <- errors[is.finite(errors)]
    if (length(errors) < 2L) next

    n <- n.vec[i]
    bias[i, -1] <- interval(sqrt(n) * errors)
    variance[i, -1] <- interval(n * (errors - mean(errors))^2)
    mse[i, -1] <- interval(n * errors^2)
  }

  list(bias = bias, variance = variance, mse = mse)
}

save_results <- function(est_matrix_ate,
                         ci_matrix_ate_lower,
                         ci_matrix_ate_upper,
                         ifvar_matrix_ate,
                         score_matrix,
                         iteration_matrix,
                         out.name,
                         n.vec,
                         nsim,
                         ATE,
                         VAR.ATE,
                         seq.rate,
                         ratio.rate,
                         log.power = 0) {
  bias_matrix_ate <- est_matrix_ate - ATE

  avg.ate <- data.frame(n = n.vec, est = NA_real_, sd = NA_real_,
                        upper = NA_real_, lower = NA_real_)
  errors <- summarize_errors(est_matrix_ate, ATE, n.vec)
  avg.bias_ate <- setNames(errors$bias, c("n", "bias", "upper", "lower"))
  avg.variance_ate <- setNames(errors$variance, c("n", "variance", "upper", "lower"))
  avg.MSE_ate <- setNames(errors$mse, c("n", "mse", "upper", "lower"))
  ci_coverage_ATE <- data.frame(n = n.vec, coverage = NA_real_,
                                coverage.lower = NA_real_, coverage.upper = NA_real_)
  diagnostics <- data.frame(n = n.vec, successful = 0L)

  for (i in seq_along(n.vec)) {
    nsim.ok <- sum(is.finite(est_matrix_ate[, i]))
    diagnostics$successful[i] <- nsim.ok
    if (nsim.ok < 2) {
      next
    }

    avg.ate[i, "est"] <- mean(est_matrix_ate[, i], na.rm = TRUE)
    avg.ate[i, "sd"] <- sd(est_matrix_ate[, i], na.rm = TRUE) / sqrt(nsim.ok)
    avg.ate[i, "upper"] <- avg.ate[i, "est"] + 1.96 * avg.ate[i, "sd"]
    avg.ate[i, "lower"] <- avg.ate[i, "est"] - 1.96 * avg.ate[i, "sd"]

    covered <- ci_matrix_ate_lower[, i] <= ATE & ci_matrix_ate_upper[, i] >= ATE
    coverage <- mean(covered, na.rm = TRUE)
    coverage.se <- sqrt(coverage * (1 - coverage) / nsim.ok)
    ci_coverage_ATE[i, "coverage"] <- coverage
    ci_coverage_ATE[i, "coverage.lower"] <- coverage - 1.96 * coverage.se
    ci_coverage_ATE[i, "coverage.upper"] <- coverage + 1.96 * coverage.se
  }

  avg.ifvariance_ate <- data.frame(n = n.vec, variance = colMeans(ifvar_matrix_ate, na.rm = TRUE))
  perturbation <- "Gaussian"

  save(est_matrix_ate, bias_matrix_ate,
       ci_matrix_ate_lower, ci_matrix_ate_upper, ifvar_matrix_ate,
       score_matrix, iteration_matrix,
       avg.ate, avg.bias_ate, avg.variance_ate, avg.MSE_ate, ci_coverage_ATE,
       avg.ifvariance_ate, diagnostics,
       n.vec, nsim, ATE, VAR.ATE, seq.rate, ratio.rate, log.power, perturbation,
       file = out.name)
}
