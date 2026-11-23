# Estimate both treatment-arm means, then take their difference.
estimate_eff <- function(data, a = c(1, 0), seq.rate = 1 / 3, ratio.rate = 1 / 6,
                          perturb = TRUE, log.power = 0) {
  
  # 1. Fit the shared nuisance models. U is latent, so exclude it.
  data <- data[, c("X", "A", "M.1", "M.2", "L", "Y")]
  n <- nrow(data)
  X <- data$X
  # perturbation scale
  x.scale.tmle <- 1.2*X
  x.scale.onestep <- 0.43*X
  A <- data$A
  M <- as.matrix(data[, c("M.1", "M.2")])
  L <- data$L
  Y <- data$Y

  fitA <- glm(A ~ X, family = binomial(), data = data)
  fitM <- lm(cbind(M.1, M.2) ~ A + X, data = data)
  fitL <- lm(L ~ A + M.1 + M.2 + X, data = data)
  # E[Y | L, M, A, X] includes both X and the latent-confounding term.
  fitY <- lm(Y ~ L + M.1 + M.2 + X + I(A - plogis(1 + X)), data = data)
  
  # Bayes
  bayesM <- glm(A ~ M.1 + M.2 + X, family = binomial(), data = data)
  bayesL <- glm(A ~ L + M.1 + M.2 + X, family = binomial(), data = data)
  
  base.nuisance <- list(fitA = fitA, fitM = fitM, fitL = fitL, fitY = fitY, bayesM = bayesM, bayesL = bayesL,
                        sigma.M = cov(residuals(fitM)), sd.L = sd(residuals(fitL)))

  fitted.p1 <- predict(fitA, type = "response")
  bayesM_p1 <- predict(bayesM, type = "response")
  bayesL_p1 <- predict(bayesL, type = "response")

  muY.error <- muL.error <- muM.error <- 0
  pi.error <- rM.error <- rL.error <- 0
  if (perturb) {
    seq.scale <- n^(-seq.rate) / log(n)^log.power
    ratio.scale <- n^(-ratio.rate) / log(n)^log.power
    # Share each nuisance's Gaussian draws across the two estimators.
    muY.error <- rnorm(1, mean = seq.scale, sd = seq.scale)
    muL.error <- rnorm(1, mean = seq.scale, sd = seq.scale)
    muM.error <- rnorm(1, mean = seq.scale, sd = seq.scale)
    pi.error <- rnorm(1, mean = ratio.scale, sd = ratio.scale)
    rM.error <- rnorm(1, mean = ratio.scale, sd = ratio.scale)
    rL.error <- rnorm(1, mean = ratio.scale, sd = ratio.scale)
  }

  perturb_nuisance <- function(nuis, x.scale) {
    p1.perturbed <- plogis(qlogis(fitted.p1) + pi.error * x.scale)
    nuis$p.a1.X <- nuis$a1 * p1.perturbed + (1 - nuis$a1) * (1 - p1.perturbed)
    nuis$rM <- exp(log(nuis$rM) + rM.error * x.scale)
    nuis$rL <- exp(log(nuis$rL) + rL.error * x.scale)
    nuis$mu.Y <- nuis$mu.Y + muY.error * x.scale
    nuis$mu.L <- nuis$mu.L + muL.error * x.scale
    nuis$mu.M <- nuis$mu.M + muM.error * x.scale
    nuis
  }
  
  cvg.criteria <- n^(-1 / 2) / log(n)^(1 / 4)
  arm.results <- vector("list", 2)

  for (arm in 1:2) {
    # 2. Initialize this arm's predictions and density ratios.
    a0 <- a[arm]
    a1 <- 1 - a0
    data.a0 <- data
    data.a0$A <- a0
    data.a1 <- data
    data.a1$A <- a1

    nuis <- list(a0 = a0, a1 = a1)

    # Regress predicted outcomes backward through L, then M.
    # a1 stays fixed in mu.Y; retain its nonlinear X term in both regressions.
    # Set A to a0 only to integrate the L and M mechanisms under a0.
    nuis$mu.Y <- predict(fitY, newdata = data.a1) # Y in the district of A, thus a_Y=a1
    next.mu <- nuis$mu.Y
    fit <- lm(next.mu ~ A + M.1 + M.2 + X + I(plogis(1 + X)), data = data)
    nuis$mu.L <- predict(fit, newdata = data.a0) # L not in the district of A, thus a_L=a0
    next.mu <- nuis$mu.L
    fit <- lm(next.mu ~ A + X + I(plogis(1 + X)), data = data)
    nuis$mu.M <- predict(fit, newdata = data.a0) # M not in the district of A, thus a_M=a0

    # Bayes density ratios use the fitted treatment probabilities.
    bayesM_pred.a0 <- (a0 * bayesM_p1 + (1 - a0) * (1 - bayesM_p1)) /
      (a0 * fitted.p1 + (1 - a0) * (1 - fitted.p1))
    bayesM_pred.a1 <- (a1 * bayesM_p1 + (1 - a1) * (1 - bayesM_p1)) /
      (a1 * fitted.p1 + (1 - a1) * (1 - fitted.p1))
    bayesL_pred.a0 <- (a0 * bayesL_p1 + (1 - a0) * (1 - bayesL_p1)) /
      (a0 * bayesM_p1 + (1 - a0) * (1 - bayesM_p1))
    bayesL_pred.a1 <- (a1 * bayesL_p1 + (1 - a1) * (1 - bayesL_p1)) /
      (a1 * bayesM_p1 + (1 - a1) * (1 - bayesM_p1))

    nuis$rM <- bayesM_pred.a0 / bayesM_pred.a1
    nuis$rL <- bayesL_pred.a0 / bayesL_pred.a1

    # 3. Perturb the nuisance functions for one-step estimation.
    nuis.onestep <- perturb_nuisance(nuis, x.scale = x.scale.onestep)
    comp <- compute_components(data, nuis.onestep)
    onestep <- summarize_psi(comp$estimated_psi, comp$EIF, n)
    onestep$plugin <- comp$psi

    # 4. Target A, Y, L, and M in order until the mean EIF is small.
    nuis <- perturb_nuisance(nuis, x.scale = x.scale.tmle)
    muL.delta <- muL.error * x.scale.tmle
    muM.delta <- muM.error * x.scale.tmle
    EDstar <- 10
    EDstar.record <- numeric(0)
    iter <- 0
    while (abs(EDstar) > cvg.criteria && iter < 500) {
      propensity.offset <- qlogis(nuis$p.a1.X)
      eps.A <- fit_expit_fluctuation(A == a1, propensity.offset, nuis$mu.M)
      nuis$p.a1.X <- plogis(propensity.offset + eps.A * nuis$mu.M)

      comp <- compute_components(data, nuis)
      eps.Y <- fit_linear_fluctuation(Y, nuis$mu.Y, (A == a1) * nuis$rM * nuis$rL)
      nuis$mu.Y <- nuis$mu.Y + eps.Y

      # Refit each regression to the updated next outcome, then fluctuate.
      next.mu <- nuis$mu.Y
      fit <- lm(next.mu ~ A + M.1 + M.2 + X + I(plogis(1 + X)), data = data)
      nuis$mu.L <- predict(fit, newdata = data.a0) + muL.delta
      eps.L <- fit_linear_fluctuation(nuis$mu.Y, nuis$mu.L, (A == a0) * comp$rA)
      nuis$mu.L <- nuis$mu.L + eps.L

      next.mu <- nuis$mu.L
      fit <- lm(next.mu ~ A + X + I(plogis(1 + X)), data = data)
      nuis$mu.M <- predict(fit, newdata = data.a0) + muM.delta
      eps.M <- fit_linear_fluctuation(nuis$mu.L, nuis$mu.M, (A == a0) * comp$rA)
      nuis$mu.M <- nuis$mu.M + eps.M

      comp <- compute_components(data, nuis)
      EDstar <- comp$EDstar
      EDstar.record <- c(EDstar.record, EDstar)
      iter <- iter + 1L
    }

    tmle <- summarize_psi(comp$psi, comp$EIF, n)
    tmle$plugin <- comp$psi
    tmle$EDstar <- EDstar
    tmle$iter <- iter
    tmle$EDstar.record <- EDstar.record
    tmle$cvg.criteria <- cvg.criteria
    arm.results[[arm]] <- list(Onestep = onestep, TMLE = tmle)
  }

  # 5. Combine the two arms and their EIFs to estimate the ACE and its CI.
  Y1 <- arm.results[[1]]
  Y0 <- arm.results[[2]]
  onestep.ate <- Y1$Onestep$estimated_psi - Y0$Onestep$estimated_psi
  onestep.eif <- Y1$Onestep$EIF - Y0$Onestep$EIF
  onestep <- summarize_psi(onestep.ate, onestep.eif, nrow(data))
  names(onestep)[names(onestep) == "estimated_psi"] <- "ACE"

  tmle.ate <- Y1$TMLE$estimated_psi - Y0$TMLE$estimated_psi
  tmle.eif <- Y1$TMLE$EIF - Y0$TMLE$EIF
  tmle <- summarize_psi(tmle.ate, tmle.eif, nrow(data))
  names(tmle)[names(tmle) == "estimated_psi"] <- "ACE"
  tmle$max.score <- max(abs(Y1$TMLE$EDstar), abs(Y0$TMLE$EDstar))
  tmle$iter <- max(Y1$TMLE$iter, Y0$TMLE$iter)

  list(Onestep = onestep,
       TMLE = tmle,
       nuisance = base.nuisance)
}


# Reused calculations for estimation and targeting.
compute_components <- function(data, nuis) {
  A <- data$A
  Y <- data$Y
  a0 <- nuis$a0
  a1 <- nuis$a1

  p.a1.X <- nuis$p.a1.X
  p.a0.X <- 1 - p.a1.X
  rA <- p.a1.X / p.a0.X
  rM <- nuis$rM
  rL <- nuis$rL

  mu.Y <- nuis$mu.Y
  mu.L <- nuis$mu.L
  mu.M <- nuis$mu.M
  mu.A <- p.a1.X * mu.M
  psi <- mean(mu.A + (A == a0) * Y)

  EIF.A <- ((A == a1) - p.a1.X) * mu.M
  EIF.M <- (A == a0) * rA * (mu.L - mu.M)
  EIF.L <- (A == a0) * rA * (mu.Y - mu.L)
  EIF.Y <- (A == a1) * rM * rL * (Y - mu.Y)
  
  # As in flexCausal, center the EIF at the corrected estimate, not the plug-in.
  pseudo.outcome <- EIF.Y + EIF.L + EIF.M + EIF.A + mu.A + (A == a0) * Y
  estimated_psi <- mean(pseudo.outcome)
  EIF <- pseudo.outcome - estimated_psi

  list(psi = psi,
       estimated_psi = estimated_psi,
       EIF = EIF,
       EIF.A = EIF.A,
       EIF.M = EIF.M,
       EIF.L = EIF.L,
       EIF.Y = EIF.Y,
       mu.Y = mu.Y,
       mu.L = mu.L,
       mu.M = mu.M,
       rA = rA,
       rM = rM,
       rL = rL,
       EDstar = mean(EIF.A) + mean(EIF.Y) + mean(EIF.L + EIF.M))
}

fit_expit_fluctuation <- function(y, offset, clever) {
  # Solve the score of the one-parameter logistic targeting model.
  score <- function(eps) sum(clever * (y - plogis(offset + eps * clever)))
  uniroot(score, interval = c(-1, 1), extendInt = "downX", tol = 1e-10)$root
}

fit_linear_fluctuation <- function(y, offset, weights) {
  # Weighted intercept-only regression with a fixed offset.
  sum(weights * (y - offset)) / sum(weights)
}

summarize_psi <- function(psi, EIF, n, level = 0.95) {
  z <- if (level == 0.95) 1.96 else qnorm(1 - (1 - level) / 2)
  EIF <- EIF - mean(EIF)
  se <- sqrt(mean(EIF^2) / n)

  list(estimated_psi = psi,
       lower.ci = psi - z * se,
       upper.ci = psi + z * se,
       EIF = EIF)
}

