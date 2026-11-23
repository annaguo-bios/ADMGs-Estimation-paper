# Fit a one-dimensional logistic TMLE fluctuation with a supplied clever covariate.
fit_logistic_fluctuation <- function(y, offset, clever, weights = rep(1, length(y))) {
  fit <- glm(y ~ offset(offset) + clever - 1, family = binomial(),weights = weights, start = 0)
  coef(fit)[1]
}

# Fit a one-dimensional linear TMLE fluctuation with a supplied weighted offset.
fit_linear_fluctuation <- function(y, offset, weights = rep(1, length(y))) {
  fit <- lm(y ~ offset(offset) + 1, weights = weights)
  coef(fit)[1]
}

# Estimate nuisance quantities that are common to both treatment levels.
fit_eff_nuisance <- function(data) {
  data$X <- as.integer(data$X)
  data$A <- as.integer(data$A)
  data$M <- as.integer(data$M)
  data$L <- as.integer(data$L)

  pA1 <- numeric(2)
  for (x in 0:1) {
    pA1[x + 1] <- mean(data$A[data$X == x] == 1)
  }

  fitM <- glm(M ~ A + X, family = binomial(), data = data)
  pM1 <- matrix(NA_real_, nrow = 2, ncol = 2)
  for (a in 0:1) {
    for (x in 0:1) {
      pM1[a + 1, x + 1] <- predict(fitM, newdata = data.frame(A = a, X = x), type = "response")
    }
  }
  fitL <- glm(L ~ M + X, family = binomial(), data = data)
  pL1 <- array(NA_real_, dim = c(2, 2, 2))
  for (m in 0:1) {
    for (a in 0:1) {
      for (x in 0:1) {
        pL1[m + 1, a + 1, x + 1] <- predict(fitL, newdata = data.frame(M = m, X = x), type = "response")
      }
    }
  }

  # Bayes estimate of P(A = 0 | L, M, X) using fitted A|X, M|A,X, and L|M,A,X.
  pA0LMX <- array(NA_real_, dim = c(2, 2, 2))
  for (l in 0:1) {
    for (m in 0:1) {
      for (x in 0:1) {
        pA0X <- 1 - pA1[x + 1]
        pA1X <- pA1[x + 1]
        pM.A0X <- if (m == 1) pM1[1, x + 1] else 1 - pM1[1, x + 1]
        pM.A1X <- if (m == 1) pM1[2, x + 1] else 1 - pM1[2, x + 1]
        pL.MA0X <- if (l == 1) pL1[m + 1, 1, x + 1] else 1 - pL1[m + 1, 1, x + 1]
        pL.MA1X <- if (l == 1) pL1[m + 1, 2, x + 1] else 1 - pL1[m + 1, 2, x + 1]
        numerator.A0 <- pA0X * pM.A0X * pL.MA0X
        numerator.A1 <- pA1X * pM.A1X * pL.MA1X
        pA0LMX[l + 1, m + 1, x + 1] <- numerator.A0 / (numerator.A0 + numerator.A1)
      }
    }
  }

  fitY <- lm(Y ~ L + M + A * X, data = data)
  Q <- array(NA_real_, dim = c(2, 2, 2, 2))
  for (l in 0:1) {
    for (m in 0:1) {
      for (a in 0:1) {
        for (x in 0:1) {
          Q[l + 1, m + 1, a + 1, x + 1] <- predict(fitY,
                                                    newdata = data.frame(L = l,
                                                                         M = m,
                                                                         A = a,
                                                                         X = x))
        }
      }
    }
  }

  list(pA1 = pA1, pA0LMX = pA0LMX, pM1 = pM1, pL1 = pL1, Q = Q)
}

# Build treatment-specific sequential regressions for a single E[Y(a)] functional.
init_a_nuisance <- function(base, a) {
  a0 <- a
  a1 <- 1 - a

  p.a1.X <- if (a1 == 1) base$pA1 else 1 - base$pA1
  mu.L <- matrix(NA_real_, nrow = 2, ncol = 2)
  mu.M <- numeric(2)

  for (m in 0:1) {
    for (x in 0:1) {
      pL1 <- base$pL1[m + 1, a0 + 1, x + 1]
      mu.L[m + 1, x + 1] <- (1 - pL1) * base$Q[1, m + 1, a1 + 1, x + 1] +
        pL1 * base$Q[2, m + 1, a1 + 1, x + 1]
    }
  }

  for (x in 0:1) {
    pM1 <- base$pM1[a0 + 1, x + 1]
    mu.M[x + 1] <- (1 - pM1) * mu.L[1, x + 1] + pM1 * mu.L[2, x + 1]
  }

  list(base = base,
       a0 = a0,
       a1 = a1,
       p.a1.X = p.a1.X,
       mu.L = mu.L,
       mu.M = mu.M)
}

# Return P(A = a | X) using the current treatment-specific propensity nuisance.
pA <- function(nuis, a, x) {
  if (a == nuis$a1) {
    nuis$p.a1.X[x + 1]
  } else {
    1 - nuis$p.a1.X[x + 1]
  }
}

# Return P(M = m | A = a, X = x) from the fitted logistic M model.
pM_density <- function(nuis, m, a, x) {
  p1 <- nuis$base$pM1[cbind(a + 1, x + 1)]
  ifelse(m == 1, p1, 1 - p1)
}

# Return P(L = l | M = m, A = a, X = x) from the fitted logistic L model.
pL_density <- function(nuis, l, m, a, x) {
  p1 <- nuis$base$pL1[cbind(m + 1, a + 1, x + 1)]
  ifelse(l == 1, p1, 1 - p1)
}

# Look up the fitted outcome regression E[Y | L = l, M = m, A = a, X = x].
Q_lookup <- function(nuis, l, m, a, x) {
  n <- max(length(l), length(m), length(x))
  l <- rep(l, length.out = n)
  m <- rep(m, length.out = n)
  x <- rep(x, length.out = n)
  nuis$base$Q[cbind(l + 1, m + 1, rep(a + 1, n), x + 1)]
}

# Look up the sequential regression after integrating Q over L | M, A=a0, X.
muL_lookup <- function(nuis, m, x) {
  nuis$mu.L[cbind(m + 1, x + 1)]
}

# Look up the sequential regression after integrating mu.L over M | A=a0, X.
muM_lookup <- function(nuis, x) {
  nuis$mu.M[x + 1]
}

# Compute the plug-in parameter and named efficient EIF components.
compute_eff_components <- function(data, nuis) {
  X <- as.integer(data$X)
  A <- as.integer(data$A)
  M <- as.integer(data$M)
  L <- as.integer(data$L)
  Y <- data$Y

  a0 <- nuis$a0
  a1 <- nuis$a1

  p.a1.X <- pA(nuis, a1, X)
  p.a0.X <- pA(nuis, a0, X)
  p.M.a1X <- pM_density(nuis, M, a1, X)
  p.M.a0X <- pM_density(nuis, M, a0, X)
  p.L.Ma1X <- pL_density(nuis, L, M, a1, X)
  p.L.Ma0X <- pL_density(nuis, L, M, a0, X)
  p.A0.LMX <- nuis$base$pA0LMX[cbind(L + 1, M + 1, X + 1)]
  if (a0 == 0) {
    p.a0.LMX <- p.A0.LMX
  } else {
    p.a0.LMX <- 1 - p.A0.LMX
  }

  muY.a1 <- Q_lookup(nuis, L, M, a1, X)
  mu.L.a0 <- muL_lookup(nuis, M, X)
  mu.M.a0 <- muM_lookup(nuis, X)
  mu.A.a1 <- p.a1.X * mu.M.a0

  psi <- mean(mu.A.a1 + (A == a0) * Y)

  EIF.Y <- (A == a1) * (p.M.a0X / p.M.a1X) * (p.L.Ma0X / p.L.Ma1X) * (Y - muY.a1)
  EIF.L <- p.a0.LMX * (p.a1.X / p.a0.X) * (muY.a1 - mu.L.a0)
  EIF.M <- (A == a0) * (p.a1.X / p.a0.X) * (mu.L.a0 - mu.M.a0)
  EIF.A <- ((A == a1) - p.a1.X) * mu.M.a0
  EIF.X <- mu.A.a1 + (A == a0) * Y - psi
  EIF_eff <- EIF.X + EIF.A + EIF.M + EIF.L + EIF.Y

  list(psi = psi,
       EIF_eff = EIF_eff,
       EIF.Y = EIF.Y,
       EIF.L = EIF.L,
       EIF.M = EIF.M,
       EIF.A = EIF.A,
       EIF.X = EIF.X,
       EDstar = mean(EIF.A) + mean(EIF.M) + mean(EIF.L) + mean(EIF.Y))
}

# Target nuisances in the package order: A first, then Y, then L and M.
# The L step differs from the package by using the efficient conditional-mean weight.
target_eff_nuisance <- function(data, nuis, n.iter = 500, cvg.criteria = NULL) {
  X <- as.integer(data$X)
  A <- as.integer(data$A)
  M <- as.integer(data$M)
  L <- as.integer(data$L)
  Y <- data$Y

  a0 <- nuis$a0
  a1 <- nuis$a1
  if (is.null(cvg.criteria)) {
    cvg.criteria <- nrow(data)^(-1/2)
  }
  EDstar <- 10
  EDstar.record <- c()
  iter <- 0

  while (abs(EDstar) > cvg.criteria && iter < n.iter) {
    p.a1.X <- pA(nuis, a1, X)
    p.a0.X <- pA(nuis, a0, X)
    mu.M.a0 <- muM_lookup(nuis, X)

    # A targeting step, matching the first fluctuation in flexCausal::NPS.TMLE.a.
    if (any(p.a1.X <= 0 | p.a1.X >= 1)) {
      stop("Non-finite A targeting offset: saturated P(A = a1 | X) is 0 or 1")
    }
    eps.A <- fit_logistic_fluctuation(A == a1, qlogis(p.a1.X), mu.M.a0)
    for (x in 0:1) {
      old <- qlogis(nuis$p.a1.X[x + 1])
      nuis$p.a1.X[x + 1] <- plogis(old + eps.A * nuis$mu.M[x + 1])
    }

    p.a1.X <- pA(nuis, a1, X)
    p.a0.X <- pA(nuis, a0, X)

    # Y targeting step with the outcome EIF clever covariate.
    weight.Y <- (A == a1) *
      pM_density(nuis, M, a0, X) / pM_density(nuis, M, a1, X) *
      pL_density(nuis, L, M, a0, X) / pL_density(nuis, L, M, a1, X)
    offset.Y <- Q_lookup(nuis, L, M, a1, X)
    eps.Y <- fit_linear_fluctuation(Y, offset.Y, weight.Y)
    nuis$base$Q[, , a1 + 1, ] <- nuis$base$Q[, , a1 + 1, ] + eps.Y

    # L sequential-regression targeting; this is where the efficient EIF changes the weight.
    next.mu.L <- Q_lookup(nuis, L, M, a1, X)
    offset.L <- muL_lookup(nuis, M, X)
    p.A0.LMX <- nuis$base$pA0LMX[cbind(L + 1, M + 1, X + 1)]
    if (a0 == 0) {
      p.a0.LMX <- p.A0.LMX
    } else {
      p.a0.LMX <- 1 - p.A0.LMX
    }
    weight.L <- p.a0.LMX * (p.a1.X / p.a0.X)
    eps.L <- fit_linear_fluctuation(next.mu.L, offset.L, weight.L)
    nuis$mu.L <- nuis$mu.L + eps.L

    # M sequential-regression targeting with the usual A=a0 weighted fluctuation.
    next.mu.M <- muL_lookup(nuis, M, X)
    offset.M <- muM_lookup(nuis, X)
    weight.M <- (A == a0) * (p.a1.X / p.a0.X)
    eps.M <- fit_linear_fluctuation(next.mu.M, offset.M, weight.M)
    nuis$mu.M <- nuis$mu.M + eps.M

    comp <- compute_eff_components(data, nuis)
    EDstar <- comp$EDstar
    EDstar.record <- c(EDstar.record, EDstar)
    iter <- iter + 1
  }

  nuis$EDstar <- EDstar
  nuis$EDstar.record <- EDstar.record
  nuis$iter <- iter
  nuis
}

# Compute Wald confidence limits from an estimate and its estimated EIF.
summarize_psi <- function(psi, EIF, n, level = 0.95) {
  z <- qnorm(1 - (1 - level) / 2)
  se <- sqrt(mean(EIF^2) / n)
  list(estimated_psi = psi,
       lower.ci = psi - z * se,
       upper.ci = psi + z * se,
       EIF = EIF)
}

# Estimate a single treatment-specific mean using efficient one-step and TMLE.
estimate_eff_single <- function(data, base.nuisance, a) {
  n <- nrow(data)
  initial.nuis <- init_a_nuisance(base.nuisance, a)
  initial <- compute_eff_components(data, initial.nuis)

  psi.onestep <- initial$psi + mean(initial$EIF_eff)
  onestep <- summarize_psi(psi.onestep, initial$EIF_eff, n)
  onestep$EIF.Y <- initial$EIF.Y
  onestep$EIF.A <- initial$EIF.A
  onestep$EIF.v <- initial$EIF.M + initial$EIF.L

  targeted.nuis <- target_eff_nuisance(data, initial.nuis)
  targeted <- compute_eff_components(data, targeted.nuis)
  psi.tmle <- targeted$psi
  tmle <- summarize_psi(psi.tmle, targeted$EIF_eff, n)
  tmle$EIF.Y <- targeted$EIF.Y
  tmle$EIF.A <- targeted$EIF.A
  tmle$EIF.v <- targeted$EIF.M + targeted$EIF.L
  tmle$EDstar <- targeted.nuis$EDstar
  tmle$iter <- targeted.nuis$iter
  tmle$EDstar.record <- targeted.nuis$EDstar.record

  list(Onestep = onestep, TMLE = tmle)
}

# Estimate the ATE by running the treatment-specific estimator at a[1] and a[2].
estimate_eff <- function(data, a = c(1, 0)) {
  data <- data[, c("X", "A", "M", "L", "Y")]
  data$X <- as.integer(data$X)
  data$A <- as.integer(data$A)
  data$M <- as.integer(data$M)
  data$L <- as.integer(data$L)

  base.nuisance <- fit_eff_nuisance(data)

  Y1 <- estimate_eff_single(data, base.nuisance, a[1])
  Y0 <- estimate_eff_single(data, base.nuisance, a[2])

  onestep.ate <- Y1$Onestep$estimated_psi - Y0$Onestep$estimated_psi
  onestep.eif <- Y1$Onestep$EIF - Y0$Onestep$EIF
  onestep <- summarize_psi(onestep.ate, onestep.eif, nrow(data))
  names(onestep)[names(onestep) == "estimated_psi"] <- "ACE"

  tmle.ate <- Y1$TMLE$estimated_psi - Y0$TMLE$estimated_psi
  tmle.eif <- Y1$TMLE$EIF - Y0$TMLE$EIF
  tmle <- summarize_psi(tmle.ate, tmle.eif, nrow(data))
  names(tmle)[names(tmle) == "estimated_psi"] <- "ACE"

  list(TMLE = tmle,
       Onestep = onestep,
       TMLE.Y1 = Y1$TMLE,
       TMLE.Y0 = Y0$TMLE,
       Onestep.Y1 = Y1$Onestep,
       Onestep.Y0 = Y0$Onestep)
}
