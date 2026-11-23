# M <- c("M","L")
# L <- c("A","L")
# C <- c("X")

# saturated estimate of p(A|X) from the generated data
density.a.x <- function(a, x, parA) {
  pA1 <- parA[1] + parA[2] * x + parA[3] / 2
  dbinom(a, 1, pA1)
}

# true p(M|A,X)
density.m.ax <- function(parM, m, a, x) {
  dbinom(m, 1, plogis(parM[1] + parM[2] * a + parM[3] * x))
}

# true p(L|M,A,X)
density.l.max <- function(parL, l, m, a, x) {
  dbinom(l, 1, plogis(parL[1] + parL[2] * m + parL[3] * a + parL[4] * x))
}


# bounded estimate of E(Y|L,M,A,X) from the generated data
build_outcome_mean <- function(data) {
  fitY <- glm(Y ~ L + M + A * X, family = quasibinomial('identity'), data = data)
  mu <- array(NA_real_, dim = c(2, 2, 2, 2))

  for (l in 0:1) {
    for (m in 0:1) {
      for (a in 0:1) {
        for (x in 0:1) {
          mu[l + 1, m + 1, a + 1, x + 1] <- predict(fitY,
                                                     newdata = data.frame(L = l,
                                                                          M = m,
                                                                          A = a,
                                                                          X = x),
                                                     type = "response")
        }
      }
    }
  }

  mu
}

E.y.lmax <- function(outcome.mean, l, m, a, x) {
  n <- max(length(l), length(m), length(x))
  l <- rep(l, length.out = n)
  m <- rep(m, length.out = n)
  x <- rep(x, length.out = n)

  outcome.mean[cbind(l + 1, m + 1, rep(a + 1, n), x + 1)]
}


compute_psi_and_var <- function(a, parA, parM, parL, data, outcome.mean) {
  X <- data$X
  A <- data$A
  M <- data$M
  L <- data$L
  Y <- data$Y

  a0 <- a
  a1 <- (1 - a)

  # propensity A|X
  p.a1.X <- density.a.x(a = a1, x = X, parA) # p(A=a1|X)
  p.a0.X <- density.a.x(a = a0, x = X, parA) # p(A=a0|X)

  # M|A,X
  p.M.a1X <- density.m.ax(parM, M, a1, X) # p(M|a1,X)
  p.M.a0X <- density.m.ax(parM, M, a0, X) # p(M|a0,X)

  # L|M,A,X
  p.L.Ma1X <- density.l.max(parL, L, M, a1, X) # p(L|M,a1,X)
  p.L.Ma0X <- density.l.max(parL, L, M, a0, X) # p(L|M,a0,X)
  # outcome regression E[Y|L,M,A=a1,X]
  muY.a1 <- E.y.lmax(outcome.mean, l = L, m = M, a = a1, x = X)

  # int E(Y|L,M,A=a1,X)p(L|M,A=a0,X) dL
  p.L1.Ma0X <- plogis(parL[1] + parL[2] * M + parL[3] * a0 + parL[4] * X)
  mu.L.a0 <- (1 - p.L1.Ma0X) * E.y.lmax(outcome.mean, l = 0, m = M, a = a1, x = X) +
    p.L1.Ma0X * E.y.lmax(outcome.mean, l = 1, m = M, a = a1, x = X)

  # int E(Y|L,M,A=a1,X)p(L|M,A=a0,X)p(M|A=a0,X) dL dM
  p.M1.a0X <- plogis(parM[1] + parM[2] * a0 + parM[3] * X)
  p.L1.M0a0X <- plogis(parL[1] + parL[3] * a0 + parL[4] * X)
  p.L1.M1a0X <- plogis(parL[1] + parL[2] + parL[3] * a0 + parL[4] * X)
  mu.m0 <- (1 - p.L1.M0a0X) * E.y.lmax(outcome.mean, l = 0, m = 0, a = a1, x = X) +
    p.L1.M0a0X * E.y.lmax(outcome.mean, l = 1, m = 0, a = a1, x = X)
  mu.m1 <- (1 - p.L1.M1a0X) * E.y.lmax(outcome.mean, l = 0, m = 1, a = a1, x = X) +
    p.L1.M1a0X * E.y.lmax(outcome.mean, l = 1, m = 1, a = a1, x = X)
  mu.M.a0 <- (1 - p.M1.a0X) * mu.m0 + p.M1.a0X * mu.m1

  # int E(Y|L,M,A=a1,X)p(L|M,A=a0,X)p(M|A=a0,X)p(A=a1|X) dL dM dA
  mu.A.a1 <- p.a1.X * mu.M.a0

  # true_psi
  true_psi <- mean(mu.A.a1 + (A == a0) * Y)

  # true EIF
  EIF <- mu.A.a1 + (A == a0) * Y - true_psi + # EIF for X
    {(A == a1) - p.a1.X} * mu.M.a0 + # EIF for A|X
    (A == a0) * {p.a1.X / p.a0.X} * (mu.L.a0 - mu.M.a0) + # EIF for M|A,X
    (A == a0) * {p.a1.X / p.a0.X} * (muY.a1 - mu.L.a0) + # EIF for L|M,A,X
    (A == a1) * {p.M.a0X / p.M.a1X} * {p.L.Ma0X / p.L.Ma1X} * (Y - muY.a1) # EIF for Y|L,M,A,X

  EIF_a0 <- (A == a0) * Y - mean((A == a0) * Y)
  EIF_X <- mu.A.a1 - mean(mu.A.a1) # EIF for X
  EIF_A <- {(A == a1) - p.a1.X} * mu.M.a0 # EIF for A|X
  EIF_M <- (A == a0) * {p.a1.X / p.a0.X} * (mu.L.a0 - mu.M.a0) # EIF for M|A,X
  EIF_L <- (A == a0) * {p.a1.X / p.a0.X} * (muY.a1 - mu.L.a0) # EIF for L|M,A,X
  EIF_Y <- (A == a1) * {p.M.a0X / p.M.a1X} * {p.L.Ma0X / p.L.Ma1X} * (Y - muY.a1) # EIF for Y|L,M,A,X

  return(list(true_psi = true_psi, EIF = EIF))
}


compute_truth <- function(n, parA, parM, parL,parY) {
  dat_output <- generate_data(n, parA = parA, parM = parM, parL = parL, parY=parY)
  data <- dat_output$data
  outcome.mean <- build_outcome_mean(data)

  # E[Y(1)]
  E.Y1_out <- compute_psi_and_var(a = 1, parA, parM, parL, data, outcome.mean)
  E.Y1 <- E.Y1_out$true_psi
  EIF_Y1 <- E.Y1_out$EIF
  VAR.Y1 <- mean(EIF_Y1^2)

  # E[Y(0)]
  E.Y0_out <- compute_psi_and_var(a = 0, parA, parM, parL, data, outcome.mean)
  E.Y0 <- E.Y0_out$true_psi
  EIF_Y0 <- E.Y0_out$EIF
  VAR.Y0 <- mean(EIF_Y0^2)

  # ATE
  ATE <- E.Y1 - E.Y0
  EIF_ATE <- EIF_Y1 - EIF_Y0
  VAR.ATE <- mean(EIF_ATE^2)

  return(list(E.Y1 = E.Y1,
              VAR.Y1 = VAR.Y1,
              EIF_Y1 = EIF_Y1,
              #
              E.Y0 = E.Y0,
              VAR.Y0 = VAR.Y0,
              EIF_Y0 = EIF_Y0,
              #
              ATE = ATE,
              VAR.ATE = VAR.ATE))
}
