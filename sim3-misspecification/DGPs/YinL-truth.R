# M <- c("M","L")
# L <- c("A","L")
# C <- c("X")

# true p(A|X)
density.a.x <- function(parA, a, x) {
  dbinom(a, 1, plogis(parA[1] + parA[2] * x))
}

# true p(M|A,X)
density.m.ax <- function(parM, m, a, x, sd.M) {
  dmvnorm(m, mean = c(parM[1, 1] + parM[1, 2] * a + parM[1, 3] * x + parM[1, 4] * a * x,
                      parM[2, 1] + parM[2, 2] * a + parM[2, 3] * x + parM[2, 4] * a * x), sigma = sd.M)
}

# true p(L|M,A,X)
density.l.max <- function(parL, l, m, a, x, sd.L) {
  dnorm(l, mean = parL[1] + parL[2] * a + parL[3] * m[1] + parL[4] * m[2] + parL[5] * x + parL[6]* a * x + parL[7] * m[1] * x + parL[8] * m[2] * x, sd.L)
}

# E(Y|M,L,A,X)
E.y.lmax <- function(parY, parU, l, m, a, x) {
  # takes m as a dataframe or matrix
  parY[1] + parY[2] * l + parY[3] * m[, 1] + parY[4] * m[, 2] + parY[5] * x + parY[6] * (parU[1] + parU[2] * a + parU[3] * x + parU[4] * a * x ) +
    parY[7]*m[,1]*x + parY[8]*m[,2]*x
}


compute_psi_and_var <- function(a, parA, parU, parM, parL, parY, sd.L, sd.M, n ) {
  M <- cbind(M.1, M.2)
  
  a0 <- a
  a1 <- 1 - a
  
  # propensity A|X
  p.a1.X <- density.a.x(parA, a = a1, X) # p(A=a1|X)
  p.a0.X <- density.a.x(parA, a = a0, X) # p(A=a0|X)
  
  # M|A,X
  p.M.a1X <- sapply(1:n, function(i) {density.m.ax(parM, c(M.1[i], M.2[i]), a1, X[i], sd.M)}) # p(M|a,X)
  p.M.a0X <- sapply(1:n, function(i) {density.m.ax(parM, c(M.1[i], M.2[i]), a0, X[i], sd.M)}) # p(M|A,X)
  
  # L|M,A,X
  p.L.Ma1X <- sapply(1:n, function(i) {density.l.max(parL, L[i], c(M.1[i], M.2[i]), a1, X[i], sd.L)}) # p(L|a,X)
  p.L.Ma0X <- sapply(1:n, function(i) {density.l.max(parL, L[i], c(M.1[i], M.2[i]), a0, X[i], sd.L)}) # p(L|A,X)
  
  # outcome regression E[Y|M,A,X]
  muY.a1 <- E.y.lmax(parY, parU, l = L, m = cbind(M.1, M.2), a = a1, x = X)
  
  # int E(Y|L,M,A,X)p(L|M,A,X) dL
  mu.L.a0 <- parY[1] + parY[2] * (parL[1] + parL[2] * a0 + parL[3] * M[, 1] + parL[4] * M[, 2] + parL[5] * X + parL[6]* a0 * X + parL[7]*M[,1]*X + parL[8]*M[,2]*X) + # a0, replace L with E(L|A=a0,X)
    parY[3] * M[, 1] + parY[4] * M[, 2] + parY[5] * X + parY[6] * (parU[1] + parU[2] * a1 + parU[3] * X + parU[4] * a1 * X) + # a1
    parY[7]*M[,1]*X + parY[8]*M[,2]*X
    
  
  # int E(Y|L,M,A,X)p(L|M,A,X)p(M|A,X) dL dM
  mu.M.a0 <- parY[1] + parY[2] * (parL[1] + parL[2] * a0 + parL[3] * {parM[1, 1] + parM[1, 2] * a0 + parM[1, 3] * X + parM[1, 4] * a0 * X} + # replace M1 with E(M1|A=a0,X)
                                    parL[4] * {parM[2, 1] + parM[2, 2] * a0 + parM[2, 3] * X + parM[2, 4] * a0 * X} + parL[5] * X + parL[6]* a0 * X + # a0, replace L with E(L|A=a0,X) # replace M2 with E(M2|A=a0,X)
                                    parL[7]*{parM[1, 1] + parM[1, 2] * a0 + parM[1, 3] * X + parM[1, 4] * a0 * X}*X + # replace M1 with E(M1|A=a0,X)
                                    parL[8]*{parM[2, 1] + parM[2, 2] * a0 + parM[2, 3] * X + parM[2, 4] * a0 * X}*X) + # replace M2 with E(M2|A=a0,X)
    parY[3] * {parM[1, 1] + parM[1, 2] * a0 + parM[1, 3] * X + parM[1, 4] * a0 * X} + # replace M1 with E(M1|A=a0,X)
    parY[4] * {parM[2, 1] + parM[2, 2] * a0 + parM[2, 3] * X + parM[2, 4] * a0 * X} + # replace M2 with E(M2|A=a0,X)
    parY[5] * X + parY[6] * (parU[1] + parU[2] * a1 + parU[3] * X + parU[4] * a1 * X) +# a1
    parY[7]*{parM[1, 1] + parM[1, 2] * a0 + parM[1, 3] * X + parM[1, 4] * a0 * X}*X + # replace M1 with E(M1|A=a0,X)
    parY[8]*{parM[2, 1] + parM[2, 2] * a0 + parM[2, 3] * X + parM[2, 4] * a0 * X}*X # replace M2 with E(M2|A=a0,X)
  
  
  
  # int E(Y|L,M,A,X)p(L|M,A,X)p(M|A,X)p(A|X) dL dM dA
  mu.A.a1 <- p.a1.X * mu.M.a0
  
  # true_psi
  true_psi <- mean(mu.A.a1 + (A == a0) * Y)
  
  # true EIF
  EIF <- mu.A.a1 + (A == a0) * Y - true_psi + # EIF for X
    {(A == a1) - p.a1.X} * mu.M.a0 + # EIF for A|X
    (A == a0) * {p.a1.X / p.a0.X} * (mu.L.a0 - mu.M.a0) + # EIF for M|A,X
    (A == a0) * {p.a1.X / p.a0.X} * (muY.a1 - mu.L.a0) + # EIF for L|M,A,X
    (A == a1) * {p.M.a0X / p.M.a1X} * {p.L.Ma0X / p.L.Ma1X} * (Y - muY.a1) # EIF for Y|M,A,X
  
  return(list(true_psi = true_psi, EIF = EIF))
}


compute_truth <- function(n){
  
  dat_output = generate_data(n)
  data = dat_output$data
  parA=dat_output$parA 
  parM=dat_output$parM
  parL=dat_output$parL
  parY=dat_output$parY
  parU=dat_output$parU
  sd.M=dat_output$sd.M
  sd.L=dat_output$sd.L
  attach(data, warn.conflicts=FALSE)
  
  # E[Y(1)]
  E.Y1_out = compute_psi_and_var(a=1, parA, parU, parM, parL, parY, sd.L, sd.M, n)
  E.Y1 = E.Y1_out$true_psi
  EIF_Y1 = E.Y1_out$EIF
  VAR.Y1 = mean(EIF_Y1^2)
  
  # E[Y(0)]
  E.Y0_out = compute_psi_and_var(a=0, parA, parU, parM, parL, parY, sd.L, sd.M, n)
  E.Y0 = E.Y0_out$true_psi
  EIF_Y0 = E.Y0_out$EIF
  VAR.Y0 = mean(EIF_Y0^2)
  
  # ATE
  ATE = E.Y1 - E.Y0
  EIF_ATE = EIF_Y1 - EIF_Y0 
  VAR.ATE = mean(EIF_ATE^2)
  
  return(list(E.Y1=E.Y1, 
              VAR.Y1=VAR.Y1,
              EIF_Y1=EIF_Y1,
              #
              E.Y0=E.Y0, 
              VAR.Y0=VAR.Y0,
              EIF_Y0=EIF_Y0,
              #
              ATE=ATE, 
              VAR.ATE=VAR.ATE))
}
