# M <- c("M","Y")
# L <- c("A","L")
# C <- c("X")

# true p(A|X)
density.a.x <- function(parA, a, x) {
  dbinom(a, 1, plogis(parA[1] + parA[2] * x))
}

# true p(M|A,X)
density.m.ax <- function(parM, m, a, x, sd.M) {
  
  dnorm(m, mean = parM[1] + parM[2] * a + parM[3] * x + parM[4] * a * x, sd.M)
}

# true p(L|M,A,X)
density.l.max <- function(parL, parU1, l, m, a, x, sd.L, sd.U1) {
  
  dnorm(l, mean = parL[1] + parL[2]*m + parL[3]*x + parL[4]*{parU1[1] + parU1[2]*a + parU1[3]*x + parU1[4]*a*x}, sqrt(sd.L^2+sd.U1^2))
  
}


# E(Y|L,M,A,X)
E.y.lmax <- function(parY,parU2,l,m,a,x){ # takes m as a dataframe or matrix
  parY[1] + parY[2]*l  + parY[3]*a + parY[4]*x + parY[5]*{parU2[1] + parU2[2]*m + parU2[3]*a + parU2[4]*x}
}


compute_psi_and_var <- function(a, parA, parU1, parU2, parM, parL, parY, sd.L, sd.M, sd.U1, n){
  
  
  a0 <- a
  a1 <- 1 - a
  
  # propensity A|X
  p.a1.X <- density.a.x(parA, a = a1, X) # p(A=a1|X)
  p.a0.X <- density.a.x(parA, a = a0, X) # p(A=a0|X)
  
  # M|A,X
  p.M.a1X <- sapply(1:n, function(i) {density.m.ax(parM, M[i], a1, X[i], sd.M)}) # p(M|a,X)
  p.M.a0X <- sapply(1:n, function(i) {density.m.ax(parM, M[i], a0, X[i], sd.M)}) # p(M|A,X)
  
  # L|M,A,X
  p.L.Ma1X <- sapply(1:n, function(i) {density.l.max(parL, parU1, L[i], M[i], a1, X[i], sd.L, sd.U1)}) # p(L|a,X)
  p.L.Ma0X <- sapply(1:n, function(i) {density.l.max(parL, parU1, L[i], M[i], a0, X[i], sd.L, sd.U1)}) # p(L|A,X)
  
  # outcome regression E[Y|M,A,X]
  muY.a1 <- E.y.lmax(parY, parU2, l = L, m = M, a = a1, x = X)
  muY.a0 <- E.y.lmax(parY, parU2, l = L, m = M, a = a0, x = X)
  
  # int E(Y|L,M,A,X)p(L|M,A,X) dL
  mu.L.a1 <- parY[1] + parY[2]*{parL[1] + parL[2]*M + parL[3]*X + parL[4]*{parU1[1] + parU1[2]*a1 + parU1[3]*X + parU1[4]*a1*X}}  + # replace L with L|M,a1,X
    parY[3]*a0 + parY[4]*X + parY[5]*{parU2[1] + parU2[2]*M + parU2[3]*a0 + parU2[4]*X}
  
  # int E(Y|L,M,A,X)p(L|M,A,X)p(M|A,X) dL dM
  mu.M.a0 <- parY[1] + parY[2]*{parL[1] + 
      parL[2]*{parM[1] + parM[2]*a0 + parM[3]*X + parM[4]*a0*X} + # replace M1 with M1|a0,X 
      parL[3]*X + parL[4]*{parU1[1] + parU1[2]*a1 + parU1[3]*X + parU1[4]*a1*X}}  + # replace L with L|M,a1,X
      parY[3]*a0 + parY[4]*X + parY[5]*{parU2[1] + 
      parU2[2]*{parM[1] + parM[2]*a0 + parM[3]*X + parM[4]*a0*X} + # replace M1 with M1|a0,X
      parU2[3]*a0 + parU2[4]*X}
  
  # int E(Y|L,M,A,X)p(L|M,A,X)p(M|A,X)p(A|X) dL dM dA
  mu.A.a1 <- p.a1.X * mu.M.a0
  
  # true_psi
  true_psi <- mean(mu.A.a1 + (A == a0) * Y)
  
  # true EIF
  EIF <- mu.A.a1 + (A == a0) * Y - true_psi + # EIF for X
    {(A == a1) - p.a1.X} * mu.M.a0 + # EIF for A|X
    (A == a0) * {p.a1.X / p.a0.X} * (mu.L.a1 - mu.M.a0) + # EIF for M|A,X # M in M
    (A == a1) * {p.M.a0X / p.M.a1X} * (muY.a0 - mu.L.a1) + # EIF for L|M,A,X # L in L
    (A == a0) * {p.a1.X / p.a0.X} * {p.L.Ma0X / p.L.Ma1X} * (Y - muY.a0) # EIF for Y|M,A,X # Y in M
  
  return(list(true_psi=true_psi, 
              EIF=EIF))
}


compute_truth <- function(n){
  
  dat_output = generate_data(n)
  data = dat_output$data
  parA=dat_output$parA 
  parM=dat_output$parM
  parL=dat_output$parL
  parY=dat_output$parY
  parU1=dat_output$parU1
  parU2=dat_output$parU2
  sd.M=dat_output$sd.M
  sd.L=dat_output$sd.L
  sd.U1=dat_output$sd.U1
  attach(data, warn.conflicts=FALSE)
  
  # E[Y(1)]
  E.Y1_out = compute_psi_and_var(a=1, parA, parU1, parU2, parM, parL, parY, sd.L, sd.M, sd.U1, n)
  E.Y1 = E.Y1_out$true_psi
  EIF_Y1 = E.Y1_out$EIF
  VAR.Y1 = mean(EIF_Y1^2)
  
  # E[Y(0)]
  E.Y0_out = compute_psi_and_var(a=0, parA, parU1, parU2, parM, parL, parY, sd.L, sd.M, sd.U1, n)
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



