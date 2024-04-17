library(MASS)
## Simulation Figure4(a) ==

generate_data <- function(n,parA = c(1,1), parU=c(1,1,1,0), parM = c(1, 1, 1, 0), parL= c(1,1,1, 1) ,parY = c(1, 1, 1, 1, 1), sd.U=1, sd.M= 1, sd.L=1, sd.Y=1){

  X <- runif(n, 0, 1) # p(X)

  A <- rbinom(n, 1, plogis(parA[1] + parA[2]*X)) # p(A|X)

  U <- parU[1] + parU[2]*A + parU[3]*X + parU[4]*A*X + rnorm(n,0,sd.U) # p(U|A,X)

  M <- parM[1] + parM[2]*A + parM[3]*X + parM[4]*A*X + rnorm(n , 0 , sd.M) # p(M|A,X)

  L <- parL[1] + parL[2]*A + parL[3]*M + parL[4]*X + rnorm(n,0,sd.L) # p(L|A,X)

  Y <- parY[1] + parY[2]*L  + parY[3]*M + parY[4]*X + parY[5]*U  + rnorm(n, 0, sd.Y) # p(Y|U,M,X)

  data <- data.frame(X=X, U=U, A=A, M=M, L=L, Y=Y)

  # propensity score
  ps <- A*(plogis(parA[1] + parA[2]*X))+(1-A)*(1-(plogis(parA[1] + parA[2]*X)))

  return(list(data = data,
              parA=parA,
              parU=parU,
              parM=parM,
              parL=parL,
              parY=parY,
              sd.U=sd.U,
              sd.M=sd.M,
              sd.L=sd.L,
              sd.Y=sd.Y,
              ps=ps))
}


