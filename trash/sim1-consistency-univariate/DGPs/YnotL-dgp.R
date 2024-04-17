library(MASS)
## Simulation Figure4(b) ==

generate_data <- function(n,parA = c(1,1), parU1=c(1,1,1,0),parU2=c(1,1,1,1), parM = c(1, 1, 1, 0), parL= c(1,1,1, 1) ,parY = c(1, 1, 1, 1, 1), sd.U1=1, sd.U2=1, sd.M= 1, sd.L=1, sd.Y=1){

  X <- runif(n, 0, 1) # p(X)

  A <- rbinom(n, 1, plogis(parA[1] + parA[2]*X)) # p(A|X)

  U1 <- parU1[1] + parU1[2]*A + parU1[3]*X + parU1[4]*A*X + rnorm(n,0,sd.U1) # p(U1|A,X)

  M <- parM[1] + parM[2]*A + parM[3]*X + parM[4]*A*X + rnorm(n , 0 , sd.M) # p(M|A,X)

  U2 <- parU2[1] + parU2[2]*M  + parU2[3]*A + parU2[4]*X + rnorm(n,0,sd.U2) # p(U2|A,X,M)

  L <- parL[1] + parL[2]*M + parL[3]*X + parL[4]*U1 + rnorm(n,0,sd.L) # p(L|M,X,U1)

  Y <- parY[1] + parY[2]*L  + parY[3]*A + parY[4]*X + parY[5]*U2  + rnorm(n, 0, sd.Y) # p(Y|L,A,X,U2)

  data <- data.frame(X=X, U1=U1, U2=U2, A=A, M=M, L=L, Y=Y)

  # propensity score
  ps <- A*(plogis(parA[1] + parA[2]*X))+(1-A)*(1-(plogis(parA[1] + parA[2]*X)))

  return(list(data = data,
              parA=parA,
              parU1=parU1,
              parU2=parU2,
              parM=parM,
              parL=parL,
              parY=parY,
              sd.U1=sd.U1,
              sd.U2=sd.U2,
              sd.M=sd.M,
              sd.L=sd.L,
              sd.Y=sd.Y,
              ps=ps))
}

