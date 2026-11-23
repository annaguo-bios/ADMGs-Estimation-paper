library(MASS)
## Simulation Figure4(a) ==

generate_data <- function(n,parA = c(1, 1, -0.5), parM = c(1,1,1), parL= c(1,1,0, 1) ,parY = c(1, 4, 1, 1, 1), sd.Y=1){

  U <- runif(n, 0, 1) # p(U)
  
  X <- rbinom(n,1,0.5) # p(X)

  A <- rbinom(n, 1, plogis(parA[1] + parA[2]*X + parA[3]*U)) # p(A|X)

  M <- rbinom(n, 1, plogis(parM[1] + parM[2]*A + parM[3]*X)) # p(M|A,U,X)

  L <- rbinom(n, 1, plogis(parL[1] + parL[2]*M + parL[3]*A + parL[4]*X)) # p(L|A,M,U)

  Y <- parY[1] + parY[2]*L + parY[3]*M + parY[4]*X + parY[5]*U  + rnorm(n, 0, sd.Y) # p(Y|U,M,X)

  data <- data.frame(X=X, U=U, A=A, M=M, L=L, Y=Y)

  return(list(data = data,
              parA=parA,
              parM=parM,
              parL=parL,
              parY=parY,
              sd.Y=sd.Y))
}

