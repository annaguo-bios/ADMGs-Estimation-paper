generate_data <- function(n,parA = c(1/4,1/4,1/3), parM = c(1,0.5,1), parL= c(1,1,1, 1) ,parY = c(1/7, 1/5, 1/5, 1/5, 1/5)){

  U <- runif(n,0,1)
  
  X <- rbinom(n,1,0.5) # p(X)

  A <- rbinom(n, 1, (parA[1] + parA[2]*X+parA[3]*U)) # p(A|U,X)

  M <- rbinom(n,1, plogis(parM[1] + parM[2]*A+parM[3]*X)) # p(M|A,X)

  L <- rbinom(n,1, plogis(parL[1] + parL[2]*M+parL[3]*A + parL[4]*X)) # p(L|A,X)

  Y <- rbinom(n,1,parY[1] + parY[2]*L  + parY[3]*M + parY[4]*X + parY[5]*U) # p(Y|U,M,X)

  data <- data.frame(X=X, U=U, A=A, M=M, L=L, Y=Y)
  
  return(list(data = data,
              parA=parA,
              parM=parM,
              parL=parL,
              parY=parY))
}

