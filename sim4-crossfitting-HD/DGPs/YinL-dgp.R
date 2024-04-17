library(MASS)
library(mvtnorm)
## Simulation Figure4(a) ==

generate_data <- function(n,parA = c(0.48, 0.07, 1.00, -1.00, -0.34, -0.12, 0.30, -0.35, 1.00, -0.10, 0.46, # linear terms
                                     0.33, 0.00, 0.45, 0.1, -0.32, -0.08, -0.2, 0.50, 0.50, -0.03)*0.1,
                          parU=c(1,1,1,0), 
                          parM = matrix(c(3.0, 1.5, -1.5, -1.5, -1.0, -2.0, -3.0, -3.0, -1.5, 2.0, 1.5, 3.0, # linear terms
                                   1.5, 2.0, 0.5, 0.5, 3.0, # A*X interactions
                                   -0.2, -0.33, 0.5, 0.3, -0.5,# X^2 high order terms
                                   1.5, -1.5, -3.0, 2.0, -2.0, 3.0, -3.0, 1.5, -1.5, -1.5, 1.5, -1.0, # linear terms
                                   -1.5, 0.3, 3.0, -0.33, 0.5, # A*X interactions
                                   0.5, 0.5, -0.2, 0.1, 0.2)*0.025, nrow=2, byrow = T), # X^2 high order terms
                          parL= c(-3.0, -2.0, -1.5, 1.5, -1.5, -1.0, 0.5, -1.0, 0.3, 3.0, 0.5, 1.5, 0.5, -1.5, # linear terms
                                  -3.0, -0.5, 0.5, 3.0, 1.5)*0.025 ,# X^2 high order terms
                          parY = c(1.0, -2.0, -3.0, -1.5, 1.0, 0.5, -2.0, 1.5, -2.0, -3.0, -3.0, -1.5, -1.0, 0.5, 3.0, # linear terms
                                   1.0, 1.5, -2.0, 3.0, -1.0), # X^2 high order terms
                          sd.U=1, sd.M= matrix(c(2, 1, 1, 3), nrow = 2), sd.L=1, sd.Y=1){

  # generate X from uniform distr
  X <- replicate(10, runif(n,0,1))
  
  A <- rbinom(n, 1, plogis(parA[1] + rowSums(sweep(X, 2, parA[2:11], "*")) +  rowSums(sweep(X^2,2,parA[12:21],"*")) )) # p(A|X)
  
  # X <- runif(n, 0, 1) # p(X)
  # 
  # A <- rbinom(n, 1, plogis(parA[1] + parA[2]*X)) # p(A|X)

  U <- parU[1] + parU[2]*A + parU[3]*X[,1] + parU[4]*A*X[,1] + rnorm(n,0,sd.U) # p(U|A,X)

  M <- cbind(parM[1,1] + parM[1,2]*A + rowSums(sweep(X, 2, parM[1,3:12], "*")) + A*rowSums(sweep(X[,1:5],2,parM[1,13:17],"*")) + rowSums(sweep(X[,6:10]^2,2,parM[1,18:22],"*")),
             parM[2,1] + parM[2,2]*A + rowSums(sweep(X, 2, parM[2,3:12], "*")) + A*rowSums(sweep(X[,1:5],2,parM[2,13:17],"*")) + rowSums(sweep(X[,6:10]^2,2,parM[2,18:22],"*")))+ mvrnorm(n , mu =c(0,0) , Sigma = sd.M) # p(M|A,X)

  L <- parL[1] + parL[2]*A + parL[3]*M[,1] + parL[4]*M[,2] + rowSums(sweep(X, 2, parL[5:14], "*"))  + rowSums(sweep(X[,6:10]^2,2,parL[15:19],"*")) + rnorm(n,0,sd.L) # p(L|M, A,X)

  Y <- parY[1] + parY[2]*L  + parY[3]*M[,1] + parY[4]*M[,2] +
    rowSums(sweep(X, 2, parY[5:14], "*"))  + rowSums(sweep(X[,6:10]^2,2,parY[15:19],"*"))  + parY[20]*U + rnorm(n, 0, sd.Y) # p(Y|U,L,M,X)

  data <- data.frame(X=X, U=U, A=A, M=M, L=L, Y=Y)

  return(list(data = data,
              parA=parA,
              parU=parU,
              parM=parM,
              parL=parL,
              parY=parY,
              sd.U=sd.U,
              sd.M=sd.M,
              sd.L=sd.L,
              sd.Y=sd.Y))
}

