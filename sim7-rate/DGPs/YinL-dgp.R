library(MASS)
generate_data <- function(n,parA = c(1,1), parU=c(-2,2,2,4), parM = matrix(c(1, 1, 1, 0,-1,-0.5,2,0), nrow = 2,byrow = T), parL= c(1, 0.5, 0.5, 0.5, 1) ,parY = c(1, 0.5, 0.5, 0.5, 1, 1) , sd.M= matrix(c(1, 0.5, 0.5, 1.5), nrow = 2), sd.L=1, sd.Y=1){

  X <- runif(n, 0, 1) # p(X)
  
  U <- runif(n, -2, 2) # p(U)

  # the closed form p(A=1|X) is expit(1+X)
  A <- rbinom(n, 1, plogis(1+X)+plogis(1+X)*(1-plogis(1+X))*(U/2)) # p(A|U,X)

  M <- cbind(parM[1,1] + parM[1,2]*A + parM[1,3]*X + parM[1,4]*A*X,
             parM[2,1] + parM[2,2]*A + parM[2,3]*X + parM[2,4]*A*X)+ mvrnorm(n , mu =c(0,0) , Sigma = sd.M) # p(M|A,X)

  L <- parL[1] + parL[2]*A + parL[3]*M[,1] + parL[4]*M[,2] + parL[5]*X + rnorm(n,0,sd.L) # p(L|A,X)

  # the closed form mu=1+0.5l+0.5m_1+0.5m_2+x+(2/3)*(a-plogis(1+x))
  Y <- parY[1] + parY[2]*L  + parY[3]*M[,1] + parY[4]*M[,2] + parY[5]*X + parY[6]*U  + rnorm(n, 0, sd.Y) # p(Y|U,M,X)

  data <- data.frame(X=X, U=U, A=A, M=M, L=L, Y=Y)

  return(list(data = data,
              parA=parA,
              parU=parU,
              parM=parM,
              parL=parL,
              parY=parY,
              sd.M=sd.M,
              sd.L=sd.L,
              sd.Y=sd.Y
              ))
}

