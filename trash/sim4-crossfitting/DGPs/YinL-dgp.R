library(MASS)
## Simulation Figure4(a) ==

generate_data <- function(n,parA = c(0.48, 0.07, 1.00, -1.00, -0.34, -0.12, 0.30, -0.35, 1.00, -0.10, 0.46, # linear terms
                                     0.33, 0.00, 0.45, 0.1, -0.32, -0.08, -0.2, 0.50, 0.50, -0.03)*0.1,
                          parU=c(1,1,1,0), parM = matrix(c(1, 1, 1, 0,-1,-0.5,2,0), nrow = 2,byrow = T), 
                          parL= c(1,1,1, 1, 1) ,parY = c(1, 1, 1, 1, 1, 1), 
                          sd.U=1, sd.M= matrix(c(2, 1, 1, 3), nrow = 2), sd.L=1, sd.Y=1){

  # generate X from uniform distr
  X <- replicate(10, runif(n,0,1))
  
  A <- rbinom(n, 1, plogis(parA[1] + rowSums(sweep(X, 2, parA[2:11], "*")) +  rowSums(sweep(X^2,2,parA[12:21],"*")) )) # p(A|X)
  
  # X <- runif(n, 0, 1) # p(X)
  # 
  # A <- rbinom(n, 1, plogis(parA[1] + parA[2]*X)) # p(A|X)

  U <- parU[1] + parU[2]*A + parU[3]*X[,1] + parU[4]*A*X[,1] + rnorm(n,0,sd.U) # p(U|A,X)

  M <- cbind(parM[1,1] + parM[1,2]*A + parM[1,3]*X[,1] + parM[1,4]*A*X[,1],
             parM[2,1] + parM[2,2]*A + parM[2,3]*X[,1] + parM[2,4]*A*X[,1])+ mvrnorm(n , mu =c(0,0) , Sigma = sd.M) # p(M|A,X)

  L <- parL[1] + parL[2]*A + parL[3]*M[,1] + parL[4]*M[,2] + parL[5]*X[,1] + rnorm(n,0,sd.L) # p(L|A,X)

  Y <- parY[1] + parY[2]*L  + parY[3]*M[,1] + parY[4]*M[,2] + parY[5]*X[,1] + parY[6]*U  + rnorm(n, 0, sd.Y) # p(Y|U,M,X)

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

data <- generate_data(1000)$data
attach(data, warn.conflicts = F)
# 
# SL.ranger.sparse <- function(...,min.node.size=1,num.trees = 200){
#   SL.ranger(...,min.node.size=min.node.size,num.trees=num.trees, probability = T)
# }
# 
# model <- SuperLearner(Y=A, X=data.frame(X), family = binomial(), SL.library = c('SL.ranger.sparse'))
# 
# pr <- predict(model, type="response")[[1]] %>% as.vector()
# plot(pr)
