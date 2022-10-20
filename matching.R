library(dplyr)
library(FNN)

mbc = function(X, Y, Tr, M, Model1=0, Model0=0) {   
  N1 = sum(Tr==1)
  N0 = sum(Tr==0)
  N = N1 + N0
  
  X = scale(X)
  X1 = X[Tr==1,]
  X0 = X[Tr==0,]
  Y1 = Y[Tr==1]
  Y0 = Y[Tr==0]
  Index1 = knnx.index(X1, X0, M, algo="kd_tree")
  Index0 = knnx.index(X0, X1, M, algo="kd_tree")
  K1M = tabulate(c(Index1), nbins = N1)/M
  K0M = tabulate(c(Index0), nbins = N0)/M
  Res1 = (1+K1M)*(Y-Model1)[Tr==1]
  Res0 = (1+K0M)*(Y-Model0)[Tr==0]
  
  est = mean(Model1-Model0) + mean(c(Res1,-Res0))
  se = sqrt(mean(c(((Model1-Model0-est)[Tr==1]+Res1)^2, ((Model1-Model0-est)[Tr==0]-Res0)^2))/N)
  
  Y1hat = rowMeans(apply(Index1, 2, function(x){Y[Tr==1][x]}))
  Y0hat = rowMeans(apply(Index0, 2, function(x){Y[Tr==0][x]}))
  AIvar1 = mean(c((Y1hat-Y0-est)^2,
                  (Y1-Y0hat-est)^2))/N
  Indexvar1 = knn.index(X1, 1, algo="kd_tree")
  Indexvar0 = knn.index(X0, 1, algo="kd_tree")
  varhat1 = (Y1-Y1[Indexvar1])^2/2
  varhat0 = (Y0-Y0[Indexvar0])^2/2
  AIvar2 = mean(c((K1M^2+(2-1/M)*K1M)*varhat1,
                  (K0M^2+(2-1/M)*K0M)*varhat0))/N
  AIse = sqrt(AIvar1 + AIvar2)
  
  return(list(est = est, se = se, AIse = AIse))
}
