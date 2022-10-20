source("matching.R")

series = function(Y, X, X_eval) {
  suppressWarnings(predict(lm(Y ~ ., data = as.data.frame(poly(X, 2, raw = T))),
           newdata = as.data.frame(poly(X_eval, 2, raw = T))))
}


BCM = function(X, Y, Tr, M){
  model = series
  Model1 = do.call(model, list(Y[Tr==1], X[Tr==1,], X))
  Model0 = do.call(model, list(Y[Tr==0], X[Tr==0,], X))
  return(mbc(X, Y, Tr, M, Model1, Model0))
}