mspe <- function(Response,Predictors){
  n <-  length(Response)
  p <-  (dim(Predictors)[2]+1)
  df <-  n - p  #Degree of freedom i.e no of observation minus no of parameter


  Response1 <-  as.vector(Response)    #Response values
  Predictors <- as.matrix(Predictors) # Predictor values
  intercept <- rep(1, n)
  Predictors1 <- cbind(intercept,Predictors)
  Betas1 <- solve(t(Predictors1)%*%Predictors1)%*%t(Predictors1)%*%Response1


  # Residual computation
  Fitted.values <- Predictors1%*%as.matrix(Betas1)  # Predicted response
  Residuals <-  Response - Fitted.values

  mse <- (1/n) * colSums( Residuals^2 )
  print(mse)
}




