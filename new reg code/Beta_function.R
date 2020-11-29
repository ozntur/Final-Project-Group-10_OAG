coeff <- function(Response, Predictors){
  n <-  length(Response)
  p <-  (dim(Predictors)[2]+1)
  df <-  n - p  #Degree of freedom i.e no of observation minus no of parameter
  
  #Method 1
  Response1 <-  as.vector(Response)   #Response values  
  Predictors <- as.matrix(Predictors) # Predictor values
  intercept <- rep(1, n)
  Predictors1 <- cbind(intercept,Predictors)
  Betas1 <- solve(t(Predictors1)%*%Predictors1)%*%t(Predictors1)%*%Response1
  print(data.frame("Coefficients.Estimate" = Betas1))
}

library(MASS)
View(Boston)
Response <- Boston$medv
Predictors <- Boston[1:13]

coeff(Response, Predictors)