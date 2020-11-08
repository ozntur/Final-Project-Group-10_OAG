Estimates <- function(Response, Predictors,alpha){
   n <-  length(Response)   
   df <-  n - dim(Predictors)[2]  #Degree of freedom
   
  
  Response <-  as.vector(Response)
  Predictors <- as.matrix(Predictors)
  intercept <- rep(1, n)
  Predictors <- cbind(intercept,Predictors)
  Betas <- solve(t(Predictors)%*%Predictors)%*%t(Predictors)%*%Response
  #colnames(Betas) <- "Coefficients"
  Betas
  
  
   # Degree of freedom
  
  Predicted_Response <- Predictors%*%as.matrix(Betas)  # Predicted response
  Residuals <-  Response - Predicted_Response 
  
  
  # Computed residuals
  Sigma.hat <- (1/df)*t(Residuals)%*%Residuals
  Sigma.hat <- as.vector(Sigma.hat)
  Variance_Beta <-  Sigma.hat*(solve(t(Predictors)%*%Predictors))  #Variance of Betas
 
 
  #Confidence Interval 
 quant <- 1 - (alpha/2)
 CI_Beta <- c( Betas - qnorm(p=quant)*sqrt( Variance_Beta), Betas + qnorm(p=quant)*sqrt(Variance_Beta))
 }




