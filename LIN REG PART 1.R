Estimates <- function(Response, Predictors,alpha){
   n <-  length(Response)   
   df <-  n - (dim(Predictors)[2]+1)  #Degree of freedom i.e no of observation minus no of parameter
   
  
  Response <-  as.vector(Response)    #Response values  
  Predictors <- as.matrix(Predictors) # Predictor values
  intercept <- rep(1, n)
  Predictors <- cbind(intercept,Predictors)
  Betas <- solve(t(Predictors)%*%Predictors)%*%t(Predictors)%*%Response
  Betas
  
  
  # Residual computation
  Predicted_Response <- Predictors%*%as.matrix(Betas)  # Predicted response
  Residuals <-  Response - Predicted_Response 
  
  
  # Standard Error Computation
  Sigma.hat <- (1/df)*t(Residuals)%*%Residuals
  Sigma.hat <- as.vector(Sigma.hat)
  Sigma.hat
  Variance_Beta <-  Sigma.hat*diag((solve(t(Predictors)%*%Predictors))) #Variance of Betas
  SE <- sqrt( Variance_Beta)
 

  #Confidence Interval 
 quant <- 1 - (alpha/2)
 Lower_CI_Beta <- Betas - (qnorm(p=quant)*SE)
 Upper_CI_Beta <- Betas + (qnorm(p=quant)*SE)
 
 Summary <- data.frame("Coefficients Estimate" =  Betas, "Standard Error"=SE, "Lower Confidence Interval"=Lower_CI_Beta,
                       "Upper Confidence Interval"=Upper_CI_Beta)
 print(Summary)

 }





