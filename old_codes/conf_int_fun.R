# Confidence intervals Function
Estimates <- function(Response, Predictors,alpha,method){
  n <-  length(Response)   
  df <-  n - (dim(Predictors)[2]+1)  #Degree of freedom i.e no of observation minus no of parameter
  
  #Method 1
  ( Response1 <-  as.vector(Response) )   #Response values  
  Predictors <- as.matrix(Predictors) # Predictor values
  intercept <- rep(1, n)
  ( Predictors1 <- cbind(intercept,Predictors))
  Betas1 <- solve(t(Predictors1)%*%Predictors1)%*%t(Predictors1)%*%Response1
  Betas1
  length(Betas1)
  
  
  #Standard Error Computation by bootstrap Method
  B <- 10000
  Betas2 <- matrix(NA,length(Betas1),B)
  for (i in 1:B){
    samp <- sample(nrow(Predictors),nrow(Predictors),replace = T)
    Response2 <- Response1[samp]
    Predictors2 <- Predictors1[samp,]
    row.names(Predictors2) <- 1:length(Response2)
    Beta <- solve(t(Predictors2)%*%Predictors2)%*%t(Predictors2)%*%Response2
    Betas2[,i] <- Beta
  }
  
  SE2 <- apply(Betas2,1,sd)
  SE2
  
  
  # Residual computation
  Predicted_Response <- Predictors1%*%as.matrix(Betas1)  # Predicted response
  Residuals <-  Response - Predicted_Response 
  
  
  # Standard Error Computation by asymptotic normal approach
  Sigma.hat <- (1/df)*t(Residuals)%*%Residuals
  Sigma.hat <- as.vector(Sigma.hat)
  Sigma.hat
  Variance_Beta <-  Sigma.hat*diag((solve(t(Predictors1)%*%Predictors1))) #Variance of Betas
  SE1 <- sqrt( Variance_Beta)
  
  
  #Confidence Interval 
  if(method == "Bootstrap"){
    SE <- SE2
  }else{
    SE <- SE1
  }
  quant <- 1 - (alpha/2)
  Lower_CI_Beta <- Betas1 - (qnorm(p=quant)*SE)
  Upper_CI_Beta <- Betas1 + (qnorm(p=quant)*SE)
  
  Summary <- data.frame("Coefficients Estimate" =  Betas1, "Standard Error"=SE, "Lower Confidence Interval"=Lower_CI_Beta,
                        "Upper Confidence Interval"=Upper_CI_Beta)
  print(Summary)
  
}