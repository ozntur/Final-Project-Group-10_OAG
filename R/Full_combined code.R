combined <- function(Response, Predictors,alpha,method,pl_type){
  n <-  length(Response)
  p <-  (dim(Predictors)[2]+1)
  df <-  n - p  #Degree of freedom i.e no of observation minus no of parameter

  #Method 1
  Response1 <-  as.vector(Response)   #Response values
  Predictors <- as.matrix(Predictors) # Predictor values
  intercept <- rep(1, n)
  ( Predictors1 <- cbind(intercept,Predictors))
  Betas1 <- solve(t(Predictors1)%*%Predictors1)%*%t(Predictors1)%*%Response1


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

  # Residual computation
  Fitted.values <- Predictors1%*%as.matrix(Betas1)  # Predicted response
  Residuals <-  Response - Fitted.values


  # Standard Error Computation by asymptotic normal approach
  Sigma.hat <- (1/df)*t(Residuals)%*%Residuals
  Sigma.hat <- as.vector(Sigma.hat)
  Variance_Beta <-  Sigma.hat*diag((solve(t(Predictors1)%*%Predictors1))) #Variance of Betas
  SE1 <- sqrt( Variance_Beta)


  #Confidence Interval
  if(method == "Bootstrap"){
    SE <- SE2
  }else if(method == "Asymptotic"){
    SE <- SE1
  }else{
    print("approach error")
  }
  quant <- 1 - (alpha/2)
  Lower_CI_Beta <- Betas1 - (qnorm(p=quant)*SE)
  Upper_CI_Beta <- Betas1 + (qnorm(p=quant)*SE)



#function 2
library(cowplot)

  pl_dat = data.frame(Residuals = Residuals, Fitted.values = Fitted.values)
  library(ggplot2)

  if(pl_type == "res_fit") {
    # 1. Residuals vs fitted-values
   print(ggplot(pl_dat, aes(y = Residuals, x = Fitted.values )) +
      geom_point() + geom_smooth(pl_dat,se = FALSE)+labs(title = "Residuals VS Fitted-Values"))

  } else if(pl_type == "qq") {
    # 2. qq-plot of residuals
    print(ggplot(pl_dat, aes(sample = Residuals)) + stat_qq() + stat_qq_line()+labs(title = "QQ-Plot of residuals"))

  } else if(pl_type == "hist") {
    #3. Histogram (or density) of residuals
   print(ggplot(pl_dat, aes(x = Residuals)) + geom_histogram(color="black", fill="white",binwidth=1)+
         labs(title = "Histogram of residuals"))

  } else if(pl_type == "all"){

    a <- ggplot(pl_dat, aes(y = Residuals, x = Fitted.values )) +
      geom_point() + geom_smooth(method = lm, se = FALSE) + labs(title = "Residuals VS Fitted-Values")

    b <- ggplot(pl_dat, aes(sample = Residuals)) + stat_qq() + stat_qq_line()+labs(title = "QQ-Plot of residuals")

    c <- ggplot(pl_dat, aes(x = Residuals)) + geom_histogram(color="black", fill="white",binwidth=1)+
      labs(title = "Histogram of residuals")
   print(plot_grid(a,b,c))

  } else{
    print("error in pl_type argument")
  }

# Third function
  mse <- (1/n) * colSums( Residuals^2 )



#F-test Fourth function

  SSM <-  colSums( (Fitted.values - mean(Response))^2 )
  SSE <-  colSums( (Response - Fitted.values)^2 )

  DFM <-  p - 1
  DFE <-  n - p

  MSM <-  SSM / DFM
  MSE <-  SSE / DFE


  f_star <-  MSM/MSE
  p_value <-  pf(f_star, df1 = DFM, df2 = DFE,lower.tail = FALSE)

  Summary1 <- data.frame("Coefficients Estimate" =  Betas1, "Standard Error"=SE, "Lower Confidence Interval"=Lower_CI_Beta,
                         "Upper Confidence Interval"=Upper_CI_Beta)
  print(Summary1)

  Summary2 <- data.frame("Mean Square Predictor Error" =  mse, "Sum Square of Model"=SSM, "Sum Square of Error"=SSE,
                        "F Statistic"=f_star,"P-Value" = p_value)
  print(Summary2)
}


