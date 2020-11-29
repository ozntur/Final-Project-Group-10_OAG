plots <- function(Response,Predictors,pl_type){
  library(cowplot)
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
  pl_dat = data.frame(Residuals = Residuals, Fitted.values = Fitted.values)
  library(ggplot2)
  
  if(pl_type == "res_fit") {
    # 1. Residuals vs fitted-values
    print(ggplot(pl_dat, aes(y = Residuals, x = Fitted.values )) + 
            geom_point() + geom_smooth(method = lm, se = FALSE) + labs(title = "Residuals VS Fitted-Values"))
    
  } else if(pl_type == "qq") {
    # 2. qq-plot of residuals
    print(ggplot(pl_dat, aes(sample = Residuals)) + stat_qq() + stat_qq_line()+labs(title = "QQ-Plot of residuals"))
    
  } else if(pl_type == "hist") {
    #3. Histogram (or density) of residuals
    print(ggplot(pl_dat, aes(x = Residuals)) + geom_histogram(color="black", fill="white",binwidth=1)+
            labs(title = "Histogram of residuals"))
    
  } else if(pl_type == "all"){
    
    a <- ggplot(pl_dat, aes(y = Residuals, x = Fitted.values )) + 
      geom_point() + geom_smooth(method = lm, se = FALSE) + labs(title = "Residuals Vs Fitted-Values")
    
    b <- ggplot(pl_dat, aes(sample = Residuals)) + stat_qq() + stat_qq_line()+labs(title = "QQ-Plot of residuals")
    
    c <-  ggplot(pl_dat, aes(x = Residuals)) + geom_histogram(color="black", fill="white",binwidth=1)+
      labs(title = "Histogram of residuals")
    print(plot_grid(a,b,c))
    
  } else{
    print("error in pl_type argument")
  }
}

library(MASS)
View(Boston)
Response <- Boston$medv
Predictors <- Boston[1:13]

plots(Response,Predictors,pl_type="hist")



