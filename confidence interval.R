Estimates <- function(Response, Predictors,index){
  #Degree of freedom i.e no of observation minus no of parameter
  B <- 10
  Beta <- matrix(NA,length(Betas),10)
   for (i in 1:10){
  
  Data <- Boston
  Data
  Data <- as.matrix(Data)
  Data <-  Data[sample(nrow(Data),nrow(Data), replace= T),]
  Response <- Data[,14]
 Response <- as.matrix(Response)
  Response
  
  #Response values 
  Predictors <- Data[,1:13]
  Predictors <- as.matrix(Predictors)
   
  n <-  length(Response) 
  intercept <- rep(1, n)
  Predictors <- cbind(intercept,Predictors)
  Betas <- solve(t(Predictors)%*%Predictors)%*%t(Predictors)%*%Response
  Betas
  Beta[,i] <- Betas
  }
  Betass
 str(Betas)
 nrow(Data)
 
 Beta[,i] <- Betas
 
 nrow(Data)
 sample(nrow(Data),nrow(Data), replace= T)
library(MASS)
str(Boston)
(Response <- Boston$medv)
(Predictors <- Boston[1:13])
Estimates(Response, Predictors,1:100)


B <- 1000
theta_hat <- matrix(NA,B,length(Betas))
theta_hat