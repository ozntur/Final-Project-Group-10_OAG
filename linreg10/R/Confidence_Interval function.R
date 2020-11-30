#' @title Confidence Interval
#' @description Estimate the confidence Interval using either bootstrap or asymptotic
#' @param Response A \code{data-frame} containing the response value in the dataset.
#' @param Predictors A \code{data-frame} containing the different type of predictors in the dataset.
#' @param alpha A \code{numeric} containing the level of significance
#' @param method A \code{character} containing the method to be adopted for the computation.
#' @return A \code{data frame} containing the following attributes:
#' \describe{
#'      \item{Coefficients Estimate}{Estimated value of the coefficients}
#'       \item{Standard Error}{Estimated value of standard error}
#'        \item{Lower Confidence Interval}{Estimated value of the lower confidence interval}
#'         \item{Upper Confidence Interval}{Estimated value of the upper confidence interval}
#'      }
#' @author Ayomide Afolabi, Ozan Turkes, Geeta Kharel
#' @importFrom print
#' @export
#' @examples
#' ci(Response, Predictors,alpha=0.05,method="Bootstrap")

ci <- function(Response, Predictors,alpha,method){
  n <-  length(Response)
  p <-  (dim(Predictors)[2]+1)
  df <-  n - p  #Degree of freedom i.e no of observation minus no of parameter

  #Method 1
  Response1 <-  as.vector(Response)   #Response values
  Predictors <- as.matrix(Predictors) # Predictor values
  intercept <- rep(1, n)
  Predictors1 <- cbind(intercept,Predictors)
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
  }else if(method == "Asymptotic"){
    SE <- SE1
  }else(
    print("approach error")
  )
  quant <- 1 - (alpha/2)
  Lower_CI_Beta <- Betas1 - (qnorm(p=quant)*SE)
  Upper_CI_Beta <- Betas1 + (qnorm(p=quant)*SE)

  Summary <- data.frame("Coefficients Estimate" =  Betas1, "Standard Error"=SE, "Lower Confidence Interval"=Lower_CI_Beta,
                        "Upper Confidence Interval"=Upper_CI_Beta)
  print(Summary)

}



