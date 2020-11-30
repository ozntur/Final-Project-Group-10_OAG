#' @title F-Test
#' @description Estimate the coefficient vector
#' @param Response A \code{data-frame} containing the response value in the dataset.
#' @param Predictors A \code{data-frame} containinf the different type of predictorsin the dataset.
#' @return A \code{data frame} containing the following attributes:
#' \describe{
#'      \item{p_value}{Estimated p value}
#'      }
#' @author Ayomide Afolabi, Ozan Turkes, Geeta Kharel
#' @importFrom print
#' @export
#' @examples
#' coeff(Response, Predictors)
pval <- function(Response, Predictors){
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


  SSM = colSums( (Fitted.values - mean(Response))^2 )
  SSE = colSums( (Response - Fitted.values)^2 )

  DFM = p - 1
  DFE = n - p

  MSM = SSM / DFM
  MSE = SSE / DFE


  f_star = MSM/MSE
  p_value = pf(f_star, df1 = DFM, df2 = DFE,lower.tail = FALSE)

  print(p_value)
}
