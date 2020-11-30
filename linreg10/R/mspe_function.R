#' @title Computation of Mean Square Prediction Error (MSPE)
#' @description This function computes the Mean Square Prediction Error (MSPE) in matrix form.
#' @param Response A \code{data-frame} contains the response value.
#' @param Predictors A \code{data-frame} contains the predictor values.
#' @return A \code{data frame} containing the following attributes:
#' \describe{
#'      \item{mse}{Mean Square Prediction Error (MSPE)}
#'      }
#' @author Ozan Turkes, Ayomide Afolabi, Geeta Kharel
#' @export
#' @examples
#' coeff(Response, Predictors)

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




