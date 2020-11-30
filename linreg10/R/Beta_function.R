#' @title Linear Regression Coefficients Estimate
#' @description Estimate the coefficient vector
#' @param Response A \code{data-frame} containing the response value in the dataset.
#' @param Predictors A \code{data-frame} containinf the different type of predictorsin the dataset.
#' @return A \code{data frame} containing the following attributes:
#' \describe{
#'      \item{coefficient}{Estimated value of the coefficients}
#'      }
#' @author Ayomide Afolabi, Ozan Turkes, Geeta Kharel
#' @importFrom print
#' @export
#' @examples
#' coeff(Response, Predictors)

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
  coefficient <- data.frame("Coefficients.Estimate" = Betas1)
  print(coefficient)
}



