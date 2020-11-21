
# x explanatory variable
# y response
# estimate beta

##### ESTIMATE BETA #####

exvals <- data.frame(y = c(runif(5000, 40,50)), 
                            x1 = c(runif(5000, min = 0, max = 20)),
                            x2 = (runif(20, 0,40)), 
                            x3 = runif(20,0,50))

response = exvals$y
covariates = exvals$x1

lm_func = function(response, covariates, alpha = 0.05) {
  
  # Make sure data formats are appropriate
  response <- as.vector(response)
  covariates <- as.matrix(covariates)
  
  # Define parameters
  n <- length(response)
  p <- dim(covariates)[2]
  df <- n - p
  
  # Estimate beta through Eq. (6.1)
  beta.hat <- solve(t(covariates)%*%covariates)%*%t(covariates)%*%response
  
  # Estimate of the residual variance (sigma2) from Eq. (6.3)
  # Compute residuals
  resid <- response - covariates%*%as.matrix(beta.hat) 
  sigma2.hat <- (1/df)*t(resid)%*%resid
  
  # Estimate of the variance of the estimated beta from Eq. (6.2)
  var.beta <- sigma2.hat*solve(t(covariates)%*%covariates)
  
  # Estimate of the confidence interval based on alpha
  quant <- 1 - alpha/2
  ci.beta <- c(beta.hat - qnorm(p = quant)*sqrt(var.beta), beta.hat + 
                 qnorm(p = quant)*sqrt(var.beta))
  
  # fitted values y_hat
  fitted_vals <- covariates%*%as.matrix(beta.hat) 
  
  # Return all estimated values
  return(list(beta = beta.hat, sigma2 = sigma2.hat, 
              variance_beta = var.beta, ci = ci.beta, 
              residuals = resid, fitted_vals = fitted_vals))
}




##### To test! ###########

# Linear regression with lm() function
fit_lm = lm(exvals$y~exvals$x1 -1)

# Linear regression with my_lm() function
fit_my_lm = lm_func(exvals$y, exvals$x1)

# Compare outputs
manual_results = c(fit_my_lm$beta, fit_my_lm$sigma2)
base_results = c(fit_lm$coefficients, 
                 (1/fit_lm$df.residual)*t(fit_lm$residuals)%*%fit_lm$residuals)
results = cbind(manual_results, base_results)
row.names(results) = c("Beta", "Sigma")
results


plot()


##### PLOTS ##### 
library(ggplot2)

# pl_type = c(res_fit, qq, hist)
plot_lm <- function(response, covariates, pl_type) { 
  
  pl_dat = data.frame(Residuals = lm_func(response, covariates, alpha = 0.05)$residuals,
                      Fitted.Values = lm_func(response, covariates, alpha = 0.05)$fitted_vals
                      )
  
  if(pl_type == "res_fit") {
  # 1. Residuals vs fitted-values
  ggplot(pl_dat, aes(y = Residuals, x = Fitted.Values ))
  p_1 + geom_point() + geom_smooth(method = lm, se = FALSE)
  
  } else if(pl_type == "qq") {
  # 2. qq-plot of residuals
  p_2 <- ggplot(pl_dat, aes(sample = Residuals))
  p_2 + stat_qq() + stat_qq_line()
  
  } else if(pl_type == "hist") {
  #3. Histogram (or density) of residuals
  ggplot(pl_dat, aes(x = Residuals)) + geom_histogram(color="black", fill="white")
    
  } else {
    print("error")
  }
}

# TEST PLOTS
plot_lm(response, covariates, "qq")

##### Mean Square Prediction Error (MSPE) computed in matrix form #####

mspe_lm <- function(response, covariates) {
  # n: number of observations in the data (number of rows)
  n = as.matrix(1:length(response))
  
  # lm_func(response, covariates, alpha = 0.05)$residuals <- y_hat
  y_hat = lm_func(response, covariates, alpha = 0.05)$residuals
  mspe <- (1/n) * y_hat**2
  return(mspe_error = mspe)
}

##### F-test ######

ftest_lm <- function(response, covariates) {
  
  response <- as.vector(response)
  covariates <- as.matrix(covariates)
  
  n <- length(response)
  p <- dim(covariates)[2]
  df <- n - p
  
  y_hat = lm_func(response, covariates, alpha = 0.05)$residuals
  
  SSM = (y_hat - mean(response))**2
  SSE = (response - y_hat)**2
  
  DFM = p - 1 # df1 , numerator
  DFE = n-p # df2, denominator
  
  MSM = SSM / DFM
  MSE = SSE / DFE
  
  f_star = MSM/MSE
  p_value = pf(f_star, df1 = DFM, df2 = DFE)
}





















