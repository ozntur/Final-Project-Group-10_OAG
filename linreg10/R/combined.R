
# x explanatory variable
# y response
# estimate beta

exvals <- data.frame(y = c(runif(6, 40,50)),
                     x1 = c(runif(6, min = 0, max = 20)),
                     x2 = (runif(6, 0,40)),
                     x3 = runif(6,0,50))

response = exvals$y
# covariates = exvals$x1
covariates = exvals[2:4]

##### ESTIMATE BETA  - Confidence Intervals #####

# approach can be ; asymptotic or bootstrap

lm_func = function(response, covariates, alpha = 0.05, approach = "asymptotic",
                   only_plot = "FALSE", pl_type = "hist") {

  # Make sure data formats are appropriate
  response <- as.vector(response)
  covariates <- as.matrix(covariates)

  # Define parameters
  n <- length(response)
  p <- dim(covariates)[2]
  df <- n - p

  ### CONFIDENCE INTERAVAL

  if(approach == "asymptotic") {
    # Estimate beta through Eq. (6.1)
    beta.hat <- solve(t(covariates)%*%covariates)%*%t(covariates)%*%response

    # Estimate of the residual variance (sigma2) from Eq. (6.3)
    # fitted values y_hat
    fitted_vals <- covariates%*%as.matrix(beta.hat)

    # Compute residuals
    resid <- response - fitted_vals
    sigma2.hat <- (1/df)*t(resid)%*%resid

    # Estimate of the variance of the estimated beta from Eq. (6.2)
    var.beta <- as.vector(sigma2.hat)*diag(solve(t(covariates)%*%covariates))

    # Estimate of the confidence interval based on alpha
    quant <- 1 - alpha/2
    ci.beta <- data.frame(lowerCI = beta.hat - qnorm(p = quant)*sqrt(var.beta),
                          upperCI = beta.hat + qnorm(p = quant)*sqrt(var.beta))
  } else if(approach == "bootstrap") {

    print("still working on it")
  } else {
      print("apprach error")
    }

  ###### PLOT IF NECESSARY

  if(only_plot == TRUE) {
    pl_dat = data.frame(Residuals = resid, Fitted.Values = fitted_vals)

    if(pl_type == "res_fit") {
      # 1. Residuals vs fitted-values
      p <- ggplot(pl_dat, aes(y = Residuals, x = Fitted.Values )) +
        geom_point() + geom_smooth(method = lm, se = FALSE)

    } else if(pl_type == "qq") {
      # 2. qq-plot of residuals
      p <- ggplot(pl_dat, aes(sample = Residuals)) + stat_qq() + stat_qq_line()

    } else if(pl_type == "hist") {
      #3. Histogram (or density) of residuals
      p <- ggplot(pl_dat, aes(x = Residuals)) + geom_histogram(color="black", fill="white")

    } else {
      print("error")
    }
    print(p)
    return()
  }


  ########## Mean Square Prediction Error (MSPE) computed in matrix form ######

  # n: number of observations in the data (number of rows)
  n_mspe = length(response)

  mspe <- (1/n_mspe) * colSums( resid**2 )

  ##### F-test ######
  n_f <- length(response)
  p_f <- dim(covariates)[2]

  SSM = colSums( (fitted_vals - mean(response))**2 )
  SSE = colSums( (response - fitted_vals)**2 )

  DFM = p_f - 1 # df1, numerator
  DFE = n_f - p_f # df2, denominator

  MSM = SSM / DFM
  MSE = SSE / DFE

  f_star = MSM/MSE
  p_value = pf(f_star, df1 = DFM, df2 = DFE)

  # Return all estimated values
  return(list(beta = beta.hat, sigma2 = sigma2.hat,
              variance_beta = var.beta, ci = ci.beta,
              residuals = resid, fitted_vals = fitted_vals,
              mspe_error = mspe, p_value = p_value))

}

a<- lm_func(response = exvals$y, covariates = exvals[2:4], only_plot = FALSE, pl_type = "res_fit")




##### To test! ###########

# Linear regression with lm() function
fit_lm = lm(y ~ x1 + x2 + x3 -1, data = exvals)

# Linear regression with my_lm() function
fit_my_lm = lm_func(exvals$y, exvals[2:4])

# Compare outputs
manual_results = c(fit_my_lm$beta, fit_my_lm$sigma2)
base_results = c(fit_lm$coefficients,
                 (1/fit_lm$df.residual)*t(fit_lm$residuals)%*%fit_lm$residuals)
results = cbind(manual_results, base_results)
row.names(results) = c("Beta", "Sigma")
results



