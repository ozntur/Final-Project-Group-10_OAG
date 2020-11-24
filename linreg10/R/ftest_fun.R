# F-test: compute the statistic in matrix form and output the
# corresponding p-value. (see the formula in the project.html file)

# x explanatory variable
# y response
set.seed(321)
exvals <- data.frame(y = c(runif(6, 40,50)), 
                     x1 = c(runif(6, min = 0, max = 20)),
                     x2 = (runif(6, 0,40)), 
                     x3 = runif(6,0,50))

response = exvals$y
# covariates = exvals$x1
covariates = exvals[2:4]
response <- as.vector(response)
covariates <- as.matrix(covariates)

beta.hat <- solve(t(covariates)%*%covariates)%*%t(covariates)%*%response

fitted_vals <- covariates%*%as.matrix(beta.hat)

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
