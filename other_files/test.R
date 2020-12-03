library(linreg10)

library(MASS)

Response <- Boston$medv
Predictors <- Boston[1:13]

# Confidence interval alpha = 0.05 and method = "Bootstrap"
ci(Response, Predictors)

# Coefficients Estimate
coeff(Response, Predictors)

# Mean Square Prediction Error (MSPE)
mspe(Response, Predictors)

# Plots
plots(Response, Predictors, pl_type = "res_fit")

plots(Response, Predictors, pl_type = "qq")

plots(Response, Predictors, pl_type = "hist")

plots(Response, Predictors, pl_type = "density")

plots(Response, Predictors, pl_type = "all")

# F-Test
pval(Response, Predictors)


sum(anova(lm(medv ~ .,  data = Boston))["Mean Sq"])

anova(lm(medv ~ .,  data = Boston))



