# F-test: compute the statistic in matrix form and output the
# corresponding p-value. (see the formula in the project.html file)
library(roxygen2)
n<- 100
y <-  as.vector(1:n)
ybar<- (sum(y)/n)
yhat<- matrix()
for (i in 1:n){
  mean(y)
}
SSM<- matrix(NA,length(n),n)
for (i in 1:n){
  sum((yhat - ybar)^2))
}

SSE<- matrix(NA,length(n),n)
for (i in 1:n){
  sum((y - yhat)^2))
}

p<- 1
DFM<- p-1
DFE<- n-p
MSM=SSM/DFM
MSE=SSE/DFE
Fstar<- MSM/MSE
