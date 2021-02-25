library(ggplot2)

#Get dataset
id <- "1X_8OKcoYbng1XvYFDirxjEWr7LtpNr1m" #Google file ID
values <- dget(sprintf("https://docs.google.com/uc?id=%s&export=download", id))

#Extract values
X = values$X
x_0 = values$x0
beta = values$beta
sigma = values$sigma

#Prediction function for f-tilde.
PredF = function (x_0, X, beta,lambda) {
  return(t(x_0)%*%beta - lambda*t(x_0)%*%solve(t(X)%*%X+lambda*diag(length(x_0)))%*%beta)
  };

a = PredF(x_0, X, beta, 0)
b = PredF(x_0, X, beta, 1)

varF = function(x_0, X, lambda, sigma){
  temp = solve(t(X)%*%X + lambda*diag(length(x_0)))%*%t(X)
  return(sigma^2 * temp%*%t(temp))
  };

av = varF(x_0,X,0,sigma)
bv = varF(x_0,X,1,sigma)

MSEplot = function(x_0, X, beta, sigma, points){
  stepsize = 4/points
  tot = c(0:points)
  var = c(0:points)
  bia = c(0:points)
  sig = c(0:points)
  lambvals = c(0:points)
  for (i in 0:points+1) {
    var[i] = varF(x_0, X, i*stepsize, sigma)
    bia[i] = (i*stepsize*t(x_0)%*%solve((t(X)%*%X+i*stepsize*diag(length(x_0))))%*%beta)^2
    sig[i] = sigma^2
    lambvals[i]=i*stepsize
    tot[i] = bia[i] + var[i] + sig[i]
  }
  plot(lambvals, var, pch = 16)
  plot(lambvals, bia, pch = 16)
  plot(lambvals, sig, pch = 16)
  plot(lambvals, tot, pch = 16)
  };

MSEplot(x_0, X, beta, sigma, 100)
x0=x_0
bias = function(lambda, X, x0, beta) {
  p = ncol(X)
  value = (lambda*t(x0)%*%solve(t(X)%*%X+lambda*diag(p))%*%beta)^2
  return(value)
}
lambdas = seq(0, 2, length.out = 500)
BIAS = rep(NA, length(lambdas))
3
for (i in 1:length(lambdas)) BIAS[i] = bias(lambdas[i], X, x0, beta)
dfBias = data.frame(lambdas = lambdas, bias = BIAS)
ggplot(dfBias, aes(x = lambdas, y = bias)) + geom_line(color = "red") + xlab(expression(lambda)) +
  ylab(expression(bias^2))

variance = function(lambda, X, x0, sigma) {
  p = ncol(X)
  inv = solve(t(X) %*% X + lambda * diag(p))
  value = sigma^2*t(x0)%*%inv%*%t(inv)%*%x0
  return(value)
}
lambdas = seq(0, 2, length.out = 500)
VAR = rep(NA, length(lambdas))
for (i in 1:length(lambdas)) VAR[i] = variance(lambdas[i], X, x0, sigma)
dfVar = data.frame(lambdas = lambdas, var = VAR)
ggplot(dfVar, aes(x = lambdas, y = var)) + geom_line(color = "green4") + xlab(expression(lambda)) +
  ylab("variance")


exp_mse = 2
lambdas[which.min(exp_mse)]

dfAll = data.frame(lambda = lambdas, bias = BIAS, var = VAR, exp_mse = exp_mse)
ggplot(dfAll) + geom_line(aes(x = lambda, y = exp_mse), color = "blue") + geom_line(aes(x = lambda,
                                                                                        y = bias), color = "red") + geom_line(aes(x = lambda, y = var), color = "green4") +
  xlab(expression(lambda)) + ylab(expression(E(MSE)))


