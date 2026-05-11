# Bootstrap & Time Series Modelling

# Question 1
n = 30
x = rnorm(n, mean=2, sd=1)
summary(x)
sd(x)

booted.data = replicate(50000, mean(sample(x, n, replace=TRUE)))
summary(booted.data)
sd(booted.data)

hist(booted.data, breaks=20, prob=TRUE, main="Histogram of Bootstrapped Data", xlab="Bootstrap Sample Means")
# The shape of the distribution is Normal and is centered at the mean.

theoretical_diff = booted.data - mean(x)
hist(theoretical_diff, breaks=20, prob=TRUE, main="Theoretical Density", xlab="Deviation from Sample Mean")
curve(dnorm(x, mean=0, sd=sqrt(1/n)), add=TRUE, col=4, lwd=2, lty=2)
# The bootstrap distribution of the theoretical difference is very similar to the theoretical density.

n = 30
x = rnorm(n, mean=2, sd=1)
summary(x)
sd(x)

booted.data = replicate(50000, mean(sample(x, n, replace=TRUE)))
summary(booted.data)
sd(booted.data)

hist(booted.data, breaks=20, prob=TRUE, main="Histogram of Bootstrapped Data", xlab="Bootstrap Sample Means")
# The shape of the distribution is also Normal and is centered at the mean.

theoretical_diff = booted.data - mean(x)
hist(theoretical_diff, breaks=20, prob=TRUE, main="Theoretical Density", xlab="Deviation from Sample Mean")
curve(dnorm(x, mean=0, sd=sqrt(1/n)), add=TRUE, col=4, lwd=2, lty=2)
# The bootstrap distribution of the theoretical difference is also very similar to the theoretical density.

n = 30
x = rnorm(n, mean=2, sd=1)
summary(x)
sd(x)

booted.data = replicate(50000, mean(sample(x, n, replace=TRUE)))
summary(booted.data)
sd(booted.data)

hist(booted.data, breaks=20, prob=TRUE, main="Histogram of Bootstrapped Data", xlab="Bootstrap Sample Means")
# The shape of the distribution is also Normal and is centered at the mean.

theoretical_diff = booted.data - mean(x)
hist(theoretical_diff, breaks=20, prob=TRUE, main="Theoretical Density", xlab="Deviation from Sample Mean")
curve(dnorm(x, mean=0, sd=sqrt(1/n)), add=TRUE, col=4, lwd=2, lty=2)
# The bootstrap distribution of the theoretical difference is also very similar to the theoretical density.

# Question 2
theta.est = function(x) {
  n = length(x)
  theta_hat = sum(x[1:(n-1)] * x[2:n]) / sum((x[1:n])^2)
  return(theta_hat)
}
source("huron.R")
my.est = theta.est(x = huron - mean(huron))

x_minus = c(0, x[1:(n-1)])
e_hat = x - my.est * x_minus
my.resid = e_hat - mean(e_hat)
hist(my.resid, main="Histogram of Residuals", breaks=15)
qqnorm(my.resid)
qqline(my.resid, col=2)
# Since the QQ plot has an S shape at the ends, the residuals are not normally distributed.

one.boot = function(eps=my.resid, theta=my.est) {
  n = length(eps)
  eps.star = sample(eps, n, replace=TRUE)
  x.star = numeric(n)
  x.prev = 0
  for (t in 1:n) {
    x.star[t] = theta * x.prev + eps.star[t]
    x.prev = x.star[t]
  }
  return(theta.est(x.star))
}
one.boot()
one.boot()
output = replicate(10000, one.boot())

diff = output - my.est
hist(diff, main="Distribution of Theta Hat Star - Theta Hat", xlab="Error", prob=TRUE)
# The distribution is left skewed.
lines(density(diff), col=2)
q = quantile(diff, probs=c(0.025, 0.975))
CI = my.est - c(q[2], q[1])

# Question 3
df = read.csv("gt1880.2025.csv", skip=1, header=TRUE)
month_data = df[, 2:13]
yearly.temp = apply(month_data, 1, mean)
year = 1880:2025
plot(year, yearly.temp, type="l", main="Yearly Average Temperature", xlab="Year", ylab="Temperature Change")
# The temperature change has an increasing trend, but increases faster after 1980.

obj_func = function(params) {
  a = params[1]
  b = params[2]
  sum((yearly.temp - a - b*year)^2)
}
res = nlminb(c(-10, 0.1), obj_func)
ls.est = res$par
fitted = ls.est[1] + ls.est[2] * year
resid = yearly.temp - fitted
lines(year, fitted, col=2, lwd=2)
# The fit is good from 1900-2000, but before and after that is underestimates the data.
plot(year, resid, main="Residuals vs. Year")
# The pattern looks quadratic with an increase in 1940 and decrease in 1970.
qqnorm(resid)
qqline(resid, col=2)
# The QQ plot has a slight S shape at the tails, so it is moderatly normal.

min_obj = function(y) {
  dummy = as.integer(year > y)
  obj = function(p) {
    sum((yearly.temp - p[1] - p[2]*year - p[3]*dummy - p[4]*dummy*year)^2)
  }
  nlminb(c(-1, 1, -1, 1), obj)
}
results = sapply(c(1978, 1979, 1980), min_obj)
best_index = which.min(unlist(results["objective", ]))
best_year = c(1978, 1979, 1980)[best_index]
best_params = results[, best_index]$par
plot(year, yearly.temp, type="l", main="Yearly Average Temperature", xlab="Year", ylab="Temperature Change")
lines(year, fitted, col=2, lwd=2)
dummy_best = as.integer(year > best_year)
fitted_p = best_params[1] + best_params[2]*year + best_params[3]*dummy_best + best_params[4]*dummy_best*year
lines(year, fitted_p, col=3, lwd=2)

# The first model overestimates 1950-2000 and underestimates before 1900 and after 2000.
# The second model fits the data much better, increasing after 1980.
# The conclusion is that rising temperatures per decade increases significantly after 1980.
