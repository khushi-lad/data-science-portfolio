# Assignment 4

# Question 1
my.unif = function(n, a, c=0, m, x0) {
  x = numeric(n)
  curr_x = x0
  for (i in 1:n) {
    curr_x = (a * curr_x + c) %% m
    x[i] = curr_x
  }
  return(x/m)
}
data1 = my.unif(50, 172, 17, 30307, 17218)
hist(data1, main="Histogram 1", xlab="Value")
data2 = my.unif(50, 171, 51, 32767, 2026)
hist(data2, main="Histogram 2", xlab="Value")

my.run = function(x) {
  d = diff(x)
  changes = sum(d[-1] * d[-length(d)] < 0)
  return(changes + 1)
}
my.run(data2)
# Yes, this is a random sequence beause there are not too many runs or too few runs.

n = 50
E_Rn = (2*n - 1)/3
Var_Rn = (3*n - 5)/18
Rn_x = my.run(data2)
Z_x = (abs(Rn_x - E_Rn))/(sqrt(Var_Rn))
print(Z_x)
# Since the z score is less than 1.96, the total number of runs is within its 95% confidence interval.
y = seq(0, 1, length=50)
Rn_y = my.run(y)
Z_y = (abs(Rn_y - E_Rn))/(sqrt(Var_Rn))
print(Z_y)
# Since the z score is greater than 1.96, the total number of runs is not within its 95% confidence interval.
hist(y, main="Histogram of Sequence y", xlab="Value")

# Question 2
JB = function(x) {
  n = length(x)
  x_bar = mean(x)
  s_n = sd(x)
  gamma_n = (sum((x - x_bar)^3)) / (n * (s_n^3))
  k_n = (sum((x - x_bar)^4)) / (n * (s_n^4))
  JB_n = ((n * (gamma_n^2)) / 6) + ((n * ((k_n - 3)^2)) / 24)
  return(JB_n)
}
JB(x=rnorm(100))

n = 100
eps = 0
x1 = rnorm(n, 2, 1 + 5*rbinom(n, 1, eps))
qqnorm(x1)
qqline(x1)
# Mostly linear, normally distributed.
eps = 0.01
x2 = rnorm(n, 2, 1 + 5*rbinom(n, 1, eps))
qqnorm(x2)
qqline(x2)
# Also mostly linear, but has slight s shape on both sides.
eps = 0.05
x3 = rnorm(n, 2, 1 + 5*rbinom(n, 1, eps))
qqnorm(x3)
qqline(x3)
# Mostly linear, but has more s shape on both sides.

JB.MC = function(n, K=50000) {
  sim = numeric(K)
  for (i in 1:K) {
    x = rnorm(n)
    sim[i] = JB(x)
  }
  return(sim)
}

values = JB.MC(100)
hist(values, breaks=200, prob=TRUE, main="JB vs. Chi-Square(2) Distribution", xlab="JB", ylab="Density", xlim=c(0, 15))
lines(density(values), col=1)
curve(dchisq(x, df=2), add=TRUE, col=2)
# The JB distribution looks similar to the Chi-Square(2) distribution, but the right tail is smaller.

jb_x1 = JB(x1)
jb_x2 = JB(x2)
jb_x3 = JB(x3)
jb_val = c(jb_x1, jb_x2, jb_x3)
p_theoretical = pchisq(jb_val, df=2, lower.tail=FALSE)
p_MC = sapply(jb_val, function(val) mean(values >= val))
print(p_theoretical)
print(p_MC)
# The p-values from the Monte Carlo distribution are slightly larger than the theoretical p-values.

# Question 3
n = 10000
u = runif(n)
u_sq = u^2
x_bar = mean(u_sq)
SE = sd(u_sq) / sqrt(n)
CI_lower = x_bar - (1.96 * SE)
CI_upper = x_bar + (1.96 * SE)
true_val = integrate(function(u) u^2, 0, 1)$value
print(true_val)
print(CI_lower)
print(CI_upper)
# The CI does not contain the true value so the accuracy is low.

v = (u^2 + (1 - u)^2) / 2
mean_v = mean(v)
SE_v = sd(v) / sqrt(n)
CI_lower = mean_v - (1.96 * SE_v)
CI_upper = mean_v + (1.96 * SE_v)
true_val = integrate(function(u) ((u^2 + (1 - u)^2) / 2), 0, 1)$value
print(true_val)
print(CI_lower)
print(CI_upper)
# The CI contains the true value so the accuracy is high.

w = ((u/2)^2 + (1 - u/2)^2) / 2
mean_w = mean(w)
SE_w = sd(w) / sqrt(n)
CI_lower = mean_w - (1.96 * SE_w)
CI_upper = mean_w + (1.96 * SE_w)
true_val = integrate(function(u) (((u/2)^2 + (1 - u/2)^2) / 2), 0, 1)$value
print(true_val)
print(CI_lower)
print(CI_upper)
# The CI contains the true value so the accuracy is high.
