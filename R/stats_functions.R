# Assignment 3

# Question 1
IQR.outliers = function(x) {
  if (!is.numeric(x)) {
    stop("Invalid input, must be a numeric vector.")
  }
  if (any(is.na(x))) {
    stop("Invalid input, contains missing values.")
  }
  Q1 = quantile(x, 0.25)
  Q3 = quantile(x, 0.75)
  IQR = as.numeric(Q3 - Q1)
  lower = Q1 - 1.5*IQR
  upper = Q3 + 1.5*IQR
  left_outliers = x[x < lower]
  right_outliers = x[x > upper]
  boxplot(x, main="Boxplot With Outliers")
  return(c(IQR, left_outliers, right_outliers))
}
IQR.outliers(pressure$temperature)
IQR.outliers(pressure$pressure)
# IQR.outliers("abc")
# IQR.outliers(x = c(1, 2, NA, 4))

# Question 2
skewness.test = function(x) {
  x_bar = mean(x)
  s_n = sd(x)
  n = length(x)
  gamma_n = (sum((x - x_bar)^3))/(n * (s_n^3))
  z = (sqrt(n) * gamma_n)/sqrt(6)
  if (gamma_n <= 0) {
    print("left tail")
    p_val = pnorm(z)
  }
  else if (gamma_n > 0) {
    print("right tail")
    p_val = pnorm(z, lower.tail=FALSE)
  }
  return(c(gamma_n, p_val))
}
skewness.test(rnorm(20, 10, 2))
skewness.test(rnorm(20, 10, 2))
skewness.test(rnorm(50, 10, 2))
skewness.test(rnorm(50, 10, 2))
skewness.test(rnorm(200, 10, 2))
skewness.test(rnorm(200, 10, 2))
skewness.test(rexp(20, 0.5))
skewness.test(rexp(20, 0.5))
skewness.test(rexp(50, 0.5))
skewness.test(rexp(50, 0.5))
skewness.test(rexp(200, 0.5))
skewness.test(rexp(200, 0.5))
# As n increases for rnorm with mean=10 and sd=2, it goes from having a left tail to right tail.
# As n increases for rexp with rate=0.5, gamma_n and p_val decreases.

# Question 3
x = seq(-pi, pi)
y = seq(-pi, pi)
f = function(x, y) {
  (2 + sin(x)) * (cos(2 * y))
}
z = outer(x, y, f)
persp(x, y, z, theta=30, phi=30, main="Viewing Direction With theta=30 & phi=30")
persp(x, y, z, theta=-30, phi=15, main="Viewing Direction With theta=-30 & phi=15")
persp(x, y, z, theta=0, phi=60, main="Viewing Direction With theta=0 & phi=60")

# Question 4
hist(log(islands,10), breaks = "Scott", axes = FALSE, xlab = "area",
     main = "Histograms of Landmass Areas")
axis(1, at = 1:5, labels = 10^(1:5))
axis(2)
box()
# The first line calculates the base-10 log of the values in the islands vector, plots a histogram with the number of bins calculated using the Scott rule, and uses custom axes labels.
# The second line draws the x-axis, the tick marks, and labels their values.
# The third line draws the y-axis.
# The fourth line draws a box around the plot.
hist(log(islands,10), breaks = "Sturges", axes = FALSE, xlab = "area",
     main = "Histograms of Landmass Areas", sub="Base-10 Log-Scale")
axis(1, at = 1:5, labels = round(10^(1:5), 0))
axis(2)
box()

# Question 5
df = read.csv("gt1880.2025.csv", skip=1, header=TRUE)
data = df[, 1:13]
calculate_mean = function(x) {
  if (!is.numeric(x)) {
    stop("Invalid input, must be numeric.")
  }
  return(mean(x[-1], na.rm=TRUE))
}
yearly_avg = apply(data, 1, function(row) {
  calculate_mean(as.numeric(row))
})
plot(data[, 1], yearly_avg, type="l", xlab="Year", ylab="Average Temperature Change", main="Yearly Average Temperatures (1880-2025)")
decades = c(1880, seq(1900, 2020, by=20), 2025)
plot_matrix = t(data.matrix(data[data[, 1] %in% decades, 2:13]))
y_limits = range(plot_matrix, na.rm=TRUE)
plot(1:12, plot_matrix[, 1], type="b", col="black", lty=1, pch=19, ylim=y_limits, xaxt="n", xlab="Month", ylab="Temperature Change", main="Monthly Temperatures by Decades (1880-2025)", sub="NASA GISTEMP")
matlines(1:12, plot_matrix[, -1], type="b", col=2:ncol(plot_matrix), lty=2:ncol(plot_matrix), pch=19)
axis(1, at=1:12, labels=names(data)[2:13])
legend("topleft", legend=decades, col=1:ncol(plot_matrix), lty=1:ncol(plot_matrix), pch=19, cex=0.7, ncol=2)
