################################################################################
# AppStat Exercises from week 4
################################################################################
# Exercises from the script
#------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#*******************************************************************************
#* Problem 4.3.1 (CUSUM of Individual Measurements)
#* The diameter of diﬀerent holes with the same nominal size is regularly
#* measured automatically. You can ﬁnd the data in diameter.dat 
#* (cf. Prob. 2.3.1). Examine if the process is under control with a CUSUM chart. 
#* The target value is µ 0 = 10.0. 
#*******************************************************************************
path <- file.path("04_Datasets", "diameter.dat")
df <- read.table(path, header = TRUE)
head(df)
summary(df)
str(df)

# target value
mu0 <- 10.0

# Process standard deviation using the standard deviation of the data
sigma.hat <- sd(df$diam); sigma.hat

# Reference value and the decision interval form the process standard deviation
# Reference value:
K <- 0.5 * sigma.hat; K

# Decision interval:
H <- 5 * sigma.hat; H

# Calculation of the recursions C^+ and C^- for the CUSUMs
# C^+ and C^- CUSUMs
C.plus <- NULL
C.plus[1] <- 0

C.minus <- NULL
C.minus[1] <- 0

n <- length(df$diam); n

for(i in 1:n) {
  C.plus[i+1] <- max(0, df$diam[i] - (mu0+K) + C.plus[i])
  C.minus[i+1] <- max(0, (mu0-K) - df$diam[i] + C.minus[i])
}
C.plus
C.minus

# Plot the CUSUM chart
plot(0:n, C.plus, col = "black", pch = 20, ylim = c(-1, 1),
     xlab = "Index", ylab = "Cumulative Sums", main = "CUSUM")
lines(0:n, C.plus, col = "black")
points(0:n, -C.minus, pch = 20, col = "gray")
lines(0:n, -C.minus, col = "gray")
abline(h = c(-1, 0, 1) * H, lty = c(2, 1, 2))
text(rep(2, 3), c(-1,1) * H, label = c("Lower CUSUM", "Upper CUSUM"), pos = 3)
#   NOTE: The process is under control. But we recognise something going
#         on in the last 4 measurements (increasing trend). So we have to 
#         watch the process closely in the next few measurements


#*******************************************************************************
# Problem 4.3.2 (CUSUM of Random Samples)
#* A quality inspector in a beverage factory is responsible for the accuracy of
#* the bottling machine of a soft drink. She collected 25 random samples
#* consisting of four measurements of ﬁll level [in cm], see data set 
#* soft-drinks.dat. Examine if the process is under control with a CUSUM chart.
#* The target value is µ_0 = 16.0. You can estimate the process standard 
#* deviation with σ.bar = s.bar/c4 , where s.bar is the mean of the standard 
#* deviations of the individual samples, and the constant c4 depends only on the
#* sample size n and can be found in Tab. T.15.
#*******************************************************************************
# removing all variables in the environment
rm(list=ls(all=TRUE))

# loading data
path <- file.path("04_Datasets", "soft-drinks.dat")
df <- read.table(path, header = TRUE)
str(df)

# means, standard deviations and Ranges
df$mean <- apply(df[, 1:4], 1, mean); df$mean
df$s <- apply(df[, 1:4], 1, sd); df$s
df$R <- apply(df[, 1:4], 1, function(x){ max(x) - min(x)}); df$R

# target value
mu0 <- 16.0

# mean of standard deviations
s.bar <- mean(df$s); s.bar

# process standard deviation
## wrong: sigma.hat <- sd(df$s); sigma.hat
c4 <- 0.9213
sigma.hat <- s.bar / c4; sigma.hat

# reference value
K <- 0.5 * sigma.hat; K

# decision interval
H <- 5 * sigma.hat; H

# C^+ and C^- CUSUMs
C.plus <- NULL
C.plus[1] <- 0
C.minus <- NULL
C.minus[1] <- 0

n <- nrow(df)
for(i in 1:n) {
  C.plus[i+1] <- max(0, df$mean[i] - (mu0+K) + C.plus[i])
  C.minus[i+1] <- max(0, (mu0-K) - df$mean[i] + C.minus[i])
}
C.plus
C.minus

# CUSUM Chart
plot(0:n, C.plus, col = "black", pch = 20, ylim = c(-1.5, 1.5),
     xlab = "Index", ylab = "Cumulative Sums", main = "CUSUM of Random Samples")
lines(0:n, C.plus, col = "black")
points(0:n, -C.minus, col = "gray")
lines(0:n, -C.minus, col = "gray")
abline(h = c(-1, 0, 1) * H, lty = c(2, 1, 2))
text(rep(2, 3), c(-1, 1) * H, label = c("Lower CUSUM", "Upper CUSUM"), pos = 3)
# identify(C.plus) 
#     NOTE: Process out of controll from measearment i = 12 on


#*******************************************************************************
# Problem 4.3.3 (Monitor Simulated Process with CUSUM)
#* Simulate k = 50 random samples of size n = 8 each from a normal distribution 
#* with target value µ_0 = 10.0 and standard deviation σ = 2.0. Use the command 
#* rnorm. Create a data set consisting of k rows and n columns with data.frame.
#*******************************************************************************
#* a. Create a CUSUM chart of the simulated data. You can estimate the process 
#*    standard deviation from the simulated data.
#* b. Now add a synthetic process drift of ∆µ = 1.4 from sample i = 21 to the 
#*    data and look at the resulting CUSUM chart. 
#*    Use data.mod <- rbind(data[1:20,1:n], data[21:50,1:n]+1.4) to modify the
#*    simulated data.
#* c. From time i = 20 plot the expected line on which the process drifts away.
#* d. Play with the parameters of your simulation, that is, vary ∆µ .
#* -----------------------------------------------------------------------------
#* removing all variables in the environment
rm(list=ls(all=TRUE))

# Number of random samples
k <- 50

# Sample size
n <- 8

# Target value
mu_0 <- 10

# Standard deviation
sigma <- 2

# Creating dataset
set.seed(1)
data.simulated <- rnorm(k*n, mean = mu_0, sd = sigma)
df <- data.frame(matrix(data.simulated, nrow = 50, ncol = n))
str(df)

# means, standard deviations and Ranges
df$mean <- apply(df[, 1:n], 1, mean)
df$sd <- apply(df[, 1:n], 1, sd)
df$R <- apply(df[, 1:n], 1, function(x){ max(x) - min(x)})
str(df)
#* -----------------------------------------------------------------------------
#* a. Create a CUSUM chart of the simulated data. You can estimate the process 
#*    standard deviation from the simulated data.
#* -----------------------------------------------------------------------------

# mean of standard deviations
s.bar <- mean(df$sd); s.bar

# process standard deviation
# wrong: c4 <- 0.9213 because n = 8 and not 4!!!!
c4 <- 0.9650     #   from table for n=8
sigma.hat <- s.bar / c4; sigma.hat

# reference value
K <- 0.5 * sigma.hat; K

# decision interval
H <- 5 * sigma.hat; H

# C^+ and C^- CUSUMs
C.plus <- NULL
C.plus[1] <- 0
C.minus <- NULL
C.minus[1] <- 0

for(i in 1:k) {
  C.plus[i+1] <- max(0, df$mean[i] - (mu_0+K) + C.plus[i])
  C.minus[i+1] <- max(0, (mu_0-K) - df$mean[i] + C.minus[i])
}
C.plus
C.minus

# CUSUM Chart
plot(0:k, C.plus, col = "black", pch = 20, ylim = c(-1, 1)*H,
     xlab = "Index", ylab = "Cumulative Sums", main = "CUSUM with simulated data")
lines(0:k, C.plus, col = "black")
points(0:k, -C.minus, pch = 20, col = "gray")
lines(0:k, -C.minus, col = "gray")
abline(h = c(-1, 0, 1) * H, lty = c(2, 1, 2))
text(rep(2, 3), c(-1, 1) * H, label = c("Lower CUSUM", "Upper CUSUM"), pos = 3)
#*------------------------------------------------------------------------------
#* b. Now add a synthetic process drift of ∆µ = 1.4 from sample i = 21 to the 
#*    data and look at the resulting CUSUM chart. 
#*    Use data.mod <- rbind(data[1:20,1:n], data[21:50,1:n]+1.4) to modify the
#*    simulated data.
#*------------------------------------------------------------------------------
#*
#* Adding synthetic process drift to he data
delta.mu <- 1.4
df.mod <- rbind(df[1:20, 1:n], df[21:50, 1:n] + delta.mu)
str(df.mod)

# means, standard deviations and Ranges
df.mod$mean <- apply(df.mod[, 1:n], 1, mean)
df.mod$sd <- apply(df.mod[, 1:n], 1, sd)
df.mod$R <- apply(df.mod[, 1:n], 1, function(x){ max(x) - min(x)})
str(df.mod)

# mean of standard deviations
s.bar.mod <- mean(df.mod$sd); s.bar.mod

# process standard deviation
# wrong: c4 <- 0.9213 because n = 8 and not 4!!!!
c4 <- 0.9650     #   from table for n=8
sigma.hat.mod <- s.bar / c4; sigma.hat.mod

# reference value
K.mod <- 0.5 * sigma.hat; K.mod

# decision interval
H.mod <- 5 * sigma.hat; H.mod

# C^+ and C^- CUSUMs
C.plus.mod <- NULL
C.plus.mod[1] <- 0
C.minus.mod <- NULL
C.minus.mod[1] <- 0

for(i in 1:k) {
  C.plus.mod[i+1] <- max(0, df.mod$mean[i] - (mu_0+K.mod) + C.plus.mod[i])
  C.minus.mod[i+1] <- max(0, (mu_0-K.mod) - df.mod$mean[i] + C.minus.mod[i])
}
C.plus.mod
C.minus.mod

# CUSUM Chart
plot(0:k, C.plus.mod, col = "black", pch = 20, ylim = c(-1, 2)*H.mod,
     xlab = "Index", ylab = "Cumulative Sums", main = "CUSUM with simulated data")
lines(0:k, C.plus.mod, col = "black")
points(0:k, -C.minus.mod, pch = 20, col = "gray")
lines(0:k, -C.minus.mod, col = "gray")
abline(h = c(-1, 0, 1) * H.mod, lty = c(2, 1, 2))
text(rep(4, 3), c(-1, 1) * H.mod, label = c("Lower CUSUM", "Upper CUSUM"), pos =
#   NOTE: The process is not under control anymore!

#*------------------------------------------------------------------------------
#* c. From time i = 20 plot the expected line on which the process drifts away.
#*------------------------------------------------------------------------------
3)
curve(func.drift(x), add=TRUE, col="red")

#* -----------------------------------------------------------------------------
#* d. Play with the parameters of your simulation, that is, vary ∆µ .
#* -----------------------------------------------------------------------------
#* Same code as before in b), just changed delta.mu and that all samples are 
#* affected!
delta.mu <- 3
df.mod <- df[1:50, 1:n] + delta.mu)
str(df.mod)

# means, standard deviations and Ranges
df.mod$mean <- apply(df.mod[, 1:n], 1, mean)
df.mod$sd <- apply(df.mod[, 1:n], 1, sd)
df.mod$R <- apply(df.mod[, 1:n], 1, function(x){ max(x) - min(x)})
str(df.mod)

# mean of standard deviations
s.bar.mod <- mean(df.mod$sd); s.bar.mod

# process standard deviation
# wrong: c4 <- 0.9213 because n = 8 and not 4!!!!
c4 <- 0.9650     #   from table for n=8
sigma.hat.mod <- s.bar / c4; sigma.hat.mod

# reference value
K.mod <- 0.5 * sigma.hat; K.mod

# decision interval
H.mod <- 5 * sigma.hat; H.mod

# C^+ and C^- CUSUMs
C.plus.mod <- NULL
C.plus.mod[1] <- 0
C.minus.mod <- NULL
C.minus.mod[1] <- 0

for(i in 1:k) {
  C.plus.mod[i+1] <- max(0, df.mod$mean[i] - (mu_0+K.mod) + C.plus.mod[i])
  C.minus.mod[i+1] <- max(0, (mu_0-K.mod) - df.mod$mean[i] + C.minus.mod[i])
}
C.plus.mod
C.minus.mod

# CUSUM Chart
plot(0:k, C.plus.mod, col = "black", pch = 20, ylim = c(-1, 2)*H.mod,
     xlab = "Index", ylab = "Cumulative Sums", main = "CUSUM with simulated data")
lines(0:k, C.plus.mod, col = "black")
points(0:k, -C.minus.mod, pch = 20, col = "gray")
lines(0:k, -C.minus.mod, col = "gray")
abline(h = c(-1, 0, 1) * H.mod, lty = c(2, 1, 2))
text(rep(4, 3), c(-1, 1) * H.mod, label = c("Lower CUSUM", "Upper CUSUM"), pos = 3)
curve(func.drift(x), add=TRUE, col="red")


#*******************************************************************************
#* Problem 4.3.4 (EWMA of Random Samples). A quality inspector in a beverage 
#* factory is responsible for the accuracy of the bottling machine of a soft 
#* drink. She collected 25 random samples consisting of four measurements of ﬁll
#* level [in cm], see data set soft-drinks.dat. Examine if the process is under
#* control with a EWMA chart. The target value is µ0 = 16.0. You can estimate 
#* the process standard deviation with σ.hat = s.bar / c4 , where s.bar is the 
#* mean of the standard deviations of the individual samples, and the constant
#* c4 depends only on the sample size n and can be found in Tab. T.15. For λ, 
#* choose a long memory.
#*******************************************************************************
#* removing all variables in the environment
rm(list=ls(all=TRUE))

path <- file.path("04_Datasets", "soft-drinks.dat")
df <- read.table(path, header = TRUE)
head(df)

n <- ncol(df); n
# mean and standard deviation
df$mean <- apply(df[, 1:n], 1, mean)
df$sd <- apply(df[, 1:n], 1, sd)
str(df)

# mean of standard deviations
s.bar <- mean(df$sd)

# process standard deviation
c4 <- 0.9213
sigma.hat <- s.bar / c4; sigma.hat

# target value
mu0 <- 16

# smoothing parameter
lambda <- 0.1

# recursion
y <- NULL
y[1] <- mu0
m <- nrow(df)

for(i in 1:m) {
  y[i+1] <- (1 - lambda) * y[i] + lambda * df$mean[i]
}
y

# Calculating the control limits
sigma.pro <- sigma.hat / sqrt(n) *
              sqrt(lambda / (2-lambda) * (1 - (1-lambda)^(2*(0:m))))

UCL <- mu0 + 3*sigma.pro; UCL
LCL <- mu0 - 3*sigma.pro; LCL

# EWMA Chart
plot(0:m, y, pch = 20, ylim = c(15.9, 16.1), 
     xlab = "Index", ylab = "EWMA", main = "EWMA")
lines(0:m, y)

# add control limits
abline(h = mu0)
lines(0:m, UCL, lty = 2, type = "S")
lines(0:m, LCL, lty = 2, type = "S")
text(rep(m, 2), c(LCL[m], UCL[m]), label = c("LCL", "UCL"), pos = 1)
#   NOTE: Process is out of control from sample i = 10 on!


#*******************************************************************************
#* Problem 4.3.5 (Monitor Simulated Process with EWMA)
#* Simulate k = 50 random samples of size n = 8 each from a normal distribution 
#* with target value µ0 = 10.0 and standard deviation σ = 2.0. Use the command 
#* rnorm. Create a data set consisting of k rows and n columns with data.frame.
#*******************************************************************************
#* a. Create an EWMA chart of the simulated data. You can estimate the 
#*    process standard deviation from the simulated data.
#* b. Now add a synthetic process drift of ∆µ = 1.4 from sample i = 21 to the 
#*    data and look at the resulting EWMA chart. 
#*    Use data.mod <- rbind(data[1:20,1:n], data[21:50,1:n]+1.4) to modify the
#*    simulated data.
#* c. Play with the parameters of your simulation, that is, vary the length 
#*    of the memory λ and ∆µ.
#* d. Compare with the CUSUM simulation in Prob. 4.3.3.
#* -----------------------------------------------------------------------------
#* removing all variables in the environment
rm(list=ls(all=TRUE))

# number of experiments
k <- 50

# number of samples
n <- 8

# simulate data and convert it to a data frame
set.seed(1)
df.sim <- data.frame(matrix(rnorm(k*n, 10, 2), nrow = k, ncol = n)); df.sim

# mean and standard deviations
df.sim$mean <- apply(df.sim[,1:n], 1, mean)
df.sim$sd <- apply(df.sim[,1:n], 1, sd)

#* -----------------------------------------------------------------------------
#* a. EWMA chart of simulated data
#* -----------------------------------------------------------------------------
#* The defined function gives different values. I could not find out
#* where the error is...
source("Functions-summary-AppStat.R")
plotting_EWMA_charts_of_random_samples(df.sim, 10, 0.9650, 0.3)
#* -----------------------------------------------------------------------------
# Code from the solution:
# mean of standard deviations
sd.bar <- mean(df.sim$sd)

# process standard deviation
c4 <- 0.9650     #   from table for n=8
sigma.hat <- sd.bar / c4; sigma.hat

# target value
mu0 <- 10

# smoothing parameter
lambda <- 0.3

#   recursion
y <- NULL
y[1] <- mu0
for(i in 1:k) {
  y[i+1] <- (1-lambda) * y[i] + lambda * df.sim$mean[i]
}
y

#   control limits
sigma.pro <- sigma.hat/sqrt(n) * sqrt(lambda/(2-lambda) * (1-(1-lambda)^(2*(0:k))))
UCL <- mu0 + 3*sigma.pro;  UCL
LCL <- mu0 - 3*sigma.pro;  LCL

#   EWMA chart
plot(0:k, y, pch=20, ylim=c(-1,1)*10+mu0, xlab="Index", ylab="EWMA", main="EWMA with mean values (blue) and data (black)", col=2)
lines(0:k, y, col=2)
#   add mean values
points(1:k, df.sim$mean, col=4, pch=20)
#   add control limits
abline(h=mu0)
lines(0:k, UCL, lty=2, type="S")
lines(0:k, LCL, lty=2, type="S")
text(rep(k,2), c(LCL[21],UCL[21]), label=c("LCL", "UCL"), pos=1)
#   add data
for(i in 1:n){ points(1:k, df.sim[,i]) }

#   REMARK: The process is under control.

#* -----------------------------------------------------------------------------
#* b. Added synthetic process drift and look at the resulting EWMA chart. 
#* -----------------------------------------------------------------------------
# delata of mu0
delta.mu <- 1.4

#*    Use data.mod <- rbind(data[1:20,1:n], data[21:50,1:n]+1.4) to modify the
#*    simulated data.
df.mod <- rbind(df.sim[1:20, 1:n], df.sim[21:50, 1:n] + delta.mu); df.mod

# means and standard deviations
df.mod$mean <- apply(df.mod[, 1:n], 1, mean)
df.mod$sd <- apply(df.mod[, 1:n], 1, sd)
str(df.mod)

# mean of standard deviations
s.bar.mod <- mean(df.mod$sd)

# process standard deviation
c4 <- 0.9650     #   from table for n=8
sigma.hat.mod <- sd.bar / c4; sigma.hat

# target value
mu0 <- 10

# smoothing parameter
lambda <- 0.3

#   recursion
y.mod <- NULL
y.mod[1] <- mu0
for(i in 1:k) {
  y.mod[i+1] <- (1-lambda) * y.mod[i] + lambda * df.mod$mean[i]
}
y

#   control limits
sigma.pro.mod <- sigma.hat.mod/sqrt(n) * sqrt(lambda/(2-lambda) * (1-(1-lambda)^(2*(0:k))))
UCL.mod <- mu0 + 3*sigma.pro.mod;  UCL
LCL.mod <- mu0 - 3*sigma.pro.mod;  LCL

#   EWMA chart
plot(0:k, y.mod, pch=20, ylim=c(-1.2,1.2)*10+mu0, 
     xlab="Index", ylab="EWMA", 
     main="EWMA with mean values (blue) and data (black)", col=2)
lines(0:k, y.mod, col=2)
#   add mean values
points(1:k, df.mod$mean, col=4, pch=20)
#   add control limits
abline(h=mu0)
lines(0:k, UCL.mod, lty=2, type="S")
lines(0:k, LCL.mod, lty=2, type="S")
text(rep(k,2), c(LCL[21],UCL[21]), label=c("LCL", "UCL"), pos=1)
#   add data
for(i in 1:n){ points(1:k, df.mod[,i]) }

#   REMARK: The process is no longer under control.
