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
set.seed(1337)
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

