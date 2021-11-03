################################################################################
# AppStat Exercises from week 4
################################################################################
# Exercises from the script
#------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#*******************************************************************************
# Problem 4.3.1 (CUSUM of Individual Measurements)
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