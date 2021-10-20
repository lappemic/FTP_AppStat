################################################################################
# AppStat Exercises from week 2
################################################################################
# Exercises from the script
#------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#*******************************************************************************
# Problem 2.5.1 (Example 15-1 from [23; D. C. Montgomery and G. C. Runger, 
#   Applied Statistics and Probability for Engineers.]
# Using R reproduce example 16-1 from [23] to create the
# appropriate control charts. With the following command you can read the data.
#*******************************************************************************
path <- file.path("04_Datasets", "vane-opening.dat")
df <- read.table(path, header = T)
df

head(df)
str(df)
summary(df)

# Examining the assumed normal distribution with a qq plot. For this we have
#   to store the data in a vector.

vec <- c(df$x1, df$x2, df$x3, df$x4, df$x5); vec

qqnorm(vec)
qqline(vec)

# histogram with estimated normal distribution
hist(vec, freq = FALSE, breaks = 11)
curve(dnorm(x, mean = mean(vec), sd = sd(vec)),
      from = 0.9*min(df), to = 1.1*max(df), add = TRUE)
# There is no evidence against the assumptino of normal distributed values

# we assume for now sigma = 2.5
sigma <- 2.5
nSqrt <- sqrt(nrow(df))

# https://cran.r-project.org/web/packages/qicharts/vignettes/controlcharts.html
# install.packages("qicharts2")
library(qicharts2)

# Control Chart for x1
CLx1 <- mean(df[,1]); CLx1
UCLx1 <- CLx1 + 3*sigma / nSqrt; UCLx1
LCLx1 <- CLx1 - 3*sigma / nSqrt; LCLx1

x1 <- df[,1]
?qic()
qic(x1, chart = 'g')

# Control Chart for x2
x2 <- df[,2]
# Control Chart for x3
x3 <- df[,3]
# Control Chart for x4
x4 <- df[,4]
# Control Chart for x5
x5 <- df[,5]

#*******************************************************************************
# Problem 2.5.2 (Sheward Control Chart)
# A quality inspector in a beverage factory is responsible for the accuracy of
# the ﬁlling machine of a soft drink. She has collected 25 samples consisting of
# four measurements of ﬁll level [in cm], cf. data set soft-drinks.dat. Read 
# the data into R.
#*******************************************************************************
# a. Examine if the data is from a normal distribution.
# b. Use the ﬁrst 20 samples as a trial run to create an ¯x chart from an R chart.
# c. Is the trial run under statistical control?
# d. Check if the next 5 measured values meet the quality requirements.
#------------------------------------------------------------------------------
path <- file.path("04_Datasets", "soft-drinks.dat")
df <- read.table(path, header = TRUE)
head(df)
tail(df)
str(df)

# Adding the mean, standard deviation and range column for each trial
m <- ncol(df); m
df$mean <- apply(df[, 1:m], 1, mean)
df$sd <- apply(df[, 1:m], 1, sd)
df$R <- apply(df[, 1:m], 1, function(x){ max(x) - min(x)})
str(df)

df20 <- df[1:20,]
str(df20)

# Calculating the means of each newly added column from trial runs 1 to 20:
# grand mean of trials
x.barbar <- mean(as.matrix(df20[,1:4])); x.barbar
# mean of ranges
R.bar <- mean(df20$R); R.bar
# mean of standard deviation of trials
sd.bar <- mean(df20$sd); sd.bar

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# a. Examine if the data is from a normal distribution.
## Inspecting normal distribution:
df.vec <- c(df$X1, df$X2, df$X3, df$X4)
qqnorm(df.vec)
qqline(df.vec)
## -> NOTE: Data is not normally distributed in the upper part: There are
##          some indications of at least four outliers

## Creating a histogram to further inspect the normal distribution:
hist(df.vec, freq = FALSE, breaks = 11, ylim = c(0, 3))
curve(
  dnorm(x, mean = x.barbar, sd = sd.bar),
  from = min(df.vec)-1, to = max(df.vec)+1,
  n = 1001, add = TRUE)
## -> NOTE: There are outliers: Data is not normally distributed

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# b. Use the ﬁrst 20 samples as a trial run to create an ¯x chart from an R chart.
# c. Is the trial run under statistical control?

D3 <- 0       # from table for n = 4
D4 <- 2.282   # from table for n = 4
d2 <- 2.059   # from table for n = 4

# Calculating the UCL the CL and the UCL:
R.CL <- c(D3, 1, D4) * R.bar; R.CL

# Plotting the R chart:
plot(df20$R, pch = 20, ylim = c(0, 1.5), ylab = "Ranges", main = "R Chart")
lines(df20$R)
abline(h = R.CL, lty = c(2, 1, 2))
text(rep(1,3), R.CL, label = c("LCS", "CL", "UCL"), pos = 3)
#   REMARK: The trial run is not under control, sample 12 should be deleted.

#   grand average from trial run, i.e. measurements 1 to 20, without 12
ind <- c(1:11,13:20)
x.barbar.red <- mean(as.matrix(df20[ind,1:4]))
#   mean of ranges
R.bar.red <- mean(df20[ind,"R"])
#   mean of standard deviations
s.bar.red <- mean(df20[ind,"sd"])
#   control limits
R.CL.red <- c(D3, 1, D4) * R.bar.red;  R.CL.red

# Plotting the R chart again:
plot(df20$R, pch = 20, ylim = c(0, 1.5), xlim = c(0, 25), ylab = "Ranges", main = "R Chart")
lines(df20$R)
points(12, df20$R[12], col = 2, pch = 4, cex = 4)
abline(h = R.CL.red, lty = c(2, 1, 2))
text(rep(1,3), R.CL.red, label = c("LCS", "CL", "UCL"), pos = 3)
#   REMARK: The trial run is now under control

# Check the remaining 5 measurements:
abline(v = 20, col = 3, lty = 4)
points(21:25, df[21:25,"R"])
#   REMARK: The process is under control

# Plotting the x.bar chart from the R chart
## For this one needs an estimate for the process standard deviation:
d2 <- 2.059       # from table for n=4
sigma.R.hat <- R.bar.red / d2; sigma.R.hat

# and an estimate for the process mean, i.e. for the centreline:
## this we have already calculated:
x.barbar.red

# These two values give the control limits of the x.bar char:
n <- 4
x.bar.CL <- x.barbar.red + c(-3, 0, 3) * sigma.R.hat / sqrt(n); x.bar.CL

# With the x.bar.CL one can draw now the x.bar chart:
plot(df20$mean, pch = 20, ylim = c(15, 18), xlim = c(0, 25),
     ylab = "Mean Values",
     main = "x.bar Chart (based on R Chart)")
lines(df20$mean)
abline(h = x.bar.CL, lty = c(2, 1, 2))
points(12, df20$mean[12], col = 2, pch = 4, cex = 4)
text(rep(1, 3), x.bar.CL, label = c("LCS", "CL", "UCL"), pos = 3)
abline(v = 20, col = 3, lty = 4)
#   REMARK: The process is under control

# Add the remaining points:
points(21:25, df$mean[21:25])
lines(20:25, df$mean[20:25], lty = 2)

#*******************************************************************************
# Problem 2.5.3 (Individual Control Chart)
# In the production of X-ray tubes, the leakage current is a critical quality 
# variable. You ﬁnd the data in the ﬁle leakage-current.dat.
#*******************************************************************************
# a. Examine with a control chart for individual measurements whether the 
#     production process is under control.
# b. Calculate an estimate for the process mean and process standard deviation.

## Individual Control Charts are used where the sample size n = 1. This is the 
##  case in i.e. in the following examples:
##    - Automatic checks, where every produced part can be controlled
##    - If the production of a part takes a lot of time, it makes little sense
##        to create groups larger than 1
##    - If the difference of a repeated meadurement cannot be attributed to the
##        process variation, but to the measuring methods

## Control charts for individual measurements use the moving average of two
##    successive observations to measure the process variability.
#------------------------------------------------------------------------------
path <- file.path("04_Datasets", "leakage-current.dat")
df <- read.table(path, header = TRUE)
head(df)
tail(df)
str(df)
summary(df)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# a. Examine with a control chart for individual measurements whether the 
#     production process is under control.

# Calculate an estimate for the process mean
x.bar <- mean(df$current); x.bar

# To calculate the moving average, one can use the R-command diff to calculate
#   the successive differences, of which we calculate the absolute values and
#   then the mean value thereof:
MR <- mean(abs(diff(df$current))); MR

# Construct the control chart by calculating an estimate for the process 
#   standard deviation
d2 <- 1.128       # from table for n = 1
sigma.hat <- MR/d2; sigma.hat

CL <- x.bar + c(-3, 0, 3) * sigma.hat; CL
#   NOTE: Negative Control limits make no sense. So set LCL to 0:
CL <- pmax(CL, c(0, 3)); CL
plot(df$current, pch = 20, ylim = c(0, 70),
     ylab = "Individual Values",
     main = "Individuals Control Charts")
lines(df$current)
abline(h = CL, lty = c(2, 1, 2))
text(rep(1, 3), CL, label = c("LCS", "CL", "UCL"), pos = 3)
# identify(df$current)  # decomment and click in the graphic to identify outliers
                        #   finish by clicking on "finish" in the top right of
                        #   of the graphic
#   NOTE: The Process is not under control, measurement 59 is too big
#         -> Remove this point to calculate the process standard deviation and
#             process mean.

# removing measurement 59 and calculating process sd and mean
ind <- 59
# process mean
x.bar.red <- mean(df$current[-ind]); x.bar.red
# moving average
MR.red <- mean(abs(diff(df$current[-ind]))); MR.red
# standard deviation
sigma.hat.red <- MR.red / d2; sigma.hat.red

# Control limits
CL.red <- x.bar.red + c(-3, 0, 3) * sigma.hat; CL.red
#   NOTE: Again a negative control limi makes no sense, set LCL to 0:
CL.red <- pmax(CL.red, c(0, 3)); CL.red

plot(df$current[-ind], pch = 20, ylim = c(0, 70),
     ylab = "Individual Values",
     main = "Individuals Control Charts") # make sure to exclude measurement 59
lines(df$current[-ind])
abline(h = CL.red, lty = c(2, 1, 2))
text(rep(1,3), CL.red, label=c("LCL", "CL", "UCL"), pos=3)
# identify(df$current[-ind])
#   NOTE: Process still not under control -> 26 is too big

# Remove measurement 26
# removing measurement 59 and calculating process sd and mean
ind <- c(26, 59)
# process mean
x.bar.red <- mean(df$current[-ind]); x.bar.red
# moving average
MR.red <- mean(abs(diff(df$current[-ind]))); MR.red
# standard deviation
sigma.hat.red <- MR.red / d2; sigma.hat.red

# Control limits
CL.red <- x.bar.red + c(-3, 0, 3) * sigma.hat; CL.red
#   NOTE: Again a negative control limi makes no sense, set LCL to 0:
CL.red <- pmax(CL.red, c(0, 3)); CL.red

plot(df$current[-ind], pch = 20, ylim = c(0, 70),
     ylab = "Individual Values",
     main = "Individuals Control Charts") # make sure to exclude measurement 59
lines(df$current[-ind])
abline(h = CL.red, lty = c(2, 1, 2))
text(rep(1,3), CL.red, label=c("LCL", "CL", "UCL"), pos=3)
#   NOTE: The process is now under control

#*******************************************************************************
# Problem 2.5.4 (p Chart)
# In order to study the production of a lathes machine, the production of the
#   machine was checked every day for bad parts during one month 
#   (k = 25 random samples). Recorded were the number of bad parts and produced
#   objects per day in the data set rejects.dat. Create a control chart 
#   for attributes using this data.
#*******************************************************************************
path <- file.path("04_Datasets", "rejects.dat")
df <- read.table(path, header = TRUE)

head(df)
tail(df)
str(df)
summary(df)

# d denotes the bad parts
# n denotes the produced parts

# Calculate proportion of bad parts
df$p <- df$d / df$n; head(df)

# we calculate the process mean proportion of bad parts
p.bar <- sum(df$d) / sum(df$n); p.bar

