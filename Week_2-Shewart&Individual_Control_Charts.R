################################################################################
# AppStat Exercises from week 3
################################################################################
# Exercises from the script
#------------------------------------------------------------------------------

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

# a.
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
  n = 1000, add = TRUE)
## -> NOTE: There are outliers: Data is not normally distributed


# b.
df20 <- df[1:20,]
str(df20)

# Calculating the means of each newly added column:
# grand mean of trials
x.barbar <- mean(as.matrix(df20[,1:4])); x.barbar
# mean of ranges
R.bar <- mean(df20$R); R.bar
# mean of standard deviation of trials
sd.bar <- mean(df20$sd); sd.bar

D3 <- 0       # from table for n = 4
D4 <- 2.282   # from table for n = 4
d2 <- 2.059   # from table for n = 4

# Calculating the UCL the CL and the UCL:
R.CL <- c(D3, 1, D4) * R.bar; R.CL

# Plotting the R chart:
plot(df20$R, pch = 20, ylim = c(0, 1.5), ylab = "Ranges", main = "R Chart")
lines(df20$R)
abline(h = R.CL, lty = c(2, 1, 2))
text(rep(1,3), R.CL)



#*******************************************************************************
# Problem 3.6.1 (Western Electric Rules)
#* Examine if the process of Ex. 2.2.1 is under control according to the 
#* Western Electric Rules. Just look at the chart for ¯x which is based on 
#* the chart for R.
#*******************************************************************************
path <- file.path("04_Datasets", "ignition-keys.dat")
df <- read.table(path, header = TRUE)
head(df)
summary(df)
str(df)

m <- length(df); m
df$mean <- apply(df[,1:m], 1, mean)
df$sd <- apply(df[,1:m], 1, sd)
df$R <- apply(df[,1:m], 1, function(x){ max(x) - min(x) })
str(df)
