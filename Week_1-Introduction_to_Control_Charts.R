################################################################################
# AppStat Exercises from week 1
################################################################################
# Exercises from the script
#------------------------------------------------------------------------------
setwd("~/OneDrive/Learning/MSE/2021hs/FTP_AppStat/Additional_material_AppStat")
#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Importing the standard libraries
library(dplyr)
library(ggplot2)
#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#*******************************************************************************
# Problem 1.3.3
# The data setsample.datcontains 3 random sampleswith 20 entries. These are 
# each 3 temperature measurements on 20 days. Load the data setsample.dat in R.
#*******************************************************************************
# a. Examine the structure of the data with using the commandstr(data).
# b. Withsummary(data)you will receive a summary of the most important statistics.
# c. Graph the samples withplot. Withhelp(plot)you will obtain help.
# d. Calculate the mean, median, variance and standard deviation for each day.  
#	This iselegantly done with apply. Add the mean value in the above graphic.
# e. Create a histogram and a box plot of the mean values. Use hist and boxplot.
# f. Are the variables x1 and x2 significantly different? 
#	Uset.test. Are the requirementsfor thet-test met? Useqqnorm.
#------------------------------------------------------------------------------

file <- "04 Datasets/sample.dat"
data <- read.table(file, header = TRUE)

# a. to e.
head(data)
str(data)
summary(data)
plot(data)

n <- ncol(data)
data$mean <- apply(data[,1:n], 1, mean); data$mean
data$median <- apply(data[,1:n], 1, median); data$median
data$sd <- apply(data[,1:n], 1, sd); data$sd
data$var <- apply(data[,1:n], 1, var); data$var
plot(data[,1:4])

dev.off
hist(data[,"mean"]) # oder 
hist(data[,4])
curve(dnorm(x, mean = mean(data$mean), sd = sd(data$mean)),
      from = 0.9*min(data$mean), to = 1.1*max(data$mean), add = TRUE) # does not work
												# properly...

boxplot(data[,"mean"]) # oder
boxplot(data$mean)

# f.
?t.test
t.test(data$x1, data$x2, alternative = "two.sided")
## x1 and x2 are significantly different on the 95% confidence interval.

par(mfrow = c(2, 1))
qqnorm(data$x1, col = "blue", xlim = c(-3,3), ylim = c(6,22))
qqline(data$x1, col = "blue", lty = 2)
qqnorm(data$x2, col = "red", xlim = c(-3,3), ylim = c(6,22))
qqline(data$x2, col = "red", lty = 2)
dev.off()

#*******************************************************************************
# Problem 1.3.4 (Normal Distribution)
# A machine produces metal plates with averagethickness?= 8.00 mm and 
# standard deviations= 0.05 mm. The slightly fluctuating platethickness is 
# assumed to be normally distributed.
#*******************************************************************************
# a. What percentage of scrap is to be expected if the thickness does not 
#	exceed 8.10 mm?
# b. What percentage of scrap is to be expected if the thickness is between 7.92 mm 
#	and 8.08 mm?
# c. What deviation from 8.00 mm is still allowed with at most 5% scrap?
#------------------------------------------------------------------------------
mu <- 8.00
sd <- 0.05

# a.
## What is the probability of the thickness >= 8.10 mm 
## P(8.10 <= X):
pnorm(q = 8.10, mean = mu, sd = sd, lower.tail = FALSE)
# Probability of 22.75%

# b.
## What is the probability of the thickness 7.92 <= X <= 8.08
## P(X <= 7.92 and 8.08 <= X):
1 - pnorm(q = 8.08, mean = mu, sd = sd) + pnorm(q = 7.92, mean = mu, sd = sd)
# Probability of 10.96%

# c.
8 - qnorm(p = 0.025, mean = mu, sd = sd)
# A deviation of at most 0.098 mm is allowed to produce at most 5% scrap

#*******************************************************************************
# Problem 1.3.5 (Simulation of Random Runs)
# Using the command rnorm generate m = 500 random samples of size n = 5 from a 
# standard normal distribution. Save the samples as an (m × n)-matrix. Use matrix.
#*******************************************************************************
# a. Visualise the samples as runs and be astonished about the big variation 
#     in the courses.
# b. Now calculate the average ¯x for each sample and generate a histogram of 
#     the mean values. From theory, we know that the means are normally 
#     distributed with parameters µ = 0 and standard deviation σ = 1/√n.
#     (i) Check this with a quantile-quantile plot. Use qqnorm and qqline.
#     (ii) Check this graphically using a histogram. 
#           Use hist(..., freq=FALSE, ...) and curve(dnorm(x,mean=0,sd=1/sqrt(n)),
#           from=-4, to=4, add=TRUE) adds the theoretical distribution density to
#           the histogram.
# c. Repeat the task for n = 2, 10, 100.
#------------------------------------------------------------------------------
n <- 500
m <- 5
set.seed(1337)
df.sim <- as.data.frame(matrix(rnorm(m*n,mean=0,sd=1), m, n))
glimpse(df.sim)
str(df.sim)

# a.
plot(df.sim[,1], type = "l", ylim = c(-4, 4), ylab = "Simulated values")
for(i in 1:n){
  points(df.sim[,i], type = "l", col = i)
}

# b.
df$mean <- apply(df[,1:n], 1, mean)
?hist
hist(df$mean, breaks = 100)

# b. (i)
qqnorm(df$mean)
qqline(df$mean, col = "red")

# b. (ii)
?dnorm
hist(df$mean, , breaks = 100, feq = FALSE)
curve(dnorm(x, mean = 0, sd = 1/sqrt(n)), from = -4, to = 4, add = TRUE)
# is this curve really ok?

#*******************************************************************************
# Problem 2.5.1 (Example 15-1 from [23; D. C. Montgomery and G. C. Runger, 
#   Applied Statistics and Probability for Engineers.]
# Using R reproduce example 16-1 from [23] to create the
# appropriate control charts. With the following command you can read the data.
#*******************************************************************************
file <- "vane-opening.dat"
df <- read.table(file, header = T)
df

head(df)
str(df)
summary(df)

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
# a.
file <- "soft-drinks.dat"
df <- read.table(file, header = TRUE)
head(df)
tail(df)
str(df)

par(mfrow = c(1,1))
qqnorm(df$X1, col = "red")
qqline(df$X1, col = "red")

qqnorm(df$X2, col = "blue")
qqline(df$X2, col = "blue")

qqnorm(df$X3, col = "green")
qqline(df$X3, col = "green")

qqnorm(df$X4, col = "orange")
qqline(df$X4, col = "orange")

# The data is more or less normally distributed even if the points scatter some-
#   what around the regression line. The last data point is interpreted as an
#   outlier.

# b.
m <- ncol(df); m
df$mean <- apply(df[, 1:m], 1, mean)
df$sd <- apply(df[, 1:m], 1, sd)
df$R <- apply(df[, 1:m], 1, function(x){ max(x) - min(x)})
str(df)

df20 <- df[1:20,]
str(df20)
