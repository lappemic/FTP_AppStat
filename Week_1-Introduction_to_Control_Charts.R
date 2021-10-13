################################################################################
# AppStat Exercises from week 1
################################################################################
# Exercises from the script
#------------------------------------------------------------------------------
setwd("~/OneDrive/Learning/MSE/2021hs/FTP_AppStat/Additional_material_AppStat")

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
# Problem 2.5.1
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