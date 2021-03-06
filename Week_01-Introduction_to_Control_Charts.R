################################################################################
# AppStat Exercises from week 1
################################################################################
# Exercises from the script
#------------------------------------------------------------------------------

#*******************************************************************************
# Problem 1.3.3
# The data set sample.dat contains 3 random sampleswith 20 entries. These are 
# each 3 temperature measurements on 20 days. Load the data setsample.dat in R.
#*******************************************************************************
# a. Examine the structure of the data with using the commandstr(data).
# b. With summary(data) you will receive a summary of the most important statistics.
# c. Graph the samples with plot. With help(plot) you will obtain help.
# d. Calculate the mean, median, variance and standard deviation for each day.  
#	    This is elegantly done with apply. Add the mean value in the above graphic.
# e. Create a histogram and a box plot of the mean values. Use hist and boxplot.
# f. Are the variables x1 and x2 significantly different? 
#	    Use t.test. Are the requirements for thet-test met? Useqqnorm.
#------------------------------------------------------------------------------

path <- file.path("04_Datasets", "sample.dat")
data <- read.table(path, header = TRUE)

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
head(data)

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
# A machine produces metal plates with average thickness?= 8.00 mm and 
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
# Probability of 2.275%

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
n <- 5
m <- 500
set.seed(1337)
df.sim <- as.data.frame(matrix(rnorm(m*n,mean=0,sd=1), m, n))
head(df.sim)
str(df.sim)

# a.
plot(df.sim[,1], type = "l", ylim = c(-4, 4), ylab = "Simulated values")
for(i in 1:n){
  points(df.sim[,i], type = "l", col = i)
}

# b.
df.sim$mean <- apply(df.sim[,1:n], 1, mean)
?hist
hist(df.sim$mean, breaks = 100)

# b. (i)
qqnorm(df.sim$mean)
qqline(df.sim$mean, col = "red")

# b. (ii)
?dnorm
hist(df.sim$mean, , breaks = 100, freq = FALSE)
curve(dnorm(x, mean = 0, sd = 1/sqrt(n)), from = -4, to = 4, add = TRUE)
# is this curve really ok?

