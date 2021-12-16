################################################################################
# AppStat Exercises from week 6
################################################################################
# Exercises from the script
#*------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#*******************************************************************************
#* Problem 7.5.1 (Venice Sea Level, Example 5.1)
#* The annual maximum sea levels [in cm] in Venice, 1931-1981 have been recorded
#* by P. A. Pirazzoli. The data set venice.dat contains the annual maximum tides
#* at Venice for the 51 years.
#*******************************************************************************
#* a. Represent the data in a scatter diagram sea level versus year and describe
#*    the functional context in words.
#* b. Fit a straight line to the data points. Give the estimated parameter values.
#* c. Add the model in the scatter diagram. Comment on the solution.
#* d. Does the data support the hypothesis that Venice sinks?
#*------------------------------------------------------------------------------
#* Read the data
path <- file.path("04_Datasets", "venice.dat")
df <- read.table(path, header = TRUE)
head(df)
str(df)

#*------------------------------------------------------------------------------
#* a. Represent the data in a scatter diagram sea level versus year and describe
#*    the functional context in words.
#*------------------------------------------------------------------------------
plot(df$Year, df$SeaLevel, main = "Sea Level vs. Year")
grid()

#   NOTE: There seems to be a slight relationship between year and sea level.
#         The sea level raises over the year.

#*------------------------------------------------------------------------------
#* b. Fit a straight line to the data points. Give the estimated parameter values.
#*------------------------------------------------------------------------------
#   Estimation of the parameters (explicit formulae)
Year.bar <- mean(df$Year)
SeaLevel.bar <- mean(df$SeaLevel)
Sxx <- sum((df$Year-Year.bar)^2)
Sxy <- sum((df$Year-Year.bar)*(df$SeaLevel-SeaLevel.bar))
#   slope
beta1 <- Sxy/Sxx;   beta1

#   intercept
beta0 <- SeaLevel.bar - beta1*Year.bar;   beta0

#   REMARK: The intercept is the sea level in the year 0, that is 2000 years ago.
#           This shows clearly that the intercept in this
#           example has no physical meaning, since it is much too far away from
#           the point cloud.

#   fitted values
SeaLevel.hat <- beta0 + beta1*df$Year

#   residuals
res <- df$SeaLevel - SeaLevel.hat
mean(res)

#   error sum of squares
sigma2.hat <- sum(res^2)/(nrow(df)-2)

#   residual standard error
sqrt(sigma2.hat)

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters (lm)
mod <- lm(SeaLevel ~ Year, df);   mod

#   mod is a big list
str(mod)

#   coefficients
mod$coefficients

#   residuals
mod$residuals

#   fitted values
mod$fitted.values

#   deviance
dev <- deviance(mod);   dev

#   degrees of freedom
dfree <- df.residual(mod);   dfree
##  49

#   residual standard error
sigma.hat <- sqrt(dev/dfree);   sigma.hat


#*------------------------------------------------------------------------------
#* c. Add the model in the scatter diagram. Comment on the solution.
#*------------------------------------------------------------------------------
plot(SeaLevel ~ Year, df, pch=20, main="Sea Level versus Year with best model")
grid()
#   add best model
abline(mod)

#   REMARK: The best model indeed has a positive slope indicating that the data could support the hypothesis the Venice sinks.
#           On the other hand the points scatter a lot around the straight line.
#           We will have to test the hypothesis H0: slope=0

#*------------------------------------------------------------------------------
#* d. Does the data support the hypothesis that Venice sinks?
#*------------------------------------------------------------------------------   null hypothesis        H0: slope=0
#   alternative hypothesis H1: slope <> 0
beta10 <- 0

#   estimator of slope
beta1.hat <- as.numeric(mod$coefficients["Year"]);   beta1.hat

#   standard error (Sxx from Ex. 7.2.1)
se.beta1 <- sqrt(sigma.hat^2/Sxx);   se.beta1

#   test statistic
Test.beta1 <- (beta1.hat-beta10)/se.beta1;   Test.beta1

#   critical value (two-sided)
alpha <- 0.05

#   the degrees of freedom were defined in Ex. 7.2.1
t.crit <- qt(p=1-alpha/2, df=dfree);   t.crit

#   REMARK: Since abs(Test.beta1)=3.200852 > t.crit=2.009575 we reject the null 
#           hypothesis and conclude that the Sea level
#           depends significantly on the year.
#           In other words the data set supports the the hypothesis that Venice sinks.
#           The annual sinking rate is 0.5669683 cm/year.

#   total amount of sinking
diff(range(df$Year)) * beta1.hat

#   REMARK: In the years 1931-1981 Venice sank 28.34842 cm.


#--------------------------------------------------------------------------------------------------------------------------------------------------
summary(mod)



#*******************************************************************************
#* Problem 7.5.2 (Windmill, Example 5.2)
#* A research engineer is investigating the use of a windmill to generate 
#* electricity. She has collected data on DC output [in A] from her windmill and
#* the corresponding wind velocity [in m/s]. The data set windmill.dat contains 
#* the wind velocities and the DC outputs.
#*******************************************************************************
#* a. Plot a scatter diagram of DC output versus wind velocity and a scatter 
#*    diagram of DC output versus the inverse wind velocity. What are your 
#*    observations?
#* b. Fit the model
#*    
#*    DCOutput(WindVelocity) = β_0 + (β_1/WindVelocity)
#*    
#*    to the data. Give the estimated parameter values and the standard errors.
#* c. Calculate the 99% conﬁdence interval on β_1.
#* d. Add the best model to the scatter diagram DC output versus the inverse 
#*    wind velocity.
#*    Give a physical interpretation of the parameters β_0 and β_1.
#* e. Calculate the estimated DC output, the 95% conﬁdence interval of the 
#*    response and the 95% prediction interval with a wind velocity of one and 
#*    ten metres per second. Comment your results.
#*------------------------------------------------------------------------------
#* Read the data
path <- file.path("04_Datasets", "windmill.dat")
df <- read.table(path, header = TRUE)
head(df)
str(df)

#*------------------------------------------------------------------------------
#* a. Plot a scatter diagram of DC output versus wind velocity and a scatter 
#*    diagram of DC output versus the inverse wind velocity. What are your 
#*    observations?
#*------------------------------------------------------------------------------
df$WindVelocity.inv <- 1/df$WindVelocity

par(mfrow=c(1,2))
plot(df$DCOutput ~ df$WindVelocity)
plot(df$DCOutput ~ df$WindVelocity.inv)

#   NOTE: The correlation is much more obvious in the inverted plot

#*------------------------------------------------------------------------------
#* b. Fit the model to the data. 
#*    Give the estimated parameter values and the standard errors.
#*------------------------------------------------------------------------------

mod <- lm(df$DCOutput ~ df$WindVelocity.inv)

# Parameter values and standard errors
summary(mod)

# Or accessed directly
## Parameter values (beta_0 and beta_1)
mod$coefficients

## Standard errors
summary(mod)$coefficients[,2]

#*------------------------------------------------------------------------------
#* c. Calculate the 99% conﬁdence interval on β_1.
#*------------------------------------------------------------------------------
confint(mod, level = 0.99)

# Or beta_1 accessed directly:
confint(mod, parm = 2, level = 0.99)

#*------------------------------------------------------------------------------
#* d. Add the best model to the scatter diagram DC output versus the inverse 
#*    wind velocity.
#*    Give a physical interpretation of the parameters β_0 and β_1.
#*------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(df$DCOutput ~ df$WindVelocity.inv)
abline(mod)
grid()

#   original variable
WV.new <- data.frame(WindVelocity.inv=seq(0.2*min(1/df$WindVelocity), 
                                          2*max(1/data$WindVelocity), 
                                          length=101))
DCOutput.pred <- predict(mod.inv, newdata=WV.new)
plot(DCOutput ~ WindVelocity, df, pch=20, xlim=c(0,50), ylim=c(0,3), 
     main="DCOutput versus WindVelocity with best model")
grid()
abline(h=0)

#   add best model
lines(1/WV.new$WindVelocity.inv, DCOutput.pred)

#   add maximum possible DC output
abline(h=mod.inv$coefficients["(Intercept)"], col="blue")

#   minimal wind velocity to produce DC output, 
#   i.e. solve DCOutput(WindVelocity)=0 for WindVelocity
WV.min <- -as.numeric(mod.inv$coefficients["WindVelocity.inv"] / 
                        mod.inv$coefficients["(Intercept)"]);   WV.min
abline(v=WV.min, col="red")

#   REMARKS: The best model fits nicely.
#            If the wind velocity were to tend to infinity then the parameter beta0 would be the maximum possible DC output, that is 2.97886 A.
#            Observing the diagram we can see that a certain minimal wind velocity is necessary to produce a DC output, that is 5.208521 m/s.

#*------------------------------------------------------------------------------
#* e. Calculate the estimated DC output, the 95% conﬁdence interval of the 
#*    response and the 95% prediction interval with a wind velocity of one and 
#*    ten metres per second. Comment your results.
#*------------------------------------------------------------------------------
#   wind velocities of interest
WV.new2 <- data.frame(WindVelocity.inv=c(1/1,1/10))

#   estimated DC output and confidence interval of the response
WV.conf <- predict(mod.inv, newdata=WV.new2, interval="confidence", level=0.95);   WV.conf

#   prediction interval
WV.pred <- predict(mod.inv, newdata=WV.new2, interval="prediction", level=0.95);   WV.pred

#*   REMARKS: With a wind velocity of 10 m/s we obtain an estimated DC output of 
#*            1.427314 A. The confidence interval of the response is 
#*            [1.386768, 1.467861] and the slightly larger prediction interval 
#*            is [1.228331, 1.626298].
#*            On the other hand with a wind velocity of 1 m/s we obtain a 
#*            negative estimated DC output which is useless.
#*            The reason for this is that we are extrapolating outside the 
#*            interval of the measured wind velocities.
#*            In Prob. 7.5.2.d we calculated the minimal wind velocity necessary
#*            to produce a DC output which was considerably bigger than 1 m/s.

#*******************************************************************************
#*  Problem 7.5.3 (Atmospheric Pressure versus Boiling Point of Water, Chap. 1.1)
#*  In 1857 the Scottish physicist James D. Forbes discussed an experiment 
#*  concerning the relationship between atmospheric pressure and the boiling 
#*  point of water. At the time it was known that altitude could be determined 
#*  from atmospheric pressure. In the middle of the nineteenth century, 
#*  barometers were fragile instruments, and Forbes had the idea to replace 
#*  barometers by a simpler measurement of the boiling point of water.
#*  He measured in the Alps and in Scotland at each location the pressure 
#*  [in inHg] with a barometer and the boiling point [in ◦ F] using a thermometer. 
#*  The data set forbes.dat contains the pressure and the boiling point.
#*******************************************************************************
#* a. Plot the boiling point versus the logarithm (base 10) of the pressure. 
#*    What are your observations?
#* b. Fit and plot a straight line to the transformed data points. Give the 
#*    estimated parameter values. What are your observations?
#* c. Fit and plot a straight line to the transformed data points without the 
#*    12th observation. Use the argument subset in lm. Compare the estimated 
#*    parameter values, the standard errors and the residual standard error with
#*    the ﬁt of the full data. What are your observations?
#*    
#*    Solve the next questions without the 12th observation.
#* d. Test the slope for signiﬁcance of regression on the 5% level.
#* e. Calculate the 95% conﬁdence level for the slope.
#* f. Find an estimation of the boiling point at the pressure 26 inHg using the
#*    model ﬁt to the transformed data. Calculate the 95% conﬁdence interval of
#*    the response of this estimator and add the complete conﬁdence interval 
#*    band to the scatter diagram.
#* g. Calculate the 95% prediction interval and add the complete prediction 
#*    interval band to the scatter diagram.
#*------------------------------------------------------------------------------
#* Read data
path <- file.path("04_Datasets", "forbes.dat")
df <- read.table(path, header = TRUE)
head(df)
str(df)

#*------------------------------------------------------------------------------
#* a. Plot the boiling point versus the logarithm (base 10) of the pressure. 
#*    What are your observations?
#*------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(df$Boiling ~ df$Pressure)
grid()
plot(df$Boiling ~ log10(df$Pressure))
grid()

#   Note: With the log the points lie almost perfectly on a straight line
df$Pressure.log <- log10(df$Pressure)

#*------------------------------------------------------------------------------
#* b. Fit and plot a straight line to the transformed data points. Give the 
#*    estimated parameter values. What are your observations?
#*------------------------------------------------------------------------------

model <- lm(Boiling ~ Pressure, data = df)
model.log <- lm(Boiling ~ Pressure.log, data = df)

par(mfrow=c(1,2))
plot(df$Boiling ~ df$Pressure)
grid()
abline(model, col = "red")
plot(df$Boiling ~ df$Pressure.log)
grid()
abline(model.log, col = "red")

coefficients(model)
coefficients(model.log)
summary(model.log)

#   NOTE: The model fits almost perfectly. There is just one point which really
#         deviates from the line.

#*------------------------------------------------------------------------------
#* c. Fit and plot a straight line to the transformed data points without the 
#*    12th observation. Use the argument subset in lm. Compare the estimated 
#*    parameter values, the standard errors and the residual standard error with
#*    the ﬁt of the full data. What are your observations?
#*------------------------------------------------------------------------------
sub <- c(1:11, 13:17)
model.log.2 <- lm(Boiling ~ Pressure.log, data = df, subset = sub)
summary(model.log.2)

par(mfrow=c(1,2))
plot(df$Boiling ~ log10(df$Pressure), main = "With datapoint 12")
grid()
abline(model.log, col = "red")
plot(df$Boiling ~ log10(df$Pressure), main = "Without datapoint 12")
grid()
abline(model.log.2, col = "red")

#   NOTE: The R-squared is even higher now and the straight line seems to 
#         capture the remaining points better.


#*------------------------------------------------------------------------------
#*    Solve the next questions without the 12th observation.
#* d. Test the slope for signiﬁcance of regression on the 5% level.
#*------------------------------------------------------------------------------
# Null hypothesis H_0: beta_1 = 0

coefficients(summary(model.log.2))

#   NOTE: Since the p-value of log10(Pressure) [beta_1 = slopw] is much smaller
#         than 5% we can reject the null hypothesis and conclude there is a 
#         significant linear relationship on the 5% significance level.


#*------------------------------------------------------------------------------
#* e. Calculate the 95% conﬁdence level for the slope.
#*------------------------------------------------------------------------------
confint(model.log.2, parm = 2, level = 0.95)

#*------------------------------------------------------------------------------
#* f. Find an estimation of the boiling point at the pressure 26 inHg using the
#*    model ﬁt to the transformed data. Calculate the 95% conﬁdence interval of
#*    the response of this estimator and add the complete conﬁdence interval 
#*    band to the scatter diagram.
#*------------------------------------------------------------------------------
#* Checking in which range the prediction will be
range(df$Pressure)

df.new <- data.frame(Pressure.log = log10(seq(15, 35, by = 0.1)))

Boil.confint <- predict(model.log.2, newdata = df.new,
                        interval = "confidence",
                        level = 0.95)

# Which values has now the askes pressure of 26?
Boil.confint[df.new$Pressure.log==log10(26),]

#   scatter diagram: Time versus Volume with confidence intervals on the 
#   response
par(mfrow=c(1,1))
plot(Boiling ~ Pressure.log, df, subset=-12, pch=20, 
     main="Boiling versus log(Pressure), subset=-12")
grid()
abline(v=log10(26), col="red")
#   add best model
lines(df.pred$Pressure, Boil.confint[,"fit"], lty=1)
#   add confidence intervals on the response
lines(df.pred$Pressure, Boil.confint[,"lwr"], lty=2)
lines(df.pred$Pressure, Boil.confint[,"upr"], lty=2)

#*------------------------------------------------------------------------------
#* g. Calculate the 95% prediction interval and add the complete prediction 
#*    interval band to the scatter diagram.
#*------------------------------------------------------------------------------
# Predicting with the prediction interval
Boil.preds <- predict(model.log.2, newdata = df.new,
                      interval = "prediction",
                      level = 0.95)
Boil.preds[df.new$Pressure.log == log10(26),]

#   NOTE: The prediction interval is wider than the confidenceinterval

# add prediction intervals
lines(df.new$Pressure.log, Boil.preds[, "lwr"], lty = 3)
lines(df.new$Pressure.log, Boil.preds[, "upr"], lty = 3)
legend("topleft", legend=c("best model", 
                           "confidence intervals on the response", 
                           "prediction intervals"), 
       lty=c(1,2,3), bty="n")



#*******************************************************************************
#* Problem 7.5.4 (Simulation of the Distribution of Regression Parameters) 
#* We want to simulate the distribution of the regression parameters β_0  and 
#* β_1 keeping the explanatory variable x constant only varying the measurement
#* errors ε randomly. Let us choose the simple linear model y = 5+3x and n = 20 
#* uniformly distributed x-values in the interval from x_min = 0 to x_max = 50. 
#* The measurement errors ε_1 , . . . , ε_n are supposed to be normally
#* distributed with mean zero and standard error σ = 5.
#*******************************************************************************
#* We use the command set.seed() so that we can always reproduce exactly the 
#* same simulation.
#* a. Using a for-loop plot successively 100 data sets with simulated errors. 
#*    For each simulated data set add the best straight line. Observe the 
#*    scattering of the points and its corresponding regression line. 
#* b. For each data set with simulated errors record the estimated parameters 
#*    β_0 and β_1 in two separate vectors. Plot the two empirical distributions
#*    of these estimated parameters using histograms with the argument freq=F. 
#*    For each histogram add the density of the theoretical distribution. 
#*    Compare the means and the standard deviations of the simulated parameter 
#*    vectors with the theoretical parameters.
#* c. Repeat the above with 10000 simulations. What do you observe?
#*------------------------------------------------------------------------------
# An example:
set.seed(1337)
x <- runif(n = 20, min = 0, max = 5)
y <- 5+3*x
for(i in 1:100){
  Y <- y * rnorm(n = 20, mean = 0, sd = 5)
  plot(x, Y, ylim = c(-500, 500),
       main = "Data Set with simulated Errors")
  abline(a = 5, b = 3)
  abline(lm(Y ~ x), col = "red")
  dev.flush()
  Sys.sleep(0.2)
}


#*------------------------------------------------------------------------------
#* a. Using a for-loop plot successively 100 data sets with simulated errors. 
#*    For each simulated data set add the best straight line. Observe the 
#*    scattering of the points and its corresponding regression line. 
#*------------------------------------------------------------------------------
sigma <- 5
n <- 20
set.seed(1337)
for(i in 1:100){
  E <- rnorm(n, mean = 0, sd = sigma)
  Y <- y + E
  # estimation
  mod <- lm(Y ~ x)
  # graphic
  plot(x, Y, pch = 20, xlim = c(0, 50), ylim = c(0, 160),
       main = "Data Set with Simulated Errors")
  abline(v = x, lty = 3, col = "grey")
  abline(a = 5, b = 3)
  abline(mod, col = "red")
  dev.flush()
  Sys.sleep(0.2)
}

#   NOTE: The best straight line differs a lot depending on the errors.

#*------------------------------------------------------------------------------
#* b. For each data set with simulated errors record the estimated parameters 
#*    β_0 and β_1 in two separate vectors. Plot the two empirical distributions
#*    of these estimated parameters using histograms with the argument freq=F. 
#*    For each histogram add the density of the theoretical distribution. 
#*    Compare the means and the standard deviations of the simulated parameter 
#*    vectors with the theoretical parameters.
#*------------------------------------------------------------------------------
beta0.sim <- NULL
beta1.sim <- NULL
set.seed(1337)

for(i in 1:100){
  E <- rnorm(n, mean = 0, sd = sigma)
  Y <- y + E
  # estimation
  mod <- lm(Y ~ x)
  beta0.sim[i] <- coefficients(mod)[1]
  beta1.sim[i] <- coefficients(mod)[2]
}

# distributions of beta0 and beta1 with theoretical distributions
op <- par(mfrow = c(1, 2))

# Intercept
sd0.theory <- sigma*sqrt(1/n+mean(x)^2 / sum((x-mean(x))^2))
hist(beta0.sim, xlab = "beta0", freq = F, ylim = c(0, 0.2),
     breaks = seq(-10, 15, by = 1),
     xlim = range(pretty(beta0.sim)),
     col = "gray")

abline(v = mean(beta0.sim) + sd(beta0.sim) * c(-1, 0, 1), col = "blue",
       lwd = 2)

abline(v = beta0 + sd0.theory * c(-1, 0, 1), col = "red")

curve(dnorm(xi, mean = beta0, sd = sd0.theory),
      xname = "xi", from = range(pretty(beta0.sim))[1],
      to = range(pretty(beta0.sim))[2], n = 1001,
      col = "red", add = T)

# slope
sd1.theory <- sqrt(sigma^2 / sum((x - mean(x))^2))
hist(beta1.sim, xlab = "beta1", freq = F, ylim = c(0, 6), 
     breaks = seq(2,4, by = 0.05), xlim = range(pretty(beta1.sim)), col = "gray")

abline(v = mean(beta1.sim) + sd(beta1.sim) * c(-1, 0, 1), col = "blue",
       lwd = 2)

abline(v = beta1 + sd1.theory * c(-1, 0, 1), col = "red")

curve(dnorm(xi, mean = beta1, sd = sd1.theory), xname = "xi", 
      from = range(pretty(beta1.sim))[1],
      to = range(pretty(beta1.sim))[2], 
      n = 1001, col = "red", add = T)

par(op)


#*------------------------------------------------------------------------------
#* c. Repeat the above with 10000 simulations. What do you observe?
#*------------------------------------------------------------------------------
beta0.sim <- NULL
beta1.sim <- NULL
set.seed(1337)
for(i in 1:10000){
  E <- rnorm(n, mean=0, sd=sigma)
  Y <- y + E
  #   estimation
  mod <- lm(Y ~ x)
  beta0.sim[i] <- coefficients(mod)[1]
  beta1.sim[i] <- coefficients(mod)[2]
}

#   distributions of beta0 and beta1 with theoretical distributions
op <- par(mfrow=c(1,2))
#   intercept
sd0.theory <- sigma*sqrt(1/n+mean(x)^2/sum((x-mean(x))^2))
hist(beta0.sim, xlab="beta0", freq=F, ylim=c(0,0.2), breaks=seq(-10,15,by=1), 
     xlim=range(pretty(beta0.sim)), col="gray")

abline(v=mean(beta0.sim)+sd(beta0.sim)*c(-1,0,1), col="blue", lwd=2)

abline(v=beta0+sd0.theory*c(-1,0,1), col="red")

curve(dnorm(xi, mean=beta0, sd=sd0.theory), xname="xi", 
      from=range(pretty(beta0.sim))[1], 
      to=range(pretty(beta0.sim))[2], 
      n=1001, col="red", add=T)

#   slope
sd1.theory <- sqrt(sigma^2/sum((x-mean(x))^2))
hist(beta1.sim, xlab="beta1", freq=F, ylim=c(0,6), breaks=seq(2,4,by=0.05), 
     xlim=range(pretty(beta1.sim)), col="gray")

abline(v=mean(beta1.sim)+sd(beta1.sim)*c(-1,0,1), col="blue", lwd=2)

abline(v=beta1+sd1.theory*c(-1,0,1), col="red")

curve(dnorm(xi, mean=beta1, sd=sd1.theory), xname="xi", 
      from=range(pretty(beta1.sim))[1], 
      to=range(pretty(beta1.sim))[2], 
      n=1001, col="red", add=T)

par(op)

#   REMARKS: The more simulations are carried out, the closer are the expected 
#             values calculated from the simulation results to the theoretical 
#             values. This has to be this way: if we repeat the simulation 
#             infinitely often, then the theoretical value would come out.