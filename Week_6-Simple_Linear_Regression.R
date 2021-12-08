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
#*------------------------------------------------------------------------------
