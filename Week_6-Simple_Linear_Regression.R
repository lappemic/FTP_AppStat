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

