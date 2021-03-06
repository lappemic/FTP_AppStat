################################################################################
# AppStat Exercises from week 8
################################################################################
# Exercises from the script
#*------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#   load R-function to do residual analysis (written by A. Ruckstuhl)
source(file.path("02_R-Scripts", "RFn_Plot-lmSim.R"))

#*******************************************************************************
#* Problem 8.5.1 (Vending Machines)
#* Let us look at the data vending-machines.dat of Ex. 7.1.1 from the softdrink 
#* vending machines.
#*******************************************************************************
#* a. Fit the simple linear model
#* 
#*    Time = β_0 + β_1 * Volume + ε
#* 
#*    to the data.
#* b. Perform a complete residual analysis assuming that the errors are 
#*    independent, normally distributed with expected value zero and constant 
#*    variance. Report all your ﬁndings and conclusions.
#* c. Apply the following Tukey’s ﬁrst aid transformation:
#* 
#*      log(Time) = β_0 + β_1 * log(Volume) + ε
#*      log(Time) = β_0 + β_1 * √ Volume + ε
#*    
#*    Perform a complete residual analysis for both variants. Report all your 
#*    ﬁndings and conclusions, compare with the model in Eqn. 8.5.a.
#*------------------------------------------------------------------------------
path <- file.path("04_Datasets", "vending-machines.dat")
data <- read.table(path, header=TRUE)
str(data)

#*------------------------------------------------------------------------------
#* a. Fit the simple linear model
#*  
#*    Time = β_0 + β_1 * Volume + ε
#* 
#*    to the data.
#*------------------------------------------------------------------------------
#   Estimation of the parameters
mod <- lm(Time ~ Volume, data);   mod
summary(mod)

#*------------------------------------------------------------------------------
#* b. Perform a complete residual analysis assuming that the errors are 
#*    independent, normally distributed with expected value zero and constant 
#*    variance. Report all your ﬁndings and conclusions.
#*------------------------------------------------------------------------------

#   Scatter diagram: Time versus Volume
plot(Time ~ Volume, data, pch=20, xlim=c(0,30), ylim=c(0,80), main="Delivery Time versus Delivery Volume")
grid()
abline(mod)

#   residual analysis
op <- par(mfcol=c(2,3))
#   Tukey-Anscombe plot
plot(mod, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod, which=2, pch=20)
grid()
plot.lmSim(mod, which=2, SEED=1)
grid()
par(op)

#   short form without the grids and different order of the plots
op <- par(mfrow=c(2,3))
plot(mod, which=1:3, pch=20)
plot.lmSim(mod, which=1:3, SEED=1)
par(op)

#   REMARKS:
#   1.  Tukey-Anscombe plot shows outlier with index i=9 which affects smooth curve.
#       In the simulation it is visible that the original curve is extreme.
#       => The expected value of the residuals cannot be constant.
#   2.  Scale-location plot shows a clear upwards trend.
#       In the simulation it is visible that the original curve is extreme.
#       => The scattering of the residuals is not constant.
#   3.  q-q plot shows a slightly heavy tail and the outlier with index i=9 is 
#       again obvious.
#       => Residuals are not normally distributed.

#   CONCLUSION: The fit is not satisfactory. Try transformations of response and 
#               explanatory variable.

#*------------------------------------------------------------------------------
#* c. Apply the following Tukey’s ﬁrst aid transformation:
#* 
#*      log(Time) = β_0 + β_1 * log(Volume) + ε
#*      log(Time) = β_0 + β_1 * √ Volume + ε
#*    
#*    Perform a complete residual analysis for both variants. Report all your 
#*    ﬁndings and conclusions, compare with the model in Eqn. 8.5.a.
#*------------------------------------------------------------------------------

data$Time.log   <- log10(data$Time)
data$Volume.log <- log10(data$Volume)
data$Volume.sq  <- sqrt(data$Volume)

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   1. Variant
mod1 <- lm(Time.log ~ Volume.log, data);   mod1
summary(mod1)

#   Scatter diagram: Time versus Volume
plot(Time.log ~ Volume.log, data, pch=20, xlim=log10(c(1,30)), ylim=log10(c(1,80)), main="log(Delivery Time) versus log(Delivery Volume)")
grid()
abline(mod1)

#   residual analysis
op <- par(mfcol=c(2,3))
#   Tukey-Anscombe plot
plot(mod1, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod1, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod1, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod1, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod1, which=2, pch=20)
grid()
plot.lmSim(mod1, which=2, SEED=1)
grid()
par(op)

#   REMARKS:
#   1.  Tukey-Anscombe plot shows strange behaviour on the right side.
#       In the simulation it is visible that the original curve is extreme.
#       => The expected value of the residuals cannot be constant.
#   2.  Scale-location plot is okay.
#       => There is no hint that the scattering of the residuals is not constant.
#   3.  q-q plot is okay.
#       => There is no hint that the residuals are not normally distributed.

#   CONCLUSION: The fit is satisfactory. But let us try also the square-root transformation.

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   2. Variant
mod2 <- lm(Time.log ~ Volume.sq, data);   mod2
summary(mod2)

#   Scatter diagram: Time versus Volume
plot(Time.log ~ Volume.sq, data, pch=20, xlim=sqrt(c(0,30)), ylim=log10(c(1,80)), main="log(Delivery Time) versus sqrt(Delivery Volume)")
grid()
abline(mod2)

#   REMARK: Nice fit!

#   residual analysis
op <- par(mfcol=c(2,3))
#   Tukey-Anscombe plot
plot(mod2, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod2, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod2, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod2, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod2, which=2, pch=20)
grid()
plot.lmSim(mod2, which=2, SEED=1)
grid()
par(op)

#   REMARKS:
#   1.  Tukey-Anscombe plot is okay.
#       => The expected value of the residuals is constant.
#   2.  Scale-location plot is okay.
#       => The scattering of the residuals is constant.
#   3.  q-q plot is okay.
#       => Residuals are normally distributed.

#   CONCLUSION: The fit is perfect.


#-------------------------------------------------------------------------------
#   Correlation of the Residuals
#-------------------------------------------------------------------------------
#*   WARNING: We absolutely have NO idea if the chronological order in the data 
#*            set "vending-machines.dat" has never been changed.
#*            At least there is no obvoius order visible in the data set, except 
#*            the ordering with respect to the Town.
#*            => We will look at the correlation of the residuals assuming that 
#*            in the data set "vending-machines.dat" we still have the original order.
op <- par(mfcol=c(2,3))
ts0 <- residuals(mod)
max0 <- max(abs(range(ts0)))
plot(ts0, ylim=max0*c(-1,1), type="h", xlab="Index i", ylab="Residuals", 
     main="Delivery Time versus Delivery Volume")
grid()
lines(ts0, type="h")
abline(h=0)

#   correlation:  R[t+1] ~ R[t]
plot(ts0[-length(ts0)], ts0[-1], pch=20, xlim=max0*c(-1,1), ylim=max0*c(-1,1), xlab="e[i]", ylab="e[i+1]")
grid()

ts1 <- residuals(mod1)
max1 <- max(abs(range(ts1)))
plot(ts1, ylim=max1*c(-1,1), type="h", xlab="Index i", ylab="Residuals", 
     main="log(Delivery Time) versus log(Delivery Volume)")
grid()
lines(ts1, type="h")
abline(h=0)
#   correlation:  R[t+1] ~ R[t]
plot(ts1[-length(ts1)], ts1[-1], pch=20, xlim=max1*c(-1,1), ylim=max1*c(-1,1), xlab="e[i]", ylab="e[i+1]")
grid()

ts2 <- residuals(mod2)
max2 <- max(abs(range(ts2)))
plot(ts2, ylim=max2*c(-1,1), type="h", xlab="Index i", ylab="Residuals", main="log(Delivery Time) versus sqrt(Delivery Volume)")
grid()
lines(ts2, type="h")
abline(h=0)

#   correlation:  R[t+1] ~ R[t]
plot(ts2[-length(ts2)], ts2[-1], pch=20, xlim=max2*c(-1,1), ylim=max2*c(-1,1), xlab="e[i]", ylab="e[i+1]")
grid()
par(op)

#   REMARKS: Model 0 and 2 have slightly positiv correlated residuals.
#            Model 1 on the other hand has uncorrelated residuals.
#            Since the order in the data set might not correspond anymore to the
#             original order we safely ignore these findings.


#*******************************************************************************
#* Problem 8.5.2 (Anscombe Quartet). 
#* In 1973 F. J. Anscombe published, cf. [1], four diﬀerent data sets each 
#* containing an x- and a y-variable, cf. Ex. 8.1.3. All four artiﬁcial data sets
#* can be found in anscombe.dat.
#*******************************************************************************
#* a. Fit for each of the four data sets a simple linear model. Report in a 
#*    table all intercepts, slopes, corresponding standard errors, residual 
#*    standard errors and coeﬃcients of determination.
#* b. b. Reproduce Fig. 8.1.v and report all your ﬁndings and conclusions.
#*------------------------------------------------------------------------------
path <- file.path("04_Datasets", "anscombe.dat")
data <- read.table(path, header=TRUE)
str(data)

#*------------------------------------------------------------------------------
#* a. Fit for each of the four data sets a simple linear model. Report in a 
#*    table all intercepts, slopes, corresponding standard errors, residual 
#*    standard errors and coeﬃcients of determination.
#*------------------------------------------------------------------------------
mod1 <- lm(Y1 ~ X1, data)
summary(mod1)

mod2 <- lm(Y2 ~ X2, data)
summary(mod2)

mod3 <- lm(Y3 ~ X3, data)
summary(mod3)

mod4 <- lm(Y4 ~ X4, data)
summary(mod4)

#   Summary table
Tab1 <- rbind(summary(mod1)$coefficients,
              summary(mod2)$coefficients,
              summary(mod3)$coefficients,
              summary(mod4)$coefficients)
rownames(Tab1) <- paste0(rep(1:4, each=2), ". 
                         Anscombe Quartet: ", 
                         rep(c("Intercept", "Slope"), 4))
Tab1

Tab2 <- rbind(c(sigma=summary(mod1)$sigma, R2=summary(mod1)$r.squared),
              c(sigma=summary(mod2)$sigma, R2=summary(mod2)$r.squared),
              c(sigma=summary(mod3)$sigma, R2=summary(mod3)$r.squared),
              c(sigma=summary(mod4)$sigma, R2=summary(mod4)$r.squared))
rownames(Tab2) <- paste0(1:4, ". Anscombe Quartet: ")
Tab2



#*------------------------------------------------------------------------------
#* b. Reproduce Fig. 8.1.v and report all your ﬁndings and conclusions.
#*------------------------------------------------------------------------------
op <- par(mfrow=c(2,2))
plot(data$X1, data$Y1, pch=20, xlab="x", ylab="y", xlim=c(0,20), ylim=c(0,15), main="1. Anscombe Quartet")
grid()
abline(mod1)
plot(data$X2, data$Y2, pch=20, xlab="x", ylab="y", xlim=c(0,20), ylim=c(0,15), main="2. Anscombe Quartet")
grid()
abline(mod2)
plot(data$X3, data$Y3, pch=20, xlab="x", ylab="y", xlim=c(0,20), ylim=c(0,15), main="3. Anscombe Quartet")
grid()
abline(mod3)
plot(data$X4, data$Y4, pch=20, xlab="x", ylab="y", xlim=c(0,20), ylim=c(0,15), main="4. Anscombe Quartet")
grid()
abline(mod4)
par(op)



#*******************************************************************************
#* Problem 8.5.3 (Understand q-q Plot with Simulation)
#* With the following simulation you will become familiar mit the q-q plot.
#*******************************************************************************
#* a. Normal distribution. Draw n = 10, 20, 50 and 100 random numbers from a 
#*    standard normal distribution and check normality with a q-q plot. 
#*    Repeat this several times to get an idea of the randomness of the 
#*    deviations in the q-q plot.
#* b. Heavy-tailed distribution. Draw n = 10 and 100 random numbers from a 
#*    Student’s t-distribution with 20, 7 and 3 degrees of freedom and check 
#*    normality with a q-q plot. Repeat this several times to get an idea of 
#*    the randomness of the deviations in the q-q plot.
#* c. Skewed Distribution. Draw n = 10 and 100 random numbers from a 
#*    χ^2 -distribution with 20 and 1 degrees of freedom and check normality 
#*    with a q-q plot. Repeat this several times to get an idea of the 
#*    randomness of the deviations in the q-q plot.
#*------------------------------------------------------------------------------
par(mfrow=c(1,1))

#*------------------------------------------------------------------------------
#* a. Normal distribution. Draw n = 10, 20, 50 and 100 random numbers from a 
#*    standard normal distribution and check normality with a q-q plot. 
#*    Repeat this several times to get an idea of the randomness of the 
#*    deviations in the q-q plot.
#*------------------------------------------------------------------------------
n <- 1000 # -> 10 20 50 100
set.seed(1)
for(i in 1:100){
  x <- rnorm(n, mean=0, sd=1)
  #   q-q plot
  par(mfrow=c(1,1))
    qqnorm(x, pch=20, xlim=10*c(-1,1), ylim=10*c(-1,1), 
         main="q-q plot: Standard Normal Distribution")
  qqline(x, col="red")
  grid()
  dev.flush()
  Sys.sleep(0.3)
}

#*------------------------------------------------------------------------------
#* b. Heavy-tailed distribution. Draw n = 10 and 100 random numbers from a 
#*    Student’s t-distribution with 20, 7 and 3 degrees of freedom and check 
#*    normality with a q-q plot. Repeat this several times to get an idea of 
#*    the randomness of the deviations in the q-q plot.
#*------------------------------------------------------------------------------
n <- 20 # 20 100
dfree <- 3 # 3 7 20
set.seed(1)
for(i in 1:50){
  x <- rt(n, df=dfree)
  #   q-q plot
  par(mfrow=c(1,1))
  qqnorm(x, pch=20, xlim=10*c(-1,1), ylim=10*c(-1,1), main="q-q plot: Student's t Distribution")
  qqline(x, col="red")
  grid()
  dev.flush()
  Sys.sleep(0.3)
}


#*------------------------------------------------------------------------------
#* c. Skewed Distribution. Draw n = 10 and 100 random numbers from a 
#*    χ^2(chi^2) -distribution with 20 and 1 degrees of freedom and check normality 
#*    with a q-q plot. Repeat this several times to get an idea of the 
#*    randomness of the deviations in the q-q plot.
#*------------------------------------------------------------------------------
n <- 100 # 20 100
dfree <- 3 # 1 20
set.seed(1)
for(i in 1:50){
  x <- rchisq(n, df=dfree)
  #   q-q plot
  par(mfrow=c(1,1))
  qqnorm(x, pch=20, xlim=10*c(-1,1), ylim=10*c(-1,1), main="q-q plot: chi^2 Distribution")
  qqline(x, col="red")
  grid()
  dev.flush()
  Sys.sleep(0.3)
}


#*******************************************************************************
#* Problem 8.5.4 (Windmill). 
#* We come back to Prob. 7.5.2. Investigate the following regression models:
#* 
#* DCOutput       =   β_0 + β_1 * WindVelocity + ε
#* 
#* 
#* log(DCOutput)  =   β_0 + β_1 * log(WindVelocity) + ε
#* 
#*                                β_1
#* DCOutput       =   β_0 + ------------- + ε
#*                          WindVelocity
#* 
#* Perform a complete residual analysis for the variants. 
#* Report all your ﬁndings and conclusions. Which model is the best?
#*******************************************************************************
path <- file.path("04_Datasets", "windmill.dat")
data <- read.table(path, header=TRUE)
str(data)

#   define new variables
data$WindVelocity.log <- log10(data$WindVelocity)
data$WindVelocity.inv <- 1/data$WindVelocity
data$DCOutput.log     <- log10(data$DCOutput)


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (1) Naiv Model
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters
mod <- lm(DCOutput ~ WindVelocity, data)
summary(mod)

#   Scatter diagram: DCOutput ~ WindVelocity
par(mfrow=c(1,1))
plot(DCOutput ~ WindVelocity, data, pch=20, xlim=c(5,25), ylim=c(0,3), 
     main="DCOutput ~ WindVelocity")
grid()
abline(mod)

#   REMARK: It is obvious that the straight line does not appropriately fit the curved data.

#   residual analysis
op <- par(mfcol=c(2,3))
#   Tukey-Anscombe plot
plot(mod, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod, which=2, pch=20)
grid()
plot.lmSim(mod, which=2, SEED=1)
grid()
par(op)

#   REMARKS:
#   1.  Tukey-Anscombe plot shows very curved structure.
#       In the simulation it is visible that the original curve is extreme.
#       => The expected value of the residuals cannot be constant.
#   2.  Scale-location plot shows a weak downward trend.
#       In the simulation it is visible that the original curve is within the 19 
#       simulated curves.
#       => There is no hint that the scattering of the residuals is not constant.
#   3.  q-q plot shows no obvious discrepancy.
#       => There is no hint that the residuals are not normally distributed.

#   CONCLUSION: The fit is not satisfactory at all. 
#               Try log-transformations of response and explanatory variable.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (2) Model with First Aid Transformations of Response and Explanatory Variable
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters
mod.loglog <- lm(DCOutput.log ~ WindVelocity.log, data)
summary(mod.loglog)

#   Scatter diagram: DCOutput ~ WindVelocity
par(mfrow=c(1,1))
plot(DCOutput.log ~ WindVelocity.log, data, pch=20, xlim=log10(c(5,25)), ylim=log10(c(0.1,3)), main="log(DCOutput) ~ log(WindVelocity)")
grid()
abline(mod.loglog)
#identify(data$WindVelocity.log, data$DCOutput.log)
##  25

#   REMARK: It is obvious that the straight line does not appropriately fit the 
#           curved data.
#           Especially the observation with index i=25 is extremely far away 
#           from the straight line and will be marked as an outlier in the 
#           following residual analysis.

#   residual analysis
op <- par(mfcol=c(2,3))
#   Tukey-Anscombe plot
plot(mod.loglog, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.loglog, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.loglog, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.loglog, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.loglog, which=2, pch=20)
grid()
plot.lmSim(mod.loglog, which=2, SEED=1)
grid()
par(op)

#   REMARKS:
#   1.  Tukey-Anscombe plot shows very curved structure.
#       In the simulation it is visible that the original curve is extreme.
#       => The expected value of the residuals cannot be constant.
#   2.  Scale-location plot shows an oscillating behavior.
#       In the simulation it is visible that the original curve is somehow 
#       different from the 19 simulated curves.
#       => The scattering of the residuals is not constant.
#   3.  q-q plot shows no obvious discrepancy. Observation with index i=25 is 
#       marked as an outlier.
#       => The residuals are not normally distributed.

#   CONCLUSION: The fit is not satisfactory at all, all three diagnostic tools 
#               show abnormalities.
#               => The first aid transformations were not helpful.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (3) Mechanistic Model based on First Principles
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters
mod.inv <- lm(DCOutput ~ WindVelocity.inv, data)
summary(mod.inv)

#   Scatter diagram: DCOutput ~ WindVelocity
par(mfrow=c(1,1))
plot(DCOutput ~ WindVelocity.inv, data, pch=20, xlim=1/(c(5,25)), ylim=c(0,3), main="DCOutput ~ 1/WindVelocity")
grid()
abline(mod.inv)

#   REMARK: Nice fit!

#   residual analysis
op <- par(mfcol=c(2,3))
#   Tukey-Anscombe plot
plot(mod.inv, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.inv, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.inv, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.inv, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.inv, which=2, pch=20)
grid()
plot.lmSim(mod.inv, which=2, SEED=1)
grid()
par(op)

#   REMARKS:
#   1.  Tukey-Anscombe plot shows weak curved structure.
#       In the simulation it is visible that the original curve is not extreme.
#       => There is no hint that the expected value of the residuals is not constant.
#   2.  Scale-location plot shows a hick.
#       In the simulation it is visible that the original curve is within 19 simulated curves.
#       => There is no hint that the scattering of the residuals is not constant.
#   3.  q-q plot shows no obvious discrepancy except maybe an indication of a 
#       short-tailed distribution at the right end.
#       (Short-tailed distribution of the errors is in general not critical for 
#       least-squares estimation.)
#       => There is no hint that the residuals are not normally distributed.

#   CONCLUSION: The fit is satisfactory, all three diagnostic tools show no abnormalities.
#               => The mechanistic model is the best of all three.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Correlation of the Residuals
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   WARNING: We absolutely have NO idea if the chronological order in the data 
#             set "windmill.dat" has never been changed.
#            At least there is no obvoius order visible in the data set.
#            => We will look at the correlation of the residuals assuming that 
#             in the data set "windmill.dat" we still have the original order.
op <- par(mfcol=c(2,3))
ts0 <- residuals(mod)
max0 <- max(abs(range(ts0)))
plot(ts0, ylim=max0*c(-1,1), type="h", xlab="Index i", ylab="Residuals", 
     main="DCOutput ~ WindVelocity")
grid()
lines(ts0, type="h")
abline(h=0)
#   correlation:  R[t+1] ~ R[t]
plot(ts0[-length(ts0)], ts0[-1], pch=20, xlim=max0*c(-1,1), ylim=max0*c(-1,1), xlab="e[i]", ylab="e[i+1]")
grid()

ts1 <- residuals(mod.loglog)
max1 <- max(abs(range(ts1)))
plot(ts1, ylim=max1*c(-1,1), type="h", xlab="Index i", ylab="Residuals", 
     main="log(DCOutput) ~ log(WindVelocity)")
grid()
lines(ts1, type="h")
abline(h=0)
#   correlation:  R[t+1] ~ R[t]
plot(ts1[-length(ts1)], ts1[-1], pch=20, xlim=max1*c(-1,1), ylim=max1*c(-1,1), 
     xlab="e[i]", ylab="e[i+1]")
grid()

ts2 <- residuals(mod.inv)
max2 <- max(abs(range(ts2)))
plot(ts2, ylim=max2*c(-1,1), type="h", xlab="Index i", ylab="Residuals", main="DCOutput ~ 1/WindVelocity")
grid()
lines(ts2, type="h")
abline(h=0)
#   correlation:  R[t+1] ~ R[t]
plot(ts2[-length(ts2)], ts2[-1], pch=20, xlim=max2*c(-1,1), ylim=max2*c(-1,1), xlab="e[i]", ylab="e[i+1]")
grid()
par(op)

#   REMARKS: All models show uncorrelated residuals.


#*******************************************************************************
#* Problem 8.5.5 (Atmospheric Pressure versus Boiling Point of Water)
#* We come back to Prob. 7.5.3. Investigate the following regression model
#* 
#*  Boiling = β_0 + β_1 * log(Pressure) + ε
#*  
#*  in connection with the complete data set. Perform a complete residual 
#*  analysis. Report all your ﬁndings and conclusions.
#*******************************************************************************
path <- file.path("04_Datasets", "forbes.dat")
data <- read.table(path, header=TRUE)
str(data)

#   define new variable
data$Pressure.log <- log10(data$Pressure)


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (1) Model with log-transformed Pressure
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters
mod.log <- lm(Boiling ~ Pressure.log, data)
summary(mod.log)

#   Scatter diagram: Boiling ~ Pressure.log
par(mfrow=c(1,1))
plot(Boiling ~ Pressure.log, data, pch=20, main="Boiling ~ log(Pressure)")
grid()
abline(mod.log)
#identify(data$Pressure.log, data$Boiling)
##  12

#   REMARK: It is obvious that the straight line fits pretty well.
#           The observation with index i=12 is far away from the straight line 
#           and will be marked as an outlier in the following residual analysis.

#   residual analysis
op <- par(mfcol=c(2,3))
#   Tukey-Anscombe plot
plot(mod.log, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.log, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.log, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.log, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.log, which=2, pch=20)
grid()
plot.lmSim(mod.log, which=2, SEED=1)
grid()
par(op)

#   REMARKS: The outlier with index i=12 destroys most of the plots in the residual analysis.
#            => Remove outlier and try again -> (2)
#


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (2) Model with log-transformed Pressure without Outlier
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Estimation of the parameters
mod.log.red <- lm(Boiling ~ Pressure.log, data, subset=-12)
summary(mod.log.red)

#   Scatter diagram: Boiling ~ Pressure.log
par(mfrow=c(1,1))
plot(Boiling ~ Pressure.log, data, subset=-12, pch=20, main="Boiling ~ log(Pressure) without Outlier")
grid()
abline(mod.log.red)

#   REMARK: The straight line fits pretty well.

#   residual analysis
op <- par(mfcol=c(2,3))
#   Tukey-Anscombe plot
plot(mod.log.red, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.log.red, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.log.red, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.log.red, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.log.red, which=2, pch=20)
grid()
plot.lmSim(mod.log.red, which=2, SEED=1)
grid()
par(op)

#   REMARKS:
#   1.  Tukey-Anscombe plot shows very curved structure.
#       In the simulation it is visible that the original curve is not extreme.
#       => There is no hint that the expected value of the residuals is not constant.
#   2.  Scale-location plot shows a non-constant behaviour.
#       In the simulation it is visible that the original curve is not extreme.
#       => There is no hint that the scattering of the residuals is not constant.
#   3.  q-q plot shows no obvious discrepancy.
#       => There is no hint that the residuals are not normally distributed.

#   CONCLUSION: The fit without the 12th observation is satisfactory.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Correlation of the Residuals
#--------------------------------------------------------------------------------------------------------------------------------------------------
data
barplot(data$Pressure)

#   REMARK: We observe that the variable Pressure is ordered 
#           (except observation i=14). Therefore, the original order is very likely lost.
#           => An investigation of the correlation of the residuals is meaningless.

