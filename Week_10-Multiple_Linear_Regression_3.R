################################################################################
# AppStat Exercises from week 10
################################################################################
# Exercises from the script
#*------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#   load R-function to do residual analysis (written by A. Ruckstuhl)
source(file.path("02_R-Scripts", "RFn_Plot-lmSim.R"))

#*******************************************************************************
#* Problem 10.5.1 (Rental Price Index of Zurich, cf. [32], Reg4, Problem 1)
#* We come back to Prob. 9.4.1. Make a residual analysis of the ﬁtted model and 
#* improve the model. Report all your ﬁndings and conclusions.
#*******************************************************************************
path <- file.path("04_Datasets", "RPI-ZH.dat")
data <- read.table(path, header=TRUE)
str(data)
summary(data)

# Fit model
mod <- lm(RPI ~ CPI + MI, data)
summary(mod)

#   diagnostic tools
op <- par(mfcol=c(2,4))
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

#   residuals against leverages
plot(mod, which=5, pch=20)
grid()
abline(h=0, lty=3)
par(op)

#   REMARKS
#   1.  Tukey-Anscombe plot: Is the expected value constant around zero?
#       There are two groups in which a falling trend is visible.
#       This leads to two saw teeth function in the smoother, which lie outside of the stochastic fluctuations.
#       => The expected value of the residuals cannot be constant.
#   2.  Scale-location plot: Is the variance constant?
#       The oszillation of the smooth function is partially outside the stochastic fluctuation.
#       => The variance of the residuals cannot be constant.
#   3.  q-q plot: Are the residuals normally distributed.
#       Long tail is apparent.
#       Points are systematically different from the straight line, which is not extreme (compare with simulation).
#       Effect is stronger on the left side.
#       => There is no hint that the residuals are not normally distributed.
#   4.  Residuals versus leverage: Are there influential observations?
#       The distance measure of Cook is smaller than 1 for all observations and all leverages are smaller than 0.2.
#       => No bad observations.

#   CONCLUSION: The fit is not satisfactory at all.
#               We need to ask the owner of the data if she can explain why there are two groups?
#               => The answers to question 9.4.1.b-c are therefore wrong!

#   One possibility could be a time dependence.
plot(data$RPI, pch=20, main="RPI versus Time (index)")
grid()
abline(v=27.5, col="gray")

#   REMARK: At index 28 there is an abrupt jump in the rental price index of Zurich (RPI) which cannot be 
#           explained with the available data.
#           We define a dummy-variable.

data$group <- factor(c(rep("early", 27), rep("late", nrow(data)-27)))
points(data$RPI, pch=20, col=data$group)

mod.fac <- lm(RPI ~ CPI + MI + group, data)
summary(mod.fac)

anova(mod, mod.fac)

#   REMARK: The factor group is highly significant.

#   diagnostic tools
op <- par(mfcol=c(2,4))
#   Tukey-Anscombe plot
plot(mod.fac, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.fac, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.fac, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.fac, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.fac, which=2, pch=20)
grid()
plot.lmSim(mod.fac, which=2, SEED=1)
grid()

#   residuals against leverages
plot(mod.fac, which=5, pch=20)
grid()
abline(h=0, lty=3)
par(op)

#   REMARK: Now all the diagnostic plots show no violation of the assumptions.
#           Only question left is. Why is there this rupture in the rental price index of Zurich (RPI)?
#           We need to ask the owner of the data if she can explain why there are two groups?


#*******************************************************************************
#* Problem 10.5.2 (Experiment on a Dairy Product, cf. [32], Reg4, Problem 2). 
#* We come back to Prob. 9.4.4. Make a residual analysis of the ﬁtted model on 
#* the complete data. What was the motivation to omit the observations with 
#* index i = 1 and 20? Report all your ﬁndings and conclusions.
#*******************************************************************************
path <- file.path("04_Datasets", "oxygen.dat")
data <- read.table(path, header=TRUE)
str(data)
summary(data)

#   define new log-transformed variables
data$O2UP.log    <- log10(data$O2UP)


#   Fit full model with all data
mod.full.all <- lm(O2UP.log ~ BOD + TKN + TS + TVS + COD, data)
summary(mod.full.all)

#   diagnostic tools
op <- par(mfcol=c(2,4))
#   Tukey-Anscombe plot
plot(mod.full.all, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.full.all, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.full.all, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.full.all, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.full.all, which=2, pch=20)
grid()
plot.lmSim(mod.full.all, which=2, SEED=1)
grid()

#   residuals against leverages
plot(mod.full.all, which=5, pch=20)
grid()
abline(h=0, lty=3)
par(op)

#   REMARKS
#   1.  Tukey-Anscombe plot: Is the expected value constant around zero?
#       No obvious discrepancies.
#       => There is no hint that the expected value of the residuals is not constant.
#   2.  Scale-location plot: Is the variance constant?
#       No obvious discrepancies.
#       => There is no hint that the variance of the residuals is not constant.
#   3.  q-q plot: Are the residuals normally distributed.
#       Observations with index i=1 and 20 might be outliers.
#       => But there is no hint that the residuals are not normally distributed.
#   4.  Residuals versus leverage: Are there influential observations?
#       Observation with index i=17 has a distance measure of Cook which is bigger than 1.
#       Some observations have a leverage which is bigger than 0.2.
#       Observations with index i=1 and 20 are again marked as influential observations.

