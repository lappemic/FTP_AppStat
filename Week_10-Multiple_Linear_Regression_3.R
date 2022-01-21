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



#*******************************************************************************
#* Problem 10.5.3 (Robust Regression, cf. [32], Reg4, Problem 3). 
#* Study Example 10.4.4 Syntetic data to illustrate lmrob.R. The data set is 
#* lmrob-syntetic.dat. You need the instal the R-packages robustbase and rgl.
#*******************************************************************************
#   load package
require(robustbase)
require(rgl)

path <- file.path("04_Datasets", "lmrob-syntetic.dat")
data <- read.table(path, header=TRUE)
str(data)
summary(data)

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Least-squares estimation
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod.ls <- lm(y ~ x1 + x2, data)
summary(mod.ls)

#   diagnostic tools
op <- par(mfcol=c(2,4))
#   Tukey-Anscombe plot
plot(mod.ls, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.ls, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.ls, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.ls, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.ls, which=2, pch=20)
grid()
plot.lmSim(mod.ls, which=2, SEED=1)
grid()

#   residuals against leverages
plot(mod.ls, which=5, pch=20)
grid()
abline(h=0, lty=3)
par(op)

#   REMARKS: All the diagnostic plots do not show any suspicious observations.
#            => Perfect model!

#   B U T:   There are ouliers which are visible in the following 3D-plot.

#   3D-plot
open3d()
options(rgl.printRglwidget = TRUE)
plot3d(x=data$x1,
       y=data$x2,
       z=data$y,
       radius=1,
       type="s",
       col="red")
#   add least-squares estimator
x1.new <- seq(10, 30, length=101)
x2.new <- seq(-20, 5, length=101)
data.new <- expand.grid(data.frame(x1=x1.new, x2=x2.new))
mod.ls.pred <- predict(mod.ls, newdata=data.new)
z.col <- (mod.ls.pred-min(mod.ls.pred)) / diff(range(mod.ls.pred))
col <- rainbow(200)[z.col*200+1]
surface3d(x=x1.new,
          y=x2.new,
          z=matrix(mod.ls.pred, ncol=101, nrow=101),
          col=col,
          axes=F)

#   Conclusion: The model is completely wrong!
#               => Use robust regression instead.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   robust MM-estimation
#--------------------------------------------------------------------------------------------------------------------------------------------------
require(robustbase)
require(rgl)
mod.rob <- lmrob(y ~ x1 + x2, data)
summary(mod.rob)

#   3D-plot
open3d()
options(rgl.printRglwidget = TRUE)
plot3d(x=data$x1,
       y=data$x2,
       z=data$y,
       radius=1,
       type="s",
       col="red")
#   add robust MM-estimator
x1.new <- seq(10, 30, length=101)
x2.new <- seq(-20, 5, length=101)
data.new <- expand.grid(data.frame(x1=x1.new, x2=x2.new))
mod.rob.pred <- predict(mod.rob, newdata=data.new)
z.col <- (mod.rob.pred-min(mod.rob.pred)) / diff(range(mod.rob.pred))
col <- rainbow(200)[z.col*200+1]
surface3d(x=x1.new,
          y=x2.new,
          z=matrix(mod.rob.pred, ncol=101, nrow=101),
          col=col,
          axes=F)

#   diagnostic tools
op <- par(mfcol=c(2,3))
plot(mod.rob)
par(op)

#   REMARKS: The first diagram at the top left replaces the classic diagram residuals versus leverages.
#            There are clearly 2*4=8 outliers visible in all diagrams.
#            7 of them are also leverage points.
#            These 8 outliers are already listed in the summary output as outliers.
#            These 8 outliers can also be seen in the 3D-plot.
#            If there are more than two explanatory variables in a model we cannot rely on 3D-plots anymore!


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Least-squares estimation without outliers
#--------------------------------------------------------------------------------------------------------------------------------------------------
ind.out <- c(7,17,27,37,47,57,67,77)
mod.ls.red <- lm(y ~ x1 + x2, data, subset=-ind.out)
summary(mod.ls.red)

#   REMARK: This is more or less the same result as with the robust MM-regression.


