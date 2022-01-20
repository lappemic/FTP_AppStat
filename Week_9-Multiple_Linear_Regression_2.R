################################################################################
# AppStat Exercises from week 9
################################################################################
# Exercises from the script
#*------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#   load R-function to do residual analysis (written by A. Ruckstuhl)
source(file.path("02_R-Scripts", "RFn_Plot-lmSim.R"))

#*******************************************************************************
#* Problem 9.4.1 (Rental Price Index of Zurich). 
#* The data set RPI-ZH.dat contains the Rental Price Index of the city of Zurich 
#* (RPI), the Mortgage Interest of the Z¨urcher Kantonalbank (MI) and the Consumer 
#* Price Index of the canton of Zurich (CPI) for each quarter between 1994 and 
#* May 2005.
#* From legal regulations on rental rates, it is clear that the mortgage rate 
#* and the consumer price index strongly aﬀect the rental price. Consider the 
#* following model
#* 
#*      RPI = β_0 + β_1 * CPI + β_2 *  MI + ε
#*      
#* Assume that the errors ε are independent normally distributed with expected 
#* value 0 and variance σ^2.
#*******************************************************************************
#* a. Fit the model to the data. Report the estimated coeﬃcients. Overall, do 
#*    the explanatory variables inﬂuence the target values?
#* b. Interpret the coeﬃcient of determination for this ﬁt.
#* c. Calculate a 98% conﬁdence interval on β_1 
#* d. Predict the rental price index for a mortgage interest of 4.5% and a 
#*    consumer price index of 100.5. Report also a 95% prediction interval for 
#*    this prediction.
#*------------------------------------------------------------------------------
path <- file.path("04_Datasets", "RPI-ZH.dat")
data <- read.table(path, header=TRUE)
str(data)
#*------------------------------------------------------------------------------
#* a. Fit the model to the data. Report the estimated coeﬃcients. Overall, do 
#*    the explanatory variables inﬂuence the target values?
#*------------------------------------------------------------------------------
mod <- lm(RPI ~ CPI + MI, data)
summary(mod)

#*   REMARKS:     We find 
#*                - beta0.hat = -68.70831
#*                - beta1.hat = 1.61830
#*                - beta2.hat = 2.00691 and 
#*                - sigma.hat=1.008
#*                
#*                The overall test has F=272.7 on (2,43) degrees of fredom and 
#*                its P-value is much smaller than 5%.
#*                Therefore at least one of the coefficients beta1 and beta2 is 
#*                different of zero.
#*                
#* 
#* The same F-statistic and P-value we obtain with an anova and the trivial model
#* with an intercept only.
mod.const <- lm(RPI ~ 1, data)
anova(mod, mod.const)

#*------------------------------------------------------------------------------
#* b. Interpret the coeﬃcient of determination for this ﬁt.
#*------------------------------------------------------------------------------
#   In multiple regression  the coefficient of determination, often called R^2
#   is identical to the squared correlation between the response variable and the
#   fitted values. The coefficient of determination is a measure of the linear 
#   relationship between the response variable and the fit.
summary(mod)$r.squared

cor(data$RPI, fitted(mod))^2

#   visualisation
plot(RPI ~ fitted(mod), data, xlim=c(90,110), ylim=c(90,110), pch=20)
grid()
abline(a=0, b=1, lty=3)

#   REMARKS:  92.7% of the variability in the response is explained by the 
#             explanatory variables CPI and MI. This is a very good fit.

#*------------------------------------------------------------------------------
#* c. Calculate a 98% conﬁdence interval on β_1 
#*------------------------------------------------------------------------------
confint(mod, parm=2, level=0.98)

#*------------------------------------------------------------------------------
#* d. Predict the rental price index for a mortgage interest of 4.5% and a 
#*    consumer price index of 100.5. Report also a 95% prediction interval for 
#*    this prediction.
#*------------------------------------------------------------------------------

#   data frame for the new observation
data.new <- data.frame(CPI=100.5, MI=4.5)
#   prediction and 95% prediction interval
predict(mod, newdata=data.new, interval="prediction", level=0.95)


#*******************************************************************************
#* Problem 9.4.2 (Catheter, cf. [32], Reg3, Problem 2). 
#* Heart surgery in children requires a customised catheter. The optimal length
#* of a catheter [in cm] depends on the size [in cm] and the weight of a child. 
#* The idea is to estimate the length of a catheter from the patient data. Consider
#* the following model
#* 
#*          length = β_0 + β_1 * size + β_2 * weigth + ε
#*          
#* Assume that the errors ε are independent normally distributed with expected 
#* value 0 and variance σ^2 . The data set catheter.dat contains recorded catheter 
#* lengths and patient data.
#*******************************************************************************
#* a. Fit the model to the data. Overall, do the explanatory variables inﬂuence 
#*    the target values?
#* b. Test the null hypothesis H_0: β_1 = 0 and H_0: β_2 = 0. 
#*    Does the result contradict the overall test?
#* c. Check the assumptions on the errors with a normal q-q plot.
#* d. Remove the observation with the smallest residual and repeat the analysis. 
#*    Compare the results and report your conclusions.
#* e. Report a table of 95% prediction intervals for all observations. In reality
#*    a prediction error of ±2 cm is acceptable. Is this model and the data 
#*    therefore useful in practice?
#* f. Repeat question e. with simpler reduced models. Report all your ﬁndings and
#*    conclusions.
#* g. Repeat question e. with simpler reduced models and the reduced data set 
#*    from question d. Report all your ﬁndings and conclusions.
#*------------------------------------------------------------------------------
path <- file.path("04_Datasets", "catheter.dat")
data <- read.table(path, header=TRUE)
str(data)
summary(data)

#   REMARK: These are data from infants to adolescents.

#*------------------------------------------------------------------------------
#* a. Fit the model to the data. Overall, do the explanatory variables inﬂuence 
#*    the target values?
#*------------------------------------------------------------------------------
mod <- lm(Length ~ Size + Weight, data = data)
summary(mod)

#   REMARKS: The overall test has F=18.65 on (2,9) degrees of fredom and 
#             P-value=0.0006301 < 5%.
#            Therefore at least one of the coefficients beta1 and beta2 is 
#             different of zero.

cor(data$Size, data$Weight)
#   REMARKS: The explanatory variables Size and Weight are strongly correlated. 
#             Is obvious! See also (g).

#*------------------------------------------------------------------------------
#* b. Test the null hypothesis H_0: β_1 = 0 and H_0: β_2 = 0. 
#*    Does the result contradict the overall test?
#*------------------------------------------------------------------------------
#   1. The P-value for the hypothesis H0: beta1=0 is 0.6070 > 5%.
#      => The coefficient might well be zero.
#   2. The P-value for the hypothesis H0: beta2=0 is 0.2753 > 5%.
#      => The coefficient might well be zero.
#   But this does not imply that both coefficients might be zero. 
#   So, no contradiction to (a).


#*------------------------------------------------------------------------------
#* c. Check the assumptions on the errors with a normal q-q plot.
#*------------------------------------------------------------------------------
#   residual analysis
op <- par(mfcol=c(1,2))
#   q-q plot
plot(mod, which=2, pch=20)
grid()
plot.lmSim(mod, which=2, SEED=1)
grid()
par(op)

#   REMARK: The few data points do not scatter nicely around a straight line 
#           and show a clear deviation towards a heavy-tailed distribution.
#           However, the deviations are within the stochastic fluctuation (gray).
#           Therefore, we have no indications of a deviation from the normal distribution.


#*------------------------------------------------------------------------------
#* d. Remove the observation with the smallest residual and repeat the analysis. 
#*    Compare the results and report your conclusions.
#*------------------------------------------------------------------------------
ind <- which.min(residuals(mod));   ind

# fit regression model
mod.red <- lm(Length ~ Size + Weight, data, subset=-ind)
summary(mod.red)

#   REMARK: Residual standard error and standard errors of the coeficients are significantly smaller with the new fit.
#           The coefficient beta2 is now significant, i.e. there is an influence of the weight on the target variable.
#           The explanatory variable Size, on the other hand, plays an unimportant role.

#*------------------------------------------------------------------------------
#* e. Report a table of 95% prediction intervals for all observations. In reality
#*    a prediction error of ±2 cm is acceptable. Is this model and the data 
#*    therefore useful in practice?
#*------------------------------------------------------------------------------
Length.pred <- predict(mod, interval="prediction");   Length.pred
##    Warning message:
##    In predict.lm(mod, interval = "prediction") :
##      predictions on current data refer to _future_ responses

#   some plots
op <- par(mfrow=c(1,2))
#   length of prediction intervals
plot(data$Length, Length.pred[,"upr"] - Length.pred[,"lwr"], ylim=c(0,40), type="h",
     xlab="Length", ylab="Length of Prediction Intervals")
grid()
abline(h=2, col="red")
abline(h=0)

#   predicted versus real
plot(Length.pred[,"fit"] ~ data$Length, pch=20, xlim=c(0,80), ylim=c(0,80),
     xlab="Length", ylab="Fitted Values")
grid()
abline(a=0,b=1, lty=3)
#   add prediction intervals
segments(x=data$Length,y=Length.pred[,"lwr"], x0=data$Length,y0=Length.pred[,"upr"])
par(op)

#   REMARK: Prediction intervals are almost ten times too large (tolerance ± 2 cm
#            -> interval ± ~20cm).
#           Therefore this model is not at all useful to predict the length of a catheter.

#*------------------------------------------------------------------------------
#* f. Repeat question e. with simpler reduced models. Report all your ﬁndings and
#*    conclusions.
#*------------------------------------------------------------------------------
mod.1 <- lm(Length ~ Size, data)
summary(mod.1)

mod.2 <- lm(Length ~ Weight, data)
summary(mod.2)

#   data with best fit
op <- par(mfrow=c(1,2))
plot(Length ~ Size, data, pch=20, main="Length versus Size with best model")
grid()
abline(mod.1)

plot(Length ~ Weight, data, pch=20, main="Length versus Weight with best model")
grid()
abline(mod.2)
par(op)

Length.pred1 <- predict(mod.1, interval="prediction")
Length.pred2 <- predict(mod.2, interval="prediction")
op <- par(mfrow=c(2,2))
#   length of prediction intervals
plot(data$Length, Length.pred1[,"upr"] - Length.pred1[,"lwr"], ylim=c(0,40), type="h",
     xlab="Length", ylab="Length of Prediction Intervals", main="Length versus Size")
grid()
abline(h=2, col="red")
abline(h=0)
plot(data$Length, Length.pred2[,"upr"] - Length.pred2[,"lwr"], ylim=c(0,40), type="h",
     xlab="Length", ylab="Length of Prediction Intervals", main="Length versus Weight")
grid()
abline(h=2, col="red")
abline(h=0)

#   predicted versus real
plot(Length.pred1[,"fit"] ~ data$Length, pch=20, xlim=c(0,80), ylim=c(0,80),
     xlab="Length", ylab="Fitted Values")
grid()
abline(a=0,b=1, lty=3)
#   add prediction intervals
segments(x=data$Length,y=Length.pred1[,"lwr"], x0=data$Length,y0=Length.pred1[,"upr"])
plot(Length.pred2[,"fit"] ~ data$Length, pch=20, xlim=c(0,80), ylim=c(0,80),
     xlab="Length", ylab="Fitted Values")
grid()
abline(a=0,b=1, lty=3)
#   add prediction intervals
segments(x=data$Length,y=Length.pred2[,"lwr"], x0=data$Length,y0=Length.pred2[,"upr"])
par(op)

#   REMARK: Prediction intervals are still almost then times too large.
#           Therefore this model is not at all useful to predict the length of a catheter.


#*------------------------------------------------------------------------------
#* g. Repeat question e. with simpler reduced models and the reduced data set 
#*    from question d. Report all your ﬁndings and conclusions.
#*------------------------------------------------------------------------------
mod.1.red <- lm(Length ~ Size, data, subset=-ind)
summary(mod.1.red)

mod.2.red <- lm(Length ~ Weight, data, subset=-ind)
summary(mod.2.red)

#   data with best fit
op <- par(mfrow=c(1,2))
plot(Length ~ Size, data=data[-ind,], pch=20, main="Length versus Size with best model")
grid()
abline(mod.1.red)

plot(Length ~ Weight, data=data[-ind,], pch=20, main="Length versus Weight with best model")
grid()
abline(mod.2.red)
par(op)

Length.pred1.red <- predict(mod.1.red, interval="prediction")
Length.pred2.red <- predict(mod.2.red, interval="prediction")
op <- par(mfrow=c(2,2))
#   length of prediction intervals
plot(data$Length[-ind], Length.pred1.red[,"upr"] - Length.pred1.red[,"lwr"], ylim=c(0,40), type="h",
     xlab="Length", ylab="Length of Prediction Intervals", main="Length versus Size (reduced data set)")
grid()
abline(h=2, col="red")
abline(h=0)
plot(data$Length[-ind], Length.pred2.red[,"upr"] - Length.pred2.red[,"lwr"], ylim=c(0,40), type="h",
     xlab="Length", ylab="Length of Prediction Intervals", main="Length versus Weight (reduced data set)")
grid()
abline(h=2, col="red")
abline(h=0)

#   predicted versus real
plot(Length.pred1.red[,"fit"] ~ data$Length[-ind], pch=20, xlim=c(0,80), ylim=c(0,80),
     xlab="Length", ylab="Fitted Values")
grid()
abline(a=0,b=1, lty=3)
#   add prediction intervals
segments(x=data$Length[-ind],y=Length.pred1.red[,"lwr"], x0=data$Length[-ind],y0=Length.pred1.red[,"upr"])
plot(Length.pred2.red[,"fit"] ~ data$Length[-ind], pch=20, xlim=c(0,80), ylim=c(0,80),
     xlab="Length", ylab="Fitted Values")
grid()
abline(a=0,b=1, lty=3)
#   add prediction intervals
segments(x=data$Length[-ind],y=Length.pred2.red[,"lwr"], x0=data$Length[-ind],y0=Length.pred2.red[,"upr"])
par(op)

#   REMARK: Prediction intervals are still too large.
#           The only model which reduced the prediction error significantly is 
#           the model Length ~ Weight with the reduced data set.
#           But still, this model is not yet useful to predict the length of a catheter.
#           => We need other explanatory variables.
