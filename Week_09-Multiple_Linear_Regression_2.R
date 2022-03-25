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


#*******************************************************************************
#* Problem 9.4.3 (Surface Finish, cf. [23], Example 12.13). 
#* A mechanical engineer is investigating the surface ﬁnish of metal parts 
#* produced on a lathe and its relationship to the speed [in revolutions per 
#* minute] of the lathe. Two diﬀerent types of cutting tools were used. 
#* The data set surface-finish.dat contains 20 measurements of the quality 
#* (Quality), the speed of the lathe (RPM) and the type of the cutting tool (Type).
#*******************************************************************************
#* a. Represent the data in a scatter diagram.
#* b. Fit a regression model with a linear relationship between the quality and 
#*    the rotational speed and with the same slope for both types of the cutting 
#*    tool but diﬀerent intercepts. Add the best ﬁts to the scatter diagram of 
#*    question a.
#* c. Fit a regression model with a linear relationship between the quality and 
#*    the rotational speed and with individual slopes and intercepts for both 
#*    types of the cutting tool. Add the best ﬁts to the scatter diagram of 
#*    question a.
#* d. Test the hypothesis of equal slopes on the 5% level.
#*------------------------------------------------------------------------------
path <- file.path("04_Datasets", "surface-finish.dat")
data <- read.table(path, header=TRUE)
str(data)
# Type should be a factor
data$Type <- as.factor(data$Type)

summary(data)

#*------------------------------------------------------------------------------
#* a. Represent the data in a scatter diagram.
#*------------------------------------------------------------------------------
plot(Quality ~ RPM, data, pch=20, col=as.integer(Type)+1, 
     xlim=c(0,300), ylim=c(0,60), main="Quality versus RPM")
grid()
abline(h=0,v=0)
legend("topleft", pch=20, col=c(2,3), legend=levels(data$Type))

#   REMARK: We can observe two point clouds, one for Type="DM302" and the other 
#           for Type="DM416" which scatter around two straight lines.

#*------------------------------------------------------------------------------
#* b. Fit a regression model with a linear relationship between the quality and 
#*    the rotational speed and with the same slope for both types of the cutting 
#*    tool but diﬀerent intercepts. Add the best ﬁts to the scatter diagram of 
#*    question a.
#*------------------------------------------------------------------------------
mod.1 <- lm(Quality ~ RPM + Type, data)
summary(mod.1)

#   REMARKS:
#   1. The first straight line belonging to Type="DM302" has intercept=14.276196 
#       and slope=0.141150.
#   2. The second straight line belonging to Type="DM416" has 
#       intercept=14.276196-13.280195=0.996001 and slope=0.141150.

#   add the best fit to the scatter diagram
coef(mod.1)[1:2]
coef(mod.1)[3]
abline(coef(mod.1)[1:2], col=2)
abline(coef(mod.1)[1:2] + c(coef(mod.1)[3],0), col=3)

#*------------------------------------------------------------------------------
#* c. Fit a regression model with a linear relationship between the quality and 
#*    the rotational speed and with individual slopes and intercepts for both 
#*    types of the cutting tool. Add the best ﬁts to the scatter diagram of 
#*    question a.
#*------------------------------------------------------------------------------
mod.2 <- lm(Quality ~ RPM * Type, data)
summary(mod.2)

#   REMARKS:
#   1. The first straight line belonging to Type="DM302" has intercept=11.50294 
#       and slope=0.15293.
#   2. The second straight line belonging to Type="DM416" has 
#       intercept=11.50294-6.09423=5.40871 and slope=0.15293-0.03057=0.12236.

#   add the best fit to the scatter diagram
abline(coef(mod.2)[1:2], col=2, lty=2)
abline(coef(mod.2)[1:2] + coef(mod.2)[3:4], col=3, lty=2)

#*------------------------------------------------------------------------------
#* d. Test the hypothesis of equal slopes on the 5% level.
#*------------------------------------------------------------------------------
summary(mod.2)$coef

#   REMARKS: In the lm-summary-output we look at the line of "RPM:TypeDM416".
#            The Estimate -0.03056953 is the difference in the slopes of the two lines.
#            Its P-value=9.239159e-02 is bigger than 0.05 and therefore we accept the hypothesis of equal slopes.
#            On the other hand, we would reject it on the 10% level.
#            It is of course always possible to make a type II error, but since we have no further information
#            about the lathe process we cannot say anything.
#            We should ask the mechanical engineer who collected the data and knows the machine.


#*******************************************************************************
#* Problem 9.4.4 (Experiment on a Dairy Product, cf. [32], Reg3, Problem 4). 
#* The data in the ﬁle oxygen.dat come from an experiment on a dairy product. We
#* want to empirically model the oxygen uptake (O2UP) [in milligrams per minute] 
#* from the data collected using ﬁve chemical measurements. Chemical measurements 
#* include both biological (BOD) and chemical (COD) oxygen demand, Kjeldahl’s 
#* total nitrogen content (TKN), total solids (TS) and volatile solids (TVS) that 
#* are part of the solids. 
#* All chemical measurements are given in milligrams per liter.
#* 
#* The measurements were carried out on a sample of a dairy product which was 
#* kept in a water suspension for 220 days. All observations were made on the 
#* same suspension over time. The aim of this analysis is a ﬁrst modelling of 
#* the dependence of the logarithmic oxygen uptake on the chemical variables. 
#* We will omit observations 1 and 20 in the following analysis. The resulting 
#* model ßwill then be checked later for its predictive suitability.
#*******************************************************************************
#* a. Fit the model
#* 
#*  log(O2UP) = β_0 + β_1 * BOD + β_2 * TKN + β_3 * TS + β_4 * TVS + β_5 * COD + ε
#*  
#*    Report your ﬁndings on the P-values. Comment on the F-statistic in the 
#*    summary-lmoutput. What are the consequences for the ﬁtted model?
#* b. Compare the full model in a with the reduced model
#* 
#*            log(O2UP) = β_0 + β_3 * TS + β_5 * COD + ε
#*    
#*    Report your conclusions.
#*------------------------------------------------------------------------------
path <- file.path("04_Datasets", "oxygen.dat")
data <- read.table(path, header=TRUE)
str(data)
summary(data)

data$O2UP.log    <- log10(data$O2UP)

#*------------------------------------------------------------------------------
#* a. Fit the model
#* 
#*  log(O2UP) = β_0 + β_1 * BOD + β_2 * TKN + β_3 * TS + β_4 * TVS + β_5 * COD + ε
#*  
#*    Report your ﬁndings on the P-values. Comment on the F-statistic in the 
#*    summary-lmoutput. What are the consequences for the ﬁtted model?
#*------------------------------------------------------------------------------
mod.full <- lm(O2UP.log ~ BOD + TKN + TS + TVS + COD, data, subset=-c(1,20))
summary(mod.full)

#   REMARKS:
#   1. All estimated coefficients except the intercept and TS are not significantly different of zero.
#      In extremis this could imply that the explanatory variables have no influence on O2UP.log.
#   2. On the other hand, the F-statistic with P=1.815e-05 < 0.05 rejects the fear mentioned above.

#*------------------------------------------------------------------------------
#* b. Compare the full model in a with the reduced model
#* 
#*            log(O2UP) = β_0 + β_3 * TS + β_5 * COD + ε
#*    
#*    Report your conclusions.
#*------------------------------------------------------------------------------
mod.red <- lm(O2UP.log ~ TS + COD, data, subset=-c(1,20))
summary(mod.red)

#   compare models with an anova
anova(mod.full, mod.red)

#   REMARKS:
#   1. The anova-comparison shows that the reduced model describes the data as well as the full model.
#   2. The null hypothesis beta1=beta2=beta4=0, on the other hand, cannot be rejected since Pr(>F)=0.9805 > 0.05.

