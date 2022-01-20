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

#*------------------------------------------------------------------------------
#* a. Fit the model to the data. Overall, do the explanatory variables inﬂuence 
#*    the target values?
#*------------------------------------------------------------------------------
mod <- lm(Length ~ Size + Weight, data = data)
#*------------------------------------------------------------------------------
#* b. Test the null hypothesis H_0: β_1 = 0 and H_0: β_2 = 0. 
#*    Does the result contradict the overall test?
#*------------------------------------------------------------------------------

#*------------------------------------------------------------------------------
#* c. Check the assumptions on the errors with a normal q-q plot.
#*------------------------------------------------------------------------------

#*------------------------------------------------------------------------------
#* d. Remove the observation with the smallest residual and repeat the analysis. 
#*    Compare the results and report your conclusions.
#*------------------------------------------------------------------------------

#*------------------------------------------------------------------------------
#* e. Report a table of 95% prediction intervals for all observations. In reality
#*    a prediction error of ±2 cm is acceptable. Is this model and the data 
#*    therefore useful in practice?
#*------------------------------------------------------------------------------

#*------------------------------------------------------------------------------
#* f. Repeat question e. with simpler reduced models. Report all your ﬁndings and
#*    conclusions.
#*------------------------------------------------------------------------------

#*------------------------------------------------------------------------------
#* g. Repeat question e. with simpler reduced models and the reduced data set 
#*    from question d. Report all your ﬁndings and conclusions.
#*------------------------------------------------------------------------------