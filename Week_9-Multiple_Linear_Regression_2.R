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

#*------------------------------------------------------------------------------
#* a. Fit the model to the data. Report the estimated coeﬃcients. Overall, do 
#*    the explanatory variables inﬂuence the target values?
#*------------------------------------------------------------------------------

#*------------------------------------------------------------------------------
#* b. Interpret the coeﬃcient of determination for this ﬁt.
#*------------------------------------------------------------------------------

#*------------------------------------------------------------------------------
#* c. Calculate a 98% conﬁdence interval on β_1 
#*------------------------------------------------------------------------------

#*------------------------------------------------------------------------------
#* d. Predict the rental price index for a mortgage interest of 4.5% and a 
#*    consumer price index of 100.5. Report also a 95% prediction interval for 
#*    this prediction.
#*------------------------------------------------------------------------------