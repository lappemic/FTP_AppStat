################################################################################
# AppStat Exercises from week 8
################################################################################
# Exercises from the script
#*------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

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