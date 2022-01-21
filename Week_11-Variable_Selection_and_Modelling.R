################################################################################
# AppStat Exercises from week 11
################################################################################
# Exercises from the script
#*------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#   load R-function to do residual analysis (written by A. Ruckstuhl)
source(file.path("02_R-Scripts", "RFn_Plot-lmSim.R"))

#*******************************************************************************
#* Problem 11.4.1 (Experiment on a Dairy Product, cf. [32], Reg5, Problem 1). 
#* We come back to Ex. 9.4.4.
#* a. Fit the model
#* 
#* log(O2UP) = β 0 + β 1 · BOD + β 2 · TKN + β 3 · TS + β 4 · TVS + β 5 · COD + ε.
#* 
#*      Perform a variable selection with a backward elimination using AIC.
#* b. Fit the trivial model
#* 
#*                      log(O2UP) = β 0 + ε.
#*      
#*      Perform a variable selection with a forward selection using AIC.
#* c. Fit the model
#* 
#*              log(O2UP) = β 0 + β 1 · BOD + β 3 · TS + ε.
#*      
#*      Perform a variable selection with a search in both directions using AIC.
#* d. Compare your results. Which model(s) would you like to investigate further?
#* e. Check for collinearity in the data.
#* f. Repeat the above without the observations with index i = 2, 3 and 17. 
#*      How sensitive is variable selection with respect to these observations?
#*******************************************************************************
path <- file.path("04_Datasets", "oxygen.dat")
data <- read.table(path, header=TRUE)
str(data)
summary(data)
