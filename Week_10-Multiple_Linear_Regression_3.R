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
