################################################################################
# AppStat Exercises from week 14
################################################################################
# Exercises from the script
#*------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#   load R-function to do residual analysis (written by A. Ruckstuhl)
source(file.path("02_R-Scripts", "RFn_Plot-lmSim.R"))

#   load package
require(daewr)

#*******************************************************************************
#* Problem 13.5.1 (Compression of Single-Wall Containers, cf. [9], Example 2.14). 
#* The article Compression of Single-Wall Corrugated Shipping Containers Using 
#* Fixed and Floating Test Platens in the Journal of Testing and Evaluation, 
#* 1992, p. 318-320, describes an experiment in which several diﬀerent types of 
#* boxes were compared with respect to compression strength.
#* 
#*      *Data Table in Script*
#*      
#*******************************************************************************
#* a. Enter the data yourself in R and represent it with notched box-plots. 
#*    You need to deﬁne a factor (Box) for the type of box.
#* b. Is there a diﬀerence between diﬀerent types of boxes? Perform a statistical 
#*    hypothesis test on the 5% level.
#*------------------------------------------------------------------------------
