################################################################################
# AppStat Exercises from week 4
################################################################################
# Exercises from the script
#------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#*******************************************************************************
# Problem 4.3.1 (CUSUM of Individual Measurements)
#* The diameter of diﬀerent holes with the same nominal size is regularly
#* measured automatically. You can ﬁnd the data in diameter.dat 
#* (cf. Prob. 2.3.1). Examine if the process is under control with a CUSUM chart. 
#* The target value is µ 0 = 10.0. 
#*******************************************************************************
path <- file.path("04_Datasets", "diameter.dat")
df <- read.table(path, header = TRUE)
head(df)
summary(df)
str(df)
