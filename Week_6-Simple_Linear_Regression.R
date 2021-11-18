################################################################################
# AppStat Exercises from week 6
################################################################################
# Exercises from the script
#*------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#*******************************************************************************
#* Problem 7.5.1 (Venice Sea Level, Example 5.1)
#* The annual maximum sea levels [in cm] in Venice, 1931-1981 have been recorded
#* by P. A. Pirazzoli. The data set venice.dat contains the annual maximum tides
#* at Venice for the 51 years.
#*******************************************************************************
#* a. Represent the data in a scatter diagram sea level versus year and describe
#*    the functional context in words.
#* b. Fit a straight line to the data points. Give the estimated parameter values.
#* c. Add the model in the scatter diagram. Comment on the solution.
#* d. Does the data support the hypothesis that Venice sinks?
#*------------------------------------------------------------------------------
#* Read the data
path <- file.path("04_Datasets", "venice.dat")
df <- read.table(path, header = TRUE)
head(df)
str(df)

#*------------------------------------------------------------------------------
#* a. Represent the data in a scatter diagram sea level versus year and describe
#*    the functional context in words.
#*------------------------------------------------------------------------------
plot(df$Year, df$SeaLevel, main = "Sea Level vs. Year")
grid()
