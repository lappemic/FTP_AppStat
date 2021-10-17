################################################################################
# AppStat Exercises from week 3
################################################################################
# Exercises from the script
#------------------------------------------------------------------------------
setwd("~/OneDrive/Learning/MSE/2021hs/FTP_AppStat/Additional_material_AppStat/04 Datasets")

#*******************************************************************************
# Problem 3.6.1 (Western Electric Rules)
#* Examine if the process of Ex. 2.2.1 is under control according to the 
#* Western Electric Rules. Just look at the chart for ¯x which is based on 
#* the chart for R.
#*******************************************************************************
path <- file.path("04_Datasets", "ignition-keys.dat")
df <- read.table(path, header = TRUE)
head(df)
summary(df)
str(df)

m <- length(df); m
df$mean <- apply(df[,1:m], 1, mean)
df$sd <- apply(df[,1:m], 1, sd)
df$R <- apply(df[,1:m], 1, function(x){ max(x) - min(x) })
str(df)
