################################################################################
# AppStat Exercises from week 3
################################################################################
# Exercises from the script
#------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#*******************************************************************************
# Problem 3.6.1 (Western Electric Rules)
#* Examine if the process of Ex. 2.2.1 is under control according to the 
#* Western Electric Rules. Just look at the chart for ¯x.bar which is based on 
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

# Calculating the means of the means, standard deviations and the ranges
x.barbar <- mean(df$mean); x.barbar
R.bar <- mean(df$R); R.bar  
sd.bar <- mean(df$sd); sd.bar

D3 <- 0       # from table for n = 4
D4 <- 2.282   # from table for n = 4
d2 <- 2.059   # from table for n = 4

# Calculating the LCL, CL and the UCL
R.CL <- c(D3, 1, D4) * R.bar; R.CL

# Plotting the R chart:
plot(df$R, pch = 20, ylim = c(0, 0.03),
     ylab = "Ranges", main = "R chart")
lines(df$R)
abline(h = R.CL, lty = c(2, 1, 2))
text(rep(1, 1), R.CL, label = c("LCL", "CL", "UCL"), pos = 3)
#   NOTE: The process seems under control

# Plotting the x.bar chart
# First calculating the process standard deviation:
sigma.R.hat <- R.bar / d2; sigma.R.hat

# Furthermore one needs the process mean, which is already calculated
x.barbar

# With these estimates one can calculate the control limits of the x.bar chart
n <- 4
x.bar.CL <- x.barbar + c(-3, 0, 3) * sigma.R.hat / sqrt(n); x.bar.CL

# Now draw the x.bar plot with its centrelines
plot(df$mean, pch = 20, ylim = c(0.19, 0.21),
     ylab = "Mean Values", main = "x.bar Chart (based on R chart)")
lines(df$mean)
abline(h = x.bar.CL, lty = c(2, 1, 2))
text(rep(1, 1), x.bar.CL, labels = c("LCL", "CL", "UCL"), pos = 3)
