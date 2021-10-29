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
x.bar.CL <- x.barbar + c(-3, -2, -1, 0, 1, 2, 3) * sigma.R.hat / sqrt(n); x.bar.CL

# Now draw the x.bar plot with its centrelines
plot(df$mean, pch = 20, ylim = c(0.19, 0.21),
     ylab = "Mean Values", main = "x.bar Chart (based on R chart)")
lines(df$mean)
abline(h = x.bar.CL, lty = c(2, 3, 3, 1, 3, 3, 2))
text(rep(1, 1), x.bar.CL, labels = c("LCL", "CL", "UCL"), pos = 3)

#* The wester electric rules for monitoring control charts are defined as 
#* follows:
#* The process is out of control if any of the following applies
#* 1. Any single data point falls outside the limit defined by UCL and LCL
#*    (beyond the 3*sigma-limit)
#* 2. Two out of three consecutive points fall beyond the limit defined by 
#*    2/3 UCL and 2/3 LCL on the same side of the centreline (beyond the 
#*    2*sigma-limit)
#* 3. Four out of five consecutive points fall beyond the limit defined by 
#*    1/3 UCL and 1/3 LCL on the same side of the centreline (beyond the 
#*    2*sigma-limit)
#* 4. Nine consecutive points fall on the same side of the centreline 
#*    (so-called run)
#*    
#*    NOTE for created chart:
#* 1. No data point falls outside the limit defined by UCL and LCL
#* 2. No observation where 2 out of 3 consecutive points fall beyond the limit
#*    defined by the 2/3 UCL or LCL
#* 3. No observations where 4 out of 5 consecutive points fall beyond the limit
#*    defined by the 1/3 UCL or LCL
#* 4. No observation where 9 consecutive points fal beyond the same side of the 
#*    centreline
#*    
#* CONCLUSION: According to the western electric rules the process is 
#*              under control


#*******************************************************************************
# Problem 3.6.2 (OC Curve)
#* Determine an approximate operating characteristic curve of the chart for 
#* x.bar (based on the s chart) of Ex. 2.2.1. The target value µ_0 and the
#* process standard deviation σ are unknown and can be estimated from the data.
#* Calculate the probability of an omitted alarm if 
#* ∆µ = µ_1 − µ.bar_0 = δ σ.bar = 0.015 cm.
#*******************************************************************************
#* loading df of Ex. 2.2.1
path <- file.path("04_Datasets", "ignition-keys.dat")
df <- read.table(path, header = TRUE)
str(df)

# mean values, standard deviations and ranges
df$mean <- apply(df[,1:4], 1, mean); df$mean
df$sd <- apply(df[,1:4], 1, sd); df$sd
df$R <- apply(df[,1:4], 1, function(x){ max(x) - min(x)}); df$R
str(df)

# first we check if the process is under control with the x.bar chart
# (based on s chart)

#--------------------------------------------------------------------------------------------------------------------------------------------------
# s Chart
#--------------------------------------------------------------------------------------------------------------------------------------------------
# mean of standard deviations
s.bar <- mean(df$sd); s.bar
B3 <- 0       # from table for n=4
B4 <- 2.266   # from table for n=4
s.CL <- c(B3, 1, B4) * s.bar; s.CL

# plotting s chart
plot(df$sd, pch = 20, ylim = c(0, 0.012),
     ylab = "Standard Deviations", main = "s Chart")
lines(df$sd)
abline(h = s.CL, lty = c(2, 1, 2))
text(rep(1,1), s.CL, labels = c("LCL", "CL", "UCL"), pos = 3)
#   NOTE: Process is under control. 

#--------------------------------------------------------------------------------------------------------------------------------------------------
# x.bar Chart
#--------------------------------------------------------------------------------------------------------------------------------------------------
# estimatioin of the process standard deviations
c4 <- 0.9213
sigma.s.hat <- s.bar / c4; sigma.hat

# calculation of process mean
x.barbar <- mean(as.matrix(df[,1:4])); x.barbar

# UCL and LCL
n <- 4
x.bar.CL <- x.barbar + c(-3, 0, 3) * sigma.s.hat / sqrt(n); x.bar.CL

# plotting x.bar Chart
plot(df$mean, pch = 20, ylim = c(0.19, 0.21),
     ylab = "Mean Values", main = "x.bar Chart (based on s Chart)")
lines(df$mean)
abline(h = x.bar.CL, lty = c(2, 1, 2))
text(rep(1,3), x.bar.CL, labels = c("LCL", "CL", "UCL"), pos = 3)
#   NOTE: The process is under control


#--------------------------------------------------------------------------------------------------------------------------------------------------
# Power function
#--------------------------------------------------------------------------------------------------------------------------------------------------
# target value
mu0 <- x.barbar

# standard deviation
sigma <- sigma.s.hat

# q-quantile
alpha <- 0.0027
z.q <- qnorm(1 - alpha/2); z.q

# define power function
func.g.tilde <- function(delta, n){
                        pnorm(delta * sqrt(n) - z.q) + 
                        pnorm(-delta * sqrt(n) - z.q)
}

# graphic
curve(func.g.tilde(delta = x, n = 4), from = -4, to = 4, lty = 1,
      xlab = "delta", ylab = "Power Function", main = "Power Function")
abline(v = 0, h = 0)

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   OC Curve
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   define OC curve
func.OC <- function(delta, n) { 1 - func.g.tilde(delta, n) }

#   graphic
curve(func.OC(delta=x, n=4), from=0, to=4, lty=1, 
      xlab="delta", ylab="OC", main="OC Curve")
abline(v=0, h=0)




#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Probability of an omitted alarm
#--------------------------------------------------------------------------------------------------------------------------------------------------
