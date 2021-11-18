################################################################################
# AppStat Exercises from week 5
################################################################################
# Exercises from the script
#*------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#*******************************************************************************
#* Problem 5.3.1 (Hypergeometric Distribution I) 
#* Four out of ten lots win. Determine the probability that out of ﬁve randomly
#*  selected lots there will be
#*******************************************************************************
#* a. exactly two winning lots.
#* b. a maximum of three winning lots.
#* c. more than one winning lot. 
#*------------------------------------------------------------------------------

#*------------------------------------------------------------------------------
#* a. exactly two winning lots.
#*------------------------------------------------------------------------------
p <- 4/10         # percent defective
N <- 10           # lot size
n <- 5            # sample size
c <- 2            # acceptance number

dhyper(x = c, m = p*N, n = (1-p)*N, k = n)

#*------------------------------------------------------------------------------
#* b. a maximum of three winning lots.
#*------------------------------------------------------------------------------
p <- 4/10         # percent defective
N <- 10           # lot size
n <- 5            # sample size
c <- 3            # acceptance number

phyper(q = c, m = p*N, n = (1-p)*N, k = n)

#*------------------------------------------------------------------------------
#* c. more than one winning lot. 
#*------------------------------------------------------------------------------
p <- 4/10         # percent defective
N <- 10           # lot size
n <- 5            # sample size
c <- 1            # acceptance number

phyper(q = c, m = p*N, n = (1-p)*N, k = n, lower.tail = FALSE)


#*******************************************************************************
#* Problem 5.3.2 (Hypergeometric Distribution II) 
#* There are N = m+n balls in an urn, of which m are red and n balls are black.
#* We draw k balls at once. Determine the probability that there are exactly 
#* x red balls among the k balls.
#*******************************************************************************
#* There are
#*    (m+n)
#*    ( k )
#* possibilities in total to arrangke k balls among m+n balls, and exactly
#*    (m)( n )
#*    (x)(k-x)
#* possibilities to arrange x red balls among k balls. The factor
#*    (m)
#*    (x)
#* counts the number of arrangements with x red balls among m red balls, 
#* and the factor
#*    ( n )
#*    (k-x)
#* counts the number of arrangements with k-x black balls among n black balls. 
#* Therefore we obtain
#*            (m)( n )
#*            (x)(k-x)
#*            ---------
#* P(X = x) =   (m+n)
#*              ( k )


#*******************************************************************************
#* Problem 5.3.3 (Acceptance Sampling Plan)
#* In acceptance sampling lots with N = 11 000 bolts are tested. Lots with less 
#* than 1% of bad bolts are rejected in 10% of all cases, though they would have
#* to be accepted as ﬁt for use. On the other hand, lots with more than 5% of
#* bad bolts are accepted at most in 5% of all cases, though their quality does
#* not meet the terms of delivery.
#*******************************************************************************
#* a. What is the producer risk α and the consumer risk β ?
#* b. What is the probability of acceptance p α and a probability of 
#*    rejection p β ?
#* c. Find the parameters of the acceptance sampling plan, i.e. the sample size
#*    n and the acceptance number c.
#*------------------------------------------------------------------------------

N <- 11000

#*------------------------------------------------------------------------------
#* a. Producer and consumer risk
#*------------------------------------------------------------------------------
# Producer risk
alpha <- 0.1
# Consumer risk
beta <- 0.05


#*------------------------------------------------------------------------------
#* b. What is the probability of acceptance p α and a probability of 
#*    rejection p β ?
#*------------------------------------------------------------------------------
# probability of acceptance
p.alpha <- 0.01
# probability of rejection
p.beta <- 0.05

#*------------------------------------------------------------------------------
#* c. Find the parameters of the acceptance sampling plan, i.e. the sample size
#*    n and the acceptance number c.
#*------------------------------------------------------------------------------
#* brute-force solution: calculate all useful combinations
source("Functions-summary-AppStat.R")
n.good <- NULL
c.good <- NULL
for(ni in 1:(N/10)) {   # do not try all
  for(ci in 1:(ni-1)) {
    a.flag <- OC_function(p=p.alpha,N,n=ni,c=ci) >= 1-alpha
    b.flag <- OC_function(p=p.beta, N,n=ni,c=ci) <= beta
    if(a.flag & b.flag){
      n.good <- c(n.good, ni)
      c.good <- c(c.good, ci)
    }
  }
}

#   parameters of all possible acceptance sampling plans
plot(n.good, c.good, pch=20, cex=0.1, main="Parameter n and c of all Plans")

#--------------------------------------------------------------------------------------------------------------------------------------------------
# optimal acceptance sampling plan with the smallest possible sample size

# choose smallest sample size
n <- n.good[1]; n

#   choose acceptance number
c <- c.good[1]; c

#   graphics
curve(OC_function(p=x,N,n=n,c=c), from=0, to=0.1, n=10001, 
      xlab="p", ylab="OC, Acceptance Probability", 
      main="Operating Characteristic")
abline(h=0, v=0)
lines(c(0,p.alpha,p.alpha), c(1-alpha,1-alpha,0), lty=2)
lines(c(0,p.beta,p.beta), c(beta,beta,0), lty=2)
points(c(p.alpha, p.beta), c(1-alpha,beta), pch=20, cex=2)
#   add legend
text(c(0,0,p.alpha+0.002,p.beta-0.002), c(1-alpha+0.02,beta+0.02,0,0),
     pos=c(4,4,3,3), labels=c(expression(1-alpha),
                              expression(beta),
                              expression(p[alpha]),
                              expression(p[beta])))

#   real producer risk, smaller than alpha=0.1
1-OC_function(p=p.alpha,N,n=n,c=c)

#   real consumer risk, smaller than beta=0.05
OC_function(p=p.beta,N,n=n,c=c)


#*******************************************************************************
#* Problem 6.2.2 (Shewhart Control Charts with qcc)
#* A quality inspector in a beverage factory is responsible for the accuracy of
#* the ﬁlling machine of a soft drink. She has collected 25 samples consisting 
#* of four measurements of ﬁll level [in cm], cf. data set soft-drinks.dat. Use 
#* the ﬁrst 20 measured values as a trial run to make the following charts and 
#* check if subsequent measurements are under control.
#*******************************************************************************
#* a. Chart for x.bar
#* b. Chart for R
#* c. Chart for s
#* d. Compare to the charts in Prob. 2.5.2.
#*------------------------------------------------------------------------------
#* Installing and loading qcc package
# install.packages("qcc")
require(qcc)

# loading dataset
path <- file.path("04_Datasets", "soft-drinks.dat")
df <- read.table(path, header = TRUE)
head(df)

#*------------------------------------------------------------------------------
#* a. Chart for x.bar
#*------------------------------------------------------------------------------
?qcc
qcc.xbar <- qcc(df[1:20,], type = "xbar", newdata = df[21:25,], nsigmas = 3)
summary(qcc.xbar)
#   NOTE: The trial run is not under control. Datapoint 12 should be deleted

# taking out datapoint 12
ind <- c(1:11, 13:20); ind
qcc.xbar.mod <- qcc(df[ind,], type = "xbar", newdata = df[21:25,], nsigmas = 3)
summary(qcc.xbar.mod)
#   NOTE: Now the whole process is under control

#*------------------------------------------------------------------------------
#* b. Chart for R
#*------------------------------------------------------------------------------
qcc.R <- qcc(df[1:20,], type = "R", newdata = df[21:25,], nsigmas = 3)
summary(qcc.R)
#   NOTE: Same as above can be observed: DP 12 causes problems

qcc.R.mod <- qcc(df[ind,], type = "R", newdata = df[21:25,], nsigmas = 3)
summary(qcc.R.mod)
#   NOTE: The process is under control now

#*------------------------------------------------------------------------------
#* c. Chart for s
#*------------------------------------------------------------------------------
qcc.s <- qcc(df[1:20,], type = "S", newdata = df[21:25,], nsigmas = 3)
summary(qcc.s)
#   NOTE: Same issue -> remove index 12

qcc.s.mod <- qcc(df[ind, ], type = "S", newdata = df[21:25,], nsigmas = 3)
summary(qcc.s.mod)
#   NOTE: Process is under control
#*------------------------------------------------------------------------------
#* d. Compare to the charts in Prob. 2.5.2.
#*------------------------------------------------------------------------------
#* The same results occured


#*******************************************************************************
#* Problem 6.2.3 (Group Data, Shewhart Control Chart with qcc, Ex. 15-91)
#* The diameter of fuse pins used in an aircraft engine application is an 
#* important quality characteristic. Twenty-ﬁve samples of three pins each were 
#* measured, cf. the data set fuse-pins.dat. The target value is µ 0 = 64.000 mm.
#*******************************************************************************
#* a. Display the data graphically, i.e. measurements, resp. means of
#*    measurements versus sample index.
#* b. Group the data so that it can be further processed with R and the 
#*    package qcc.
#* c. Create charts for x.bar and s.
#*------------------------------------------------------------------------------
#* load data
path <- file.path("04_Datasets", "fuse-pins.dat")
df <- read.table(path, header = TRUE)
head(df)
str(df)
df

mu0 <- 64
#*------------------------------------------------------------------------------
#* a. Display the data graphically, i.e. measurements, resp. means of
#*    measurements versus sample index.
#*------------------------------------------------------------------------------
plot(df$sample, df$diam, cex = 0.7)
lines(tapply(df$diam, df$sample, mean))
#   NOTE: Data looks ok

#*------------------------------------------------------------------------------
#* b. Group the data so that it can be further processed with R and the 
#*    package qcc.
#*------------------------------------------------------------------------------
df.grouped <- qcc.groups(df$diam, df$sample)
str(df.grouped)
head(df.grouped)
#*------------------------------------------------------------------------------
#* c. Create charts for x.bar and s.
#*------------------------------------------------------------------------------
# x.bar chart
qcc.xbar <- qcc(df.grouped, type = "xbar", target = mu0, nsigmas = 3)
summary(qcc.xbar)
#   NOTE: Process seems under control

# s chart
qcc.s <- qcc(df.grouped, type = "S", target = mu0, nsigmas = 3)
summary(qcc.s)
#   NOTE: The process is under control


#*******************************************************************************
#* Problem 6.2.4 (CUSUM and EWMA with qcc, Ex. 15-91) 
#* The diameter of fuse pins used in an aircraft engine application is an 
#* important quality characteristic. Twentyﬁve samples of three pins each were 
#* measured, cf. the data set fuse-pins.dat. The target value is µ 0 = 64.000 mm.
#*******************************************************************************
#* a. Create a CUSUM chart.
#* b. Create a EWMA chart with λ = 0.3.
#* c. The trial run consists of the 25 measurements. Is the new 
#*    sample 63.997, 63.991 and 64.004 under control?
#*------------------------------------------------------------------------------
#* Load dataset
path <- file.path("04_Datasets", "fuse-pins.dat")
df <- read.table(path, header = TRUE)
head(df)
str(df)
df

mu0 <- 64

#*------------------------------------------------------------------------------
#* a. Create a CUSUM chart.
#*------------------------------------------------------------------------------
df.g <- qcc.groups(df$diam, df$ sample)
head(df.g)
?`qcc-package`

qcc.cusum <- cusum(df.g, type = "xbar", target = mu0, plot = FALSE)
summary(qcc.cusum)

plot(qcc.cusum, nsigmas = 3)
#   NOTE: The process is under control


#*------------------------------------------------------------------------------
#* b. Create a EWMA chart with λ = 0.3.
#*------------------------------------------------------------------------------
lambda <- 0.3
qcc.ewma <- ewma(df.g, type = "xbar", target = mu0, plot = FALSE)
summary(qcc.ewma)

plot(qcc.ewma, lambda = lambda, nsigmas = 3)
#   NOTE: Process is under control


#*------------------------------------------------------------------------------
#* c. The trial run consists of the 25 measurements. Is the new 
#*    sample 63.997, 63.991 and 64.004 under control?
#*------------------------------------------------------------------------------
new.sample <- matrix(c(63.997, 63.991, 64.004), ncol = 3); new.sample

# estimate CUSUM and EWMA
qcc.cusum.new <- cusum(df.g, type = "xbar", target = mu0, 
                       newdata = new.sample, plot = FALSE)
qcc.ewma.new <- ewma(df.g, type = "xbar", target = mu0, 
                     newdata = new.sample, plot = FALSE)

# plot the charts
plot(qcc.cusum.new, nsigmas = 3)
plot(qcc.ewma.new, lambda = lambda, nsigmas = 3)
#   NOTE: The new sample is ok.


#*******************************************************************************
#* Problem 6.2.5 (OC with qcc)
#* A quality inspector in a beverage factory is responsible for the accuracy of 
#* the ﬁlling machine of a soft drink. She has collected 25 samples consisting 
#* of four measurements of ﬁll level [in cm], cf. data set soft-drinks.dat. 
#* Create operating characteristic curves for diﬀerent sample sizes n from 
#* the stable trial run.
#*******************************************************************************
#* Load the data
path <- file.path("04_Datasets", "soft-drinks.dat")
df <- read.table(path, header = TRUE)
head(df)
str(df) 

# As we now from previous exercises the trial run is not under control and
# we have to exclude data point 12
ind <- c(1:11, 13:25)
?'qcc-package'
qcc.oc <- qcc(df[ind, ], type = "xbar", plot = FALSE)
oc.curves(qcc.oc)


#*******************************************************************************
#* Problem 6.2.6 (PCR with qcc)
#* A quality inspector in a beverage factory is responsible for the accuracy of 
#* the ﬁlling machine of a soft drink. She has collected 25 samples consisting
#* of four measurements of ﬁll level [in cm], cf. data set soft-drinks.dat. 
#* Estimate the process capability index C_P from the stable trial run for 
#* tolerance limits USL = 18.1 cm and LSL = 15.6 cm. Study the graphics and try 
#* to understand the R output. The information in help(process.capability) 
#* might help.
#*******************************************************************************
#* Load the data
path <- file.path("04_Datasets", "soft-drinks.dat")
df <- read.table(path, header = TRUE)

?process.capability()

# as before we have to exclude datapoint 12
ind <- c(1:11, 13:25)
qcc.PC <- qcc(df[ind,], type = "xbar", nsigmas = 3, plot = FALSE)

# Specification limits
USL <- 18.1
LSL <- 15.6
process.capability(qcc.PC, spec.limits = c(LSL, USL))

#* If we assume that the process is under control, 
#* i.e. if µ 1 = µ 0 and SL = µ 0 , then
#* 
#* • C_p = 1 implies a reject rate of α · 100% = 0.27%.
#* • C_p < 1 implies a reject rate of more than α · 100% = 0.27%, 
#*    i.e. the process capability is not guaranteed.
#* • C_p > 1 implies a reject rate of less than α · 100% = 0.27%, 
#*    i.e. the process capability is guaranteed.
#*    
#*    C_p is > 1so the process capability is guaranteed with a reject
#*      rate less than 0.27%


#*******************************************************************************
#* Problem 6.4.2 (Sampling Plan with AcceptanceSampling, Beispiel 22-2)
#* In acceptance sampling lots with N = 11 000 bolts are tested. Lots with less 
#* than 1% of bad bolts are rejected in 10% of all cases, though they would 
#* have to be accepted as ﬁt for use. On the other hand, lots with more than 5% 
#* of bad bolts are accepted at most in 5% of all cases, though their quality 
#* does not meet the terms of delivery. Find the parameters of the acceptance 
#* sampling plan, i.e. the sample size n and the acceptance number c.
#*******************************************************************************
# install.packages("AcceptanceSampling")
require(AcceptanceSampling)

#   The producer risk point
#       producer risk
alpha <- 0.1
#       probability of acceptance
p.alpha <- 0.01

#   The consumer risk point
#       consumer risk
beta <- 0.05
#       probability of rejection
p.beta <- 0.05

#   Utility function for finding sampling plans
find.plan(PRP=c(p.alpha, 1-alpha), CRP=c(p.beta, beta), type="hypergeom", N=11000)
