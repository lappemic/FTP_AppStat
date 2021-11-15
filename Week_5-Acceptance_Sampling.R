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

