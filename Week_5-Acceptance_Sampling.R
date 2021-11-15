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

source("Functions-summary-AppStat.R")
operating_characteristic_function()

