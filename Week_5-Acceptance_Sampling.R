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


source("Functions-summary-AppStat.R")
operating_characteristic_function()

