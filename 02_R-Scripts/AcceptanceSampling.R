#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Part I: SPC
#   "AcceptanceSampling.R"
#   Author: Marcel Steiner-Curtis
#   Date: 15.10.2008    sml
#         07.05.2018    English version
#--------------------------------------------------------------------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   load package
require(AcceptanceSampling)


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   OC2c    Operation Characteristics of an Acceptance Sampling Plan
##    Description:  The preferred way of creating new objects from the family of "OC2c" classes.
##    Usage:        OC2c(n,c,r=if (length(c)==1) c+1 else NULL, type=c("binomial","hypergeom", "poisson"), Arguments
##                  n       A vector of length k giving the sample size at each of the k stages of sampling,
##                          e.g. for double sampling k=2.
##                  c       A vector of length k giving the cumulative acceptance numbers at each of the k
##                          stages of sampling.
##                  r       A vector of length k giving the cumulative rejection numbers at each of the k
##                          stages of sampling.
##                  type    The possible types relate to the distribution on which the plans are based on,
##                          namely, binomial, hypergeom, and poisson.
##                  ...         Additional parameters passed to the class generating function for each type. See


#  a standard binomial sampling plan
OC.hypergeom <- OC2c(n=40,c=20, N=100, type="hypergeom")
summary(OC.hypergeom)
##    Acceptance Sampling Plan (hypergeom with N=100)
##
##                   Sample 1
##    Sample size(s)       40
##    Acc. Number(s)       20
##    Rej. Number(s)       21

#   plot the OC curve
plot(OC.hypergeom, type="l")



#--------------------------------------------------------------------------------------------------------------------------------------------------
#   find.plan   Utility function for finding sampling plans.

##    Description:  Find the sampling plan with smallest sample size (single sample only) such that specified Producer
##                  Risk Point (PRP) and Consumer Risk Point (CRP) are both met.
##    Arguments:    PRP     The Producer Risk Point in the form of a two element numeric vector of the
##                          form c(pd, pa). The first element, pd, specifies the quality level at which to
##                          evaluate the plan. The second element, pa, indicates the minimum probability
##                          of acceptance to be achieved by the plan.
##                  CRP     The Consumer Risk Point in the form of a two element numeric vector of the
##                          form c(pd, pa). The first element, pd, specifies the quality level at which to
##                          evaluate the plan. The second element, pa, indicates the maximum probability
##                          of acceptance to be achieved by the plan.
##                  type    The distribution which the sampling plan is based on. Possible values are binomial,
##                          hypergeom, poisson and normal.
##                  N       The size of the population from which samples are drawn. Only applicable for
##                          type="hypergeom".
##                  s.type  The type of the standard deviation. A value of known results in a sampling plan
##                          based on the population standard deviation, while a value of unknown results
##                          in the use of the sample standard deviation.
##    Usage:        find.plan(PRP, CRP, type="binomial")
##                  find.plan(PRP, CRP, type="hypergeom", N)
##                  find.plan(PRP, CRP, type="normal", s.type="unknown")

#   the producer risk point
alpha <- 0.2
#   minimum probability of acceptance to be achieved
p.alpha <- 0.05

#   the consumer risk point
beta <- 0.1
#   maximum probability of acceptance
p.beta <- 0.1

#   utility function for finding sampling plans
find.plan(PRP=c(p.alpha, 1-alpha), CRP=c(p.beta, beta), type="hypergeom", N=100)
##    $n
##    [1] 64
##
##    $c
##    [1] 4
##
##    $r
##    [1] 5



#--------------------------------------------------------------------------------------------------------------------------------------------------
