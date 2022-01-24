################################################################################
# AppStat Exercises from week 14
################################################################################
# Exercises from the script
#*------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#   load R-function to do residual analysis (written by A. Ruckstuhl)
source(file.path("02_R-Scripts", "RFn_Plot-lmSim.R"))

#   load package
require(daewr)

#*******************************************************************************
#* Problem 14.3.2 (Filling Variability, cf. [23], Problem 14-51)
#* A factorial experiment is used to study the ﬁlling variability of dry soup 
#* mix packages. The factors are
#* • the number of mixing ports through which the vegetable oil was added (A) 
#*    with levels one or two ports,
#* • the temperature surrounding the mixer (B) with levels cooled and ambient,
#* • the mixing time (C) with levels 60 or 80 seconds,
#* • the batch weight (D) with levels 1500 or 2000 lb and
#* • the number of days of delay between mixing and packaging (E) with levels 
#*    one or seven days.
#*    
#*                          *Data Table in Script*
#*      
#*******************************************************************************
#* a. The design generator is E = −ABCD. Describe the design used in the experiment:
#*    • Name of the design?
#*    • Deﬁning relations?
#*    • Alias structure?
#* b. Calculate by hand the estimate for the main eﬀect A as well as for the 
#*    interactions C :D and A:B :E.
#* c. Use a model without interactions. Which eﬀects are statistically signiﬁcant?
#* d. Perform an analysis of residuals. Report your ﬁndings.
#* e. Fit a model with all main eﬀects and all second-order interaction eﬀects. 
#*    Find all signiﬁcant eﬀects and ﬁt a reduced model. Note that for signiﬁcant 
#*    interaction eﬀects, the corresponding main eﬀects are always added in the 
#*    reduced model.
#* f. Report your results of your analysis of the data from the experiment.
#*------------------------------------------------------------------------------
#        #   IMPORTANT REMARK:   If we define the data in R as follows, then we 
#                                 have to set the contrasts,
#        #                       i.e. the constraints to estimate the parameters.
#        op <- options(contrasts=c("contr.sum", "contr.poly"))
#        #                       Only then do the estimated coefficients match the hand calculation,
#        #                       i.e. the R-output times 2 equals the hand calculation.
#        #                       The order of the levels is by the way also important. 
#                                 Changes the sign of the effect.
#        #                       When finished you can restore the contrasts to
#                                 the default values with options(op).
#        #                       See at the end!
#        #   read data
#        data <- data.frame(A=rep(c("one","two"), 8),
#                           B=rep(rep(c("cooled","ambient"), c(2,2)), 4),
#                           C=rep(rep(c(60,80), c(4,4)), 2),
#                           D=rep(c(1500,2000), c(8,8)),
#                           E=c(1,7,7,1,7,1,1,7,7,1,1,7,1,7,7,1),
#                           y=c(1.13,1.25,0.97,1.7,1.47,1.28,1.18,0.98,0.78,1.36,1.85,0.62,1.09,1.1,0.76,2.1))
#        data$A <- factor(data$A, levels=c("one","two"))
#        data$B <- factor(data$B, levels=c("cooled","ambient"))
#        data$C <- factor(data$C, levels=c("60","80"))
#        data$D <- factor(data$D, levels=c("1500","2000"))
#        data$E <- factor(data$E, levels=c("1","7"))
#        str(data)
#        ##    'data.frame':   16 obs. of  6 variables:
#        ##     $ A: Factor w/ 2 levels "one","two": 1 2 1 2 1 2 1 2 1 2 ...
#        ##     $ B: Factor w/ 2 levels "cooled","ambient": 1 1 2 2 1 1 2 2 1 1 ...
#        ##     $ C: Factor w/ 2 levels "60","80": 1 1 1 1 2 2 2 2 1 1 ...
#        ##     $ D: Factor w/ 2 levels "1500","2000": 1 1 1 1 1 1 1 1 2 2 ...
#        ##     $ E: Factor w/ 2 levels "1","7": 1 2 2 1 2 1 1 2 2 1 ...
#        ##     $ y: num  1.13 1.25 0.97 1.7 1.47 1.28 1.18 0.98 0.78 1.36 ...
#        data
#        ##         A       B  C    D E    y
#        ##    1  one  cooled 60 1500 1 1.13
#        ##    2  two  cooled 60 1500 7 1.25
#        ##    3  one ambient 60 1500 7 0.97
#        ##    4  two ambient 60 1500 1 1.70
#        ##    5  one  cooled 80 1500 7 1.47
#        ##    6  two  cooled 80 1500 1 1.28
#        ##    7  one ambient 80 1500 1 1.18
#        ##    8  two ambient 80 1500 7 0.98
#        ##    9  one  cooled 60 2000 7 0.78
#        ##    10 two  cooled 60 2000 1 1.36
#        ##    11 one ambient 60 2000 1 1.85
#        ##    12 two ambient 60 2000 7 0.62
#        ##    13 one  cooled 80 2000 1 1.09
#        ##    14 two  cooled 80 2000 7 1.10
#        ##    15 one ambient 80 2000 7 0.76
#        ##    16 two ambient 80 2000 1 2.10


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   read data
data <- data.frame(A=rep(c(-1,1), 8),
                   B=rep(rep(c(-1,1), c(2,2)), 4),
                   C=rep(rep(c(-1,1), c(4,4)), 2),
                   D=rep(c(-1,1), c(8,8)),
                   E=c(-1,1,1,-1,1,-1,-1,1,1,-1,-1,1,-1,1,1,-1),
                   y=c(1.13,1.25,0.97,1.7,1.47,1.28,1.18,0.98,0.78,1.36,1.85,0.62,1.09,1.1,0.76,2.1))
data
str(data)

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (a) Design
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   REMARKS:
#   * 2^(5-1) fractional factorial design
#   * Multiply E = -ABCD wit E and obtain E^2 = I = -ABCDE. Only one defining relation I = -ABCDE.
#   * Every main effect is aliased with a second- or third-order interaction
#        A=-BCDE    AB=-CDE     BD=-ACE
#        B=-ACDE    AC=-BDE     BE=-ACD
#        C=-ABDE    AD=-BCE     CD=-ABE
#        D=-ABCE    AE=-BCD     CE=-ABD
#        E=-ABCD    BC=-ADE     DE=-ABC


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (b) Estimate Effects (by hand)
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Main effect A
(data$A %*% data$y)/(nrow(data)/2)

#   Interaction effect C:D
((data$C*data$D) %*% data$y)/(nrow(data)/2)

#   Interaction effect A:B:E
((data$A*data$B*data$E) %*% data$y)/(nrow(data)/2)

#   REMARK: In R %*% is the scalar product.
#           The interaction effects C:D and A:B:E are aliased, i.e. CD=-ABE,
#           therefore the absolute value of the estimates of the effects is the same.

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (b) Estimate Effects (with lm)
#--------------------------------------------------------------------------------------------------------------------------------------------------
summary.lm(aov(y ~ (.)^3, data))

#   REMARK: To obtain the values of the hand calculations: Multiply "Estimate" times 2.
#           Main effect A:           2*0.07250 = 0.145
#           Interaction effect C:D:  2*0.03625 = 0.0725


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (c) Model without Interactions
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod <- aov(y ~ A + B + C + D + E, data)
summary(mod)

#   REMARK: Only factor E is statistically significant.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (d) Residual Analysis
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod.lm <- lm(y ~ A + B + C + D + E, data)
summary(mod.lm)

#   REMARK: Same P-values as in (c).

#   diagnostic plots
op <- par(mfcol=c(2,3))
#   Tukey-Anscombe plot
plot(mod.lm, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.lm, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.lm, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.lm, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.lm, which=2, pch=20)
grid()
plot.lmSim(mod.lm, which=2, SEED=1)
grid()
par(op)

#   REMARK: Nice model! All plots within the simulations.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (e) Model with Second-Order Interactions Effects
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod.sec <- aov(y ~ (A + B + C + D + E)^2, data)
summary(mod.sec)

summary.lm(mod.sec)

#   detect significant effects in unreplicated fractional factorials (without intercept)
LGB(coefficients(mod.sec)[-1])
grid()
title(main="Half Normal Plot")
abline(h=0, v=0)
# REMARK: Look at the console output as well!

#   Pareto Chart
effects.abs <- abs(coefficients(mod.sec)[-1])
effects.sort <- sort(effects.abs, decreasing=TRUE)
barplot(effects.sort, main="Pareto Chart", ylim=c(0,0.25), col=c(rep("gray",3),rep("white",12)), las=2)
abline(h=0)

#   REMARK: The main effect E and the interaction effects B:E and D:E are significant.
#           For significant interactions, the corresponding main effects are always added.
#           The reduced model contains therefore the main effects B, D, E and the interaction effects B:E and D:E.

mod.lm.red <- lm(y ~ (B + D)*E, data)
anova(mod.lm.red)

#   diagnostic plots
op <- par(mfcol=c(2,3))
#   Tukey-Anscombe plot
plot(mod.lm.red, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.lm.red, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.lm.red, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.lm.red, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.lm.red, which=2, pch=20)
grid()
plot.lmSim(mod.lm.red, which=2, SEED=1)
grid()
par(op)

#   REMARK: Nice model! All plots within the simulations.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (f) Conclusions
#--------------------------------------------------------------------------------------------------------------------------------------------------
summary(mod.lm.red)

#   REMARKS: The main factor E (the number of days of delay between mixing and packaging) is significant.
#            The interactions B:E and D:E are significant.
#            where B: temperature surrounding the mixer
#                  D: the batch weight
#            => the minimal variance is obtained if all three main effects B, D and E are on the high level.

Ind.min <- which.min(fitted(mod.lm.red))
data[Ind.min, c("B", "D", "E")]

fitted(mod.lm.red)[Ind.min]


#--------------------------------------------------------------------------------------------------------------------------------------------------
#        #   IMPORTANT REMARK:   When finished you can restore the contrasts to the default values.
#        options(op)
options(op)
#--------------------------------------------------------------------------------------------------------------------------------------------------
