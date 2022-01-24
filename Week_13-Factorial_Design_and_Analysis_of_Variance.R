################################################################################
# AppStat Exercises from week 13
################################################################################
# Exercises from the script
#*------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#   load R-function to do residual analysis (written by A. Ruckstuhl)
source(file.path("02_R-Scripts", "RFn_Plot-lmSim.R"))

#*******************************************************************************
#* Problem 13.5.1 (Compression of Single-Wall Containers, cf. [9], Example 2.14). 
#* The article Compression of Single-Wall Corrugated Shipping Containers Using 
#* Fixed and Floating Test Platens in the Journal of Testing and Evaluation, 
#* 1992, p. 318-320, describes an experiment in which several diﬀerent types of 
#* boxes were compared with respect to compression strength.
#* 
#*      *Data Table in Script*
#*      
#*******************************************************************************
#* a. Enter the data yourself in R and represent it with notched box-plots. 
#*    You need to deﬁne a factor (Box) for the type of box.
#* b. Is there a diﬀerence between diﬀerent types of boxes? Perform a statistical 
#*    hypothesis test on the 5% level.
#*------------------------------------------------------------------------------
#* Enter matrix
data <- data.frame(Strength=c(655.5, 788.3, 734.3, 721.4, 679.1, 699.4,
                              789.2, 772.5, 786.9, 686.1, 732.1, 774.8,
                              737.1, 639.0, 696.3, 671.7, 717.2, 727.1,
                              535.1, 628.7, 542.4, 559.0, 586.9, 520.0),
                   Box=paste0("B", rep(1:4, rep(6,4))))
data
str(data)


#-------------------------------------------------------------------------------
#   (a) Notched Box-Plot
#-------------------------------------------------------------------------------
boxplot(Strength ~ Box, data, notch=TRUE, col="gray",
        xlab="Type of Box",
        ylab="Compression Strength [lb]",
        main="Compression Strength versus Type of Box")
grid()
abline(h=0)

#   REMARKS:
#   1. There are clear differences in the median of the four samples.
#   2. Especially the sample of type 4 is significantly lower than the other three.
#   3. The variance of all samples is almost the same (the height of the box is similar).
#      Sample for type 2 is left skew and sample for type 4 is right-skewed.


#-------------------------------------------------------------------------------
#   (b) ANOVA
#-------------------------------------------------------------------------------
mod <- aov(Strength ~ Box, data)
summary(mod)

#   REMARKS: We test the null hypothesis that the type of box has no influence
#
#                   mu1 = mu2 = mu3 = mu4
#            or
#                   tau2 = tau3 = tau4 = 0 (tau0 is anyway zero, because of the constraint)
#
#            The alternative is at least one mu1 is different from another one.
#            The P-value is 5.53e-07 < 0.05 and therefore we reject the null hypothesis.
#            This means that the Typ 4 is definetely significantly different to Type 2.
#            About the other pairewise comparisons we can not say anything.

#   Residual analysis (to be able to use plot.lmSim we need to fit the model with lm())
mod.lm <- lm(Strength ~ Box, data)
summary(mod.lm)

#   REMARK: The P-value of the F-test is indeed the same!

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

#   REMARK: Nice model!


#--------------------------------------------------------------------------------------------------------------------------------------------------


path <- file.path("04_Datasets", ".dat")
data <- read.table(path, header=TRUE)
str(data)
summary(data)
