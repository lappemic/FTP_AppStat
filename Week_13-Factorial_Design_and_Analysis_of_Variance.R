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


#*******************************************************************************
#* Problem 13.5.2 (Energy Consumption of Dehumidiﬁer, cf. [32], DoE2, Problem 3)
#* A consumer protection organisation compares the annual energy consumption of 
#* ﬁve diﬀerent brands of dehumidiﬁers. Because energy consumption depends on the 
#* current humidity, each brand has been tested at four diﬀerent humidity levels 
#* ranging from moderate to high humidity. For each brand, therefore, four devices 
#* were randomly assigned to humidity levels. The mean energy consumption resulting 
#* from the experiment [in kWh] is recorded in the following table:
#* 
#*                              *Table in script*
#*                              
#*******************************************************************************
#* a. Enter the data yourself in R. You need to deﬁne two factors Brand and Humidity.
#* b. Is there a diﬀerence in energy consumption between the ﬁve diﬀerent brands? 
#*    Perform a statistical hypothesis test on the 5% level.
#* c. Is there a diﬀerence in the diﬀerent humidity levels? Perform a statistical 
#*    hypothesis test on the 1% level. Do your ﬁndings support the idea to use the 
#*    block factor Humidity?
#* d. Perform a residual analysis.
#* e. Check the additivity of the model with an interaction plot.
#*------------------------------------------------------------------------------
#* Creating data frame
data <- data.frame(Energy  = c(685,792,838,875,
                               722,806,893,953,
                               733,802,880,941,
                               811,888,952,1005,
                               828,920,978,1023),
                   Brand    = paste0("B",rep(1:5,rep(4,5))),
                   Humidity = paste0("H",rep(1:4,5)))
data$Brand <- as.factor(data$Brand)
data$Humidity <- as.factor(data$Humidity)
str(data)
data##    'data.frame':   20 obs. of  3 variables:
##     $ Energy  : num  685 792 838 875 722 806 893 953 733 802 ...
##     $ Brand   : Factor w/ 5 levels "B1","B2","B3",..: 1 1 1 1 2 2 2 2 3 3 ...
##     $ Humidity: Factor w/ 4 levels "H1","H2","H3",..: 1 2 3 4 1 2 3 4 1 2 ...


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (b,c) ANOVA
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod <- aov(Energy ~ Brand + Humidity, data)
summary(mod)


#   REMARKS: The P-value of the factor "Brand" is 5.42e-09 < 0.05 and therefore we reject the null hypothesis.
#            This means that there significant differences in energy consumation between the different brands
#            The P-value of the block factor "Humidity" is 2.36e-11 < 0.01 and therefore we reject the null hypothesis.
#            We therefore conclude that it was important to use "Humidity" as a block factor.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (d) Residual Analysis
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Residual analysis (to be able to use plot.lmSim we need to fit the model with lm())
mod.lm <- lm(Energy ~ Brand + Humidity, data)
summary(mod.lm)

anova(mod.lm)

#   REMARK: This is the same table as in (b) and (c).

#   diagnostic tools
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
#   (e) Interaction plot
#--------------------------------------------------------------------------------------------------------------------------------------------------
op <- par(mfrow=c(1,2))
interaction.plot(x.factor=data$Brand, trace.factor=data$Humidity, response=data$Energy, fun=mean, ylim=c(600,1100),
                 xlab="Brand", trace.label="Humidity", ylab="Mean of Energy Consumation",
                 main="Interaction Plot")
abline(v=data$Humidity, lty=3, col="gray")
text(data$Brand, data$Energy, label=data$Humidity, pos=3)

interaction.plot(x.factor=data$Humidity, trace.factor=data$Brand, response=data$Energy, fun=mean, ylim=c(600,1100),
                 xlab="Humidity", trace.label="Brand", ylab="Mean of Energy Consumation",
                 main="Interaction Plot")
abline(v=data$Brand, lty=3, col="gray")
text(data$Humidity, data$Energy, label=data$Brand, pos=3)
par(op)

#   REMARKS: The polygonal lines are more or less parallel
#            => Additive model is appropriate.
#            It can also be seen that energy consumption increases with increasing humidity.
#            Interaction can not be formally tested because we do not have repeated measures, 
#             i.e. we have too few observations.


#*******************************************************************************
#* Problem 13.5.3 (Fuel Consumption of Cars, cf. [10], p. 191)
#* In three US cities (Town) the fuel consumption of ﬁve types of cars was measured 
#* (Car). For each combination, three test drives were performed. The response 
#* variable (KMP4L) is the distance [km] the car was able to drive with 4 liters 
#* fuel. The data is given in the data frame car-fuel.dat.
#*******************************************************************************
#* a. Display the variables Car and KMP4L graphically.
#* b. Analyse the response variable KMP4L with a two factor analysis of variance. 
#*    Use the full model with interaction. Are the interactions necessary at the 
#*    a 5% level?
#* c. Perform a residual analysis.
#* d. Represent the means with an interaction plot. How can the signiﬁcant 
#*    interaction be explained?
#* e. Perform a one-way analysis of variance for each city individually.
#* f. Repeat subtask b. without the values of San Francisco.
#*------------------------------------------------------------------------------
#   read data
file <- file.path("04_Datasets", "car-fuel.dat")
data <- read.table(file, header=TRUE)
data
str(data)
data$Car <- as.factor(data$Car)
data$Town  <- as.factor(data$Town)
str(data)

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (a) Graphic
#--------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(KMP4L ~ Car, data, ylim=c(25,50), xlab="Car", ylab="KMP4L", main="KMP4L versus Car and Town")
grid()
points(KMP4L ~ Car, data, pch=20, col=Town)
legend("topleft", levels(data$Town), pch=20, col=1:3)


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (b) ANOVA with Interaction
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod <- aov(KMP4L ~ Car * Town, data)
summary(mod)

#   REMARKS: Both factors "Car" and "Town", as well as the interaction "Car:Town" are significant on the 5% level.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (c) Residual Analysis
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Residual analysis (to be able to use plot.lmSim we need to fit the model with lm())
mod.lm <- lm(KMP4L ~ Car * Town, data)
summary(mod.lm)

anova(mod.lm)

#   REMARK: This is the same table as in (c).

#   diagnostic tools
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

#   REMARK: Nice model! So we can trust the test results.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (d) Interaction plot
#--------------------------------------------------------------------------------------------------------------------------------------------------
op <- par(mfrow=c(1,2))
interaction.plot(x.factor=data$Car, trace.factor=data$Town, response=data$KMP4L, fun=mean, ylim=c(25,50),
                 xlab="Car", trace.label="Town", ylab="Mean of Fuel Consumation",
                 main="Interaction Plot")
abline(v=data$Car, lty=3, col="gray")

interaction.plot(x.factor=data$Town, trace.factor=data$Car, response=data$KMP4L, fun=mean, ylim=c(25,50),
                 xlab="Town", trace.label="Car", ylab="Mean of Fuel Consumation",
                 main="Interaction Plot")
abline(v=data$Town, lty=3, col="gray")
par(op)

#   REMARKS: In the interaction plot, the interactions between Town and Car are clearly seen in the form of 
#             overlapping polygonal lines.
#            Particularly striking is the behavior in the city of San Francisco,
#            which shows a completely different behavior than the other two cities.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (e) ANOVA for each City
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod.LA <- aov(KMP4L ~ Car, data, subset=data$Town=="Los Angeles")
summary(mod.LA)

mod.SF <- aov(KMP4L ~ Car, data, subset=data$Town=="San Francisco")
summary(mod.SF)

mod.Po <- aov(KMP4L ~ Car, data, subset=data$Town=="Portland")
summary(mod.Po)

#   R allows to do the above in one line! Cool!
by(data, data$Town, function(x){ summary(aov(KMP4L ~ Car, data=x))} )

#   REMARK: In every town the factor "Car" is highly significant.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (f) ANOVA without San Francisco
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod.red1 <- aov(KMP4L ~ Car * Town, data, subset=data$Town!="San Francisco")
summary(mod.red1)

#   REMARK: Now, neither the factor "Town" nor the interaction "Car:Town" are significant.
#           This is not surprising if we observe the interaction plot correctly.
#           => F-Test to compare two models.

mod.red2 <- aov(KMP4L ~ Car, data, subset=data$Town!="San Francisco")
summary(mod.red2)

anova(mod.red1, mod.red2)

#   REMARK: The P-value is 0.2379 > 0.05.
#           There are no hints that the more difficult model is better than the model with an interaction.
#           =>  According to the principle of simplicity, we opt for the simpler model.