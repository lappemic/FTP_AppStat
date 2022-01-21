################################################################################
# AppStat Exercises from week 11
################################################################################
# Exercises from the script
#*------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#   load R-function to do residual analysis (written by A. Ruckstuhl)
source(file.path("02_R-Scripts", "RFn_Plot-lmSim.R"))

#   load package
require(car)

#*******************************************************************************
#* Problem 11.4.1 (Experiment on a Dairy Product, cf. [32], Reg5, Problem 1). 
#* We come back to Ex. 9.4.4.
#* a. Fit the model
#* 
#* log(O2UP) = β 0 + β 1 · BOD + β 2 · TKN + β 3 · TS + β 4 · TVS + β 5 · COD + ε.
#* 
#*      Perform a variable selection with a backward elimination using AIC.
#* b. Fit the trivial model
#* 
#*                      log(O2UP) = β 0 + ε.
#*      
#*      Perform a variable selection with a forward selection using AIC.
#* c. Fit the model
#* 
#*              log(O2UP) = β 0 + β 1 · BOD + β 3 · TS + ε.
#*      
#*      Perform a variable selection with a search in both directions using AIC.
#* d. Compare your results. Which model(s) would you like to investigate further?
#* e. Check for collinearity in the data.
#* f. Repeat the above without the observations with index i = 2, 3 and 17. 
#*      How sensitive is variable selection with respect to these observations?
#*******************************************************************************
path <- file.path("04_Datasets", "oxygen.dat")
data <- read.table(path, header=TRUE)
str(data)
summary(data)

#   define new log-transformed variables
data$O2UP.log    <- log10(data$O2UP)


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (a) Fit Full Model with all Data, Backward Elimination
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod.full.all <- lm(O2UP.log ~ BOD + TKN + TS + TVS + COD, data)
summary(mod.full.all)

mod.backward <- step(mod.full.all,
                     scope=list(upper= ~ BOD + TKN + TS + TVS + COD,
                                lower= ~ 1),
                     direction="backward")

summary(mod.backward)

#   REMARK: The best model with respect to AIC is log(O2UP) ~ TS + COD and AIC=-52.28.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (b) Fit Trivial Model with all Data, Forward Selection
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod.small.all <- lm(O2UP.log ~ 1, data)
summary(mod.small.all)

mod.forward <- step(mod.small.all,
                    scope=list(upper= ~ BOD + TKN + TS + TVS + COD,
                               lower= ~ 1),
                    direction="forward")

summary(mod.forward)

#   REMARK: The best model with respect to AIC is log(O2UP) ~ TS + COD and AIC=-52.28.
#           This is the same model which was found in (a) with a backward elimination.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (c) Fit Model with all Data, Forward and Backward Selection
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod.all <- lm(O2UP.log ~ BOD + TS, data)
summary(mod.all)

mod.both <- step(mod.all,
                 scope=list(upper= ~ BOD + TKN + TS + TVS + COD,
                            lower= ~ 1),
                 direction="both")

summary(mod.both)

#   REMARK: The best model with respect to AIC is log(O2UP) ~ TS + COD and AIC=-52.28.
#           This is the same model which was found in (a) with a backward elimination and (b) with forward selection.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (d) Which Model?

#   REMARK: With all three procedure we found the same model, therefore we have no choice.
#           => Check for collinearity.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (e) Check for Collinearity
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   modified pairs-plot functions (source help(pairs))
#   put (absolute) correlations on the lower panels (font size is proportional to correlations)
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
        usr <- par("usr")
        on.exit(par(usr))
        par(usr=c(0, 1, 0, 1))
        r <- abs(cor(x, y, use="pairwise.complete.obs"))
        txt <- format(c(r, 0.123456789), digits=digits)[1]
        txt <- paste(prefix, txt, sep="")
        if(missing(cex.cor)){ cex.cor <- 0.8/strwidth(txt) }
        text(0.5, 0.5, txt, cex=cex.cor*r) }
#   put histograms on the diagonal
panel.hist <- function(x, ...){
        usr <- par("usr")
        on.exit(par(usr))
        par(usr=c(usr[1:2], 0, 1.5))
        h <- hist(x, plot=FALSE)
        breaks <- h$breaks
        nB <- length(breaks)
        y <- h$counts
        y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col="gray") }

#   pairs-plot of the data
pairs(data[,c("O2UP.log", "BOD", "TKN", "TS", "TVS", "COD")],
      upper.panel=panel.smooth,
      lower.panel=panel.cor,
      diag.panel=panel.hist,
      pch=20)
which.min(data$TVS)

#   REMARKS
#   (1) We can see an outlier in TVS with index i=17.
#   (2) The variable BOD is correlated with TS, TVS and COD.

#   variance inflation factor
vif(mod.full.all)

#   REMARK: Since VIF(BOD)=7.134819 > 5 we conclude that BOD is too well explained by the other variables.
#           On the other hand BOD is eliminated in all variable selection processes.

vif(mod.forward)

#   REMARKS: From a VIF-standpoint everything is o.k.
#            Is this model also reasonable from a technical point of view?
#            Residual analysis?

#   diagnostic tools
op <- par(mfcol=c(2,4))
#   Tukey-Anscombe plot
plot(mod.forward, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.forward, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.forward, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.forward, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.forward, which=2, pch=20)
grid()
plot.lmSim(mod.forward, which=2, SEED=1)
grid()

#   residuals against leverages
plot(mod.forward, which=5, pch=20)
grid()
abline(h=0, lty=3)
par(op)

#   REMARK: Observations with index i=1, 17 and 20 are critical.
#           => Repeat the above without these observations.
#           => Did it separately. The same models result!


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (f) Repeat the Above Without the Observations with i=2, 3 and 17
#--------------------------------------------------------------------------------------------------------------------------------------------------
data.red <- data[-c(2,3,17),]
str(data.red)

#   Fit full model with reduced data, backward elimination
mod.full.red <- lm(O2UP.log ~ BOD + TKN + TS + TVS + COD, data=data.red)
mod.red.backward <- step(mod.full.red,
                         scope=list(upper= ~ BOD + TKN + TS + TVS + COD,
                                    lower= ~ 1),
                         direction="backward")
summary(mod.red.backward)

#   REMARK: The best model with respect to AIC is log(O2UP) ~ COD + TVS and AIC=-42.34.
#           This is a different model than the one we found in (a).

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Fit trivial model with red data, forward selection
mod.small.red <- lm(O2UP.log ~ 1, data=data.red)
mod.red.forward <- step(mod.small.red,
                        scope=list(upper= ~ BOD + TKN + TS + TVS + COD,
                                   lower= ~ 1),
                        direction="forward")

summary(mod.red.forward)

#   REMARK: The best model with respect to AIC is log(O2UP) ~ COD + TVS and AIC=-42.34.
#           This is the same model which was found with a backward elimination.

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Fit model with red data, forward and backward selection
mod.red <- lm(O2UP.log ~ BOD + TS, data=data.red)
mod.red.both <- step(mod.red,
                     scope=list(upper= ~ BOD + TKN + TS + TVS + COD,
                                lower= ~ 1),
                     direction="both")

summary(mod.red.both)

#   REMARK: The best model with respect to AIC is log(O2UP) ~ COD + TVS and AIC=-42.34.
#           This is the same model which was found with a backward elimination and with forward selection.

#   FINAL REMARKS:
#   1. In the case with the full data set we get with backward elimination, forward selection and both
#      the same model
#                       O2UP.log ~ TS + COD.
#
#   2. In the case with the reduced data set we get with backward elimination, forward selection and both
#      the same model
#                       O2UP.log ~ COD + TVS.
#
#   3. We observe that omitting a few random observations can influence variable selection.


#*******************************************************************************
#* Problem 11.4.1 (Experiment on a Dairy Product, cf. [32], Reg5, Problem 1). 
#* We come back to Ex. 9.4.4.
#* a. Fit the model
#* 
#* log(O2UP) = β 0 + β 1 · BOD + β 2 · TKN + β 3 · TS + β 4 · TVS + β 5 · COD + ε.
#* 
#*      Perform a variable selection with a backward elimination using AIC.
#* b. Fit the trivial model
#* 
#*                      log(O2UP) = β 0 + ε.
#*      
#*      Perform a variable selection with a forward selection using AIC.
#* c. Fit the model
#* 
#*              log(O2UP) = β 0 + β 1 · BOD + β 3 · TS + ε.
#*      
#*      Perform a variable selection with a search in both directions using AIC.
#* d. Compare your results. Which model(s) would you like to investigate further?
#* e. Check for collinearity in the data.
#* f. Repeat the above without the observations with index i = 2, 3 and 17. 
#*      How sensitive is variable selection with respect to these observations?
#*******************************************************************************
