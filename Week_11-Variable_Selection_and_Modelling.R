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
#* Problem 11.4.2 (Sniﬀer Data, cf. [43], Chap. 8.3). 
#* When gasoline is pumped into a tank, hydrocarbon vapors are forced out of the 
#* tank and into the atmosphere. To reduce this signiﬁcant source of air pollution, 
#* devices are installed to capture the vapor. In testing these vapor recovery 
#* systems, a sniﬀer measures the amount recovered. To estimate the eﬃciency of the
#* system, some method of estimating the total amount given oﬀ must be used. 
#* To this end, a laboratory experiment was conducted in which the amount of vapor 
#* given oﬀ was measured under controlled conditions. Four predictors are relevant 
#* for modelling:
#* 
#*      TankTemp - initial tank temperature [in ◦ F]
#*      GasTemp - temperature of the dispensed gasoline [in ◦ F] 
#*      TankPres - initial vapor pressure in the tank [in psi] 
#*      GasPres - vapor pressure of the dispensed gasoline [in psi]
#*      
#* The response is the hydrocarbons (Hydrocarbons) emitted in grams. The data of 
#* 32 measurements is given in the data frame sniffer.dat.
#*******************************************************************************
#* a. Draw a pairs-plot of the complete data set. Report all your ﬁndings which 
#*    seem to be important with respect to regression modelling of the hydrocarbons.
#* b. Check for collinearity in the data.
#* c. Fit the model 
#* 
#*      Hydrocarbons = β 0 + β 1 · TankTemp + β 2 · GasTemp
#*                               + β 3 · TankPres + β 4 · GasPres + ε.
#*      
#*      Report all your ﬁndings studying the summary-lm-output.
#* d. Perform a variable selection using AIC.
#* e. Which model(s) would you like to investigate further?
#*------------------------------------------------------------------------------
#   read data
file <- file.path("04_Datasets", "sniffer.dat")
data <- read.table(file, header=TRUE)
str(data)

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (a) Pairs Plot
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
pairs(data,
      upper.panel=panel.smooth,
      lower.panel=panel.cor,
      diag.panel=panel.hist,
      pch=20)

#   REMARKS
#   (1) We can see that all explanatory variables are strongly correlated.
#   (2) The variable TankTemp has three levels.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (b) Check for Collinearity in the Data.
#--------------------------------------------------------------------------------------------------------------------------------------------------
# fit Model
mod.full <- lm(Hydrocarbons ~., data)
#   variance inflation factor
vif(mod.full)

#   REMARK: We observe an extreme collinearity.
#           Summary-lm-output has to be read with great care.
#           => variable selection


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (c) Fit Full Model
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod.full <- lm(Hydrocarbons ~., data)
summary(mod.full)

#   REMARKS: The F-test says that the explanatory variables have an influence on the response.
#            But, because of the collinearity we do not trust the P-values of the individual coefficients

mod.both <- step(mod.full, direction="both")

summary(mod.both)

#   variance inflation factor
vif(mod.both)

#   REMARK: We still observe an extreme collinearity.
#           Summary-lm-output has to be read with great care.
#           => Variable selection did not help
#           => Linear combinations of explanatory variables.

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (e) Which model(s) would you like to investigate further?
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   REMARK: None, because of the collinearity.
#           => Try linear combinations of explanatory variables.
#              It is important the create new variables which have a physical interpretaion.

#   mean pressure
data$PresMean  <- (data$GasPres + data$TankPres)/2
#   delta pressure
data$PresDelta <- data$GasPres - data$TankPres

#   pairs-plot of the transformed data
pairs(data[,c("Hydrocarbons", "GasTemp", "PresMean", "PresDelta")],
      upper.panel=panel.smooth,
      lower.panel=panel.cor,
      diag.panel=panel.hist,
      pch=20)

#   fit model
mod.trans.full <- lm(Hydrocarbons ~ GasTemp+PresMean+PresDelta, data)
summary(mod.trans.full)

#   diagnostic tools
op <- par(mfcol=c(2,4))
#   Tukey-Anscombe plot
plot(mod.trans.full, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.trans.full, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.trans.full, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.trans.full, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.trans.full, which=2, pch=20)
grid()
plot.lmSim(mod.trans.full, which=2, SEED=1)
grid()

#   residuals against leverages
plot(mod.trans.full, which=5, pch=20)
grid()
abline(h=0, lty=3)
par(op)

#   REMARK: Nice model.

#*******************************************************************************
#* Problem 11.4.3 (Thrust of Jet-Turbine Engines, cf. [23], Problem 12-108). 
#* Thrust of jet-turbine engines y depends on the following variables:
#* 
#*              x1 = primary speed of rotation 
#*              x2 = secondary speed of rotation 
#*              x3 = fuel ﬂow rate 
#*              x4 = pressure 
#*              x5 = exhaust temperature 
#*              x6 = ambient temperature at time of test 
#* 
#* Since the exact relationship is unknown, it must be modeled empirically based 
#* on the data in the data set jet-engines.dat. Develop models which can then be 
#* discussed with the experts.
#*******************************************************************************
#   read data
file <- file.path("04_Datasets", "jet-engines.dat")
data <- read.table(file, header=TRUE)
str(data)

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   1. Step: First-Aid Transformations
#--------------------------------------------------------------------------------------------------------------------------------------------------
data$x1.log <- log10(data$x1)
data$x2.log <- log10(data$x2)
data$x3.log <- log10(data$x3)
data$x4.log <- log10(data$x4)
data$y.log  <- log10(data$y)

#   REMARK: The logarithmic transformations will bring little since the range of the transformed variables is small.
#           We do not transform both temperatures, i.e. x5 and x6.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   2. Step: Fit Regression Model
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod.1 <- lm(y.log ~ x1.log + x2.log + x3.log + x4.log + x5 + x6, data)
summary(mod.1)

#   diagnostic tools
op <- par(mfcol=c(2,4))
#   Tukey-Anscombe plot
plot(mod.1, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.1, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.1, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.1, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.1, which=2, pch=20)
grid()
plot.lmSim(mod.1, which=2, SEED=1)
grid()

#   residuals against leverages
plot(mod.1, which=5, pch=20)
grid()
abline(h=0, lty=3)
par(op)

#   REMARK: Observation with index i=20 is a leverage point.
#           => robust MM-regression


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   3.  Fit Robust MM-Regression
#--------------------------------------------------------------------------------------------------------------------------------------------------
#set.seed(6)
set.seed(1)
mod.rob <- lmrob(y.log ~ x1.log + x2.log + x3.log + x4.log + x5 + x6, data, control=lmrob.control(max.it=100))
summary(mod.rob)

#   diagnostic tools
op <- par(mfcol=c(2,4))
#   Tukey-Anscombe plot
plot(mod.rob, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.rob, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.rob, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.rob, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.rob, which=2, pch=20)
grid()
plot.lmSim(mod.rob, which=2, SEED=1)
grid()

#   residuals against leverages
plot(mod.rob, which=5, pch=20)
grid()
abline(h=0, lty=3)
par(op)

#   REMARK: Unfortunately, in this case, no clear answer is available from the robust MM-estimation.
#           Observation with index i=20 is declared as an outlier with some starting values (i.e. set.seed(6))
#           and not with others (i.e. set.seed(1)).
#           This is, of course, not very satisfying!
#           => Repeat lm without observation i=20.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   4.  Step: Fit Regression Model without Observation i=20 and Variable Selection
#--------------------------------------------------------------------------------------------------------------------------------------------------
mod.2 <- lm(y.log ~ x1.log + x2.log + x3.log + x4.log + x5 + x6, data, subset=-20)
mod.2.both <- step(mod.2, direction="both")

summary(mod.2.both)

#   REMARK: Automatic variable selection leeds to the model
#
#               log10(y) = beta0 + beta1*log10(x1) + beta3*log10(x2) + beta6*x6 + epsilon


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Check for collinearity in the data of the optimal model with the variance inflation factor
vif(mod.2.both)

#   REMARK: We observe an extreme collinearity.
#           Summary-lm-output has to be read with great care.
#           => pairs plot

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
pairs(data[, c("x1.log", "x2.log", "x6")],
      upper.panel=panel.smooth,
      lower.panel=panel.cor,
      diag.panel=panel.hist,
      pch=20)

#   REMARK: In the selected optimal model the variables x1.log and x2.log are extremely correlated.
#           x1 and x2 are rotational velocities, therefore we can transform them into a mean rotational velocity and
#           a delta of the rotational velocities.

#   mean of the logarithm of the rotational velocity is a product
data$vProd.log  <- (data$x1.log + data$x2.log)/2
#   delta of the logarithm of the rotational velocity is a quotient
data$vQuot.log <- data$x1.log - data$x2.log

#   pairs-plot of the new and old data. We used the original scale and not the log-transformed variables
pairs(data[, c("vProd.log", "vQuot.log", "x1.log", "x2.log")],
      upper.panel=panel.smooth,
      lower.panel=panel.cor,
      diag.panel=panel.hist,
      pch=20)

#   REMARK: That did NOT help! Why?
range(data$x1.log)
range(data$x2.log)

#   ANSWER:  The range of the variable x2.log clearly dominates the variable x1.log.
#            => We need a new idea "out of the box"!


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   5.  Step: Get rid of the collinearity between x1.log and x2.log
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Replace the variable x2.log with the residuals of lm(x2.log ~ x1.log, data),
#
#       x2.log.res = log10(x2) - (alpha0+alpha1*log10(x1))
#
#   QUESTION: Why?
#   ANSWER: See below!
mod.res21 <- lm(x2.log ~ x1.log, data)
data$x2.log.res <- resid(mod.res21)
summary(mod.res21)

#   REMARK: We observe alpha0=3.193558 and alpha1=0.336835.

#   pairs-plot of the new and old data. We use the log-transformed variables.
pairs(data[, c("x1.log", "x2.log.res", "x6")],
      upper.panel=panel.smooth,
      lower.panel=panel.cor,
      diag.panel=panel.hist,
      pch=20)

#   REMARK: That looks much better!
#           => Repeat the above with x2.log.res instead of x2.log and without the observation with index i=20.

mod.3 <- lm(y.log ~ x1.log + x2.log.res + x3.log + x4.log + x5 + x6, data, subset=-20)
mod.3.both <- step(mod.3, direction="both")
summary(mod.3.both)

#   diagnostic tools
op <- par(mfcol=c(2,4))
#   Tukey-Anscombe plot
plot(mod.3.both, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.3.both, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.3.both, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.3.both, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.3.both, which=2, pch=20)
grid()
plot.lmSim(mod.3.both, which=2, SEED=1)
grid()

#   residuals against leverages
plot(mod.3.both, which=5, pch=20)
grid()
abline(h=0, lty=3)

#   fitted versus measured
plot(fitted(mod.3.both), data.PCA$y.log, pch=20, xlab="Fitted values", ylab="Measured values",
     main="mod.3.both")
grid()
abline(0,1, col="red")
par(op)

#   REMARK: Nice new model
#
#                   log10(y) = gamma0 + gamma1*log10(x1) + gamma2*x2.log.res + gamma6*x6 + epsilon
#

#   Collinearity
vif(mod.3.both)

#   REMARK: The collinearity is gone.

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   The fitted values of the 2nd and 3rd models are identical. Why?
range(fitted(mod.2.both)-fitted(mod.3.both))
##  -1.332268e-15  1.065814e-14
par(mfrow=c(1,1))
plot(fitted(mod.2.both), fitted(mod.3.both), pch=20)
grid()
abline(a=0, b=1)

#   ANSWER: The new variable is
#
#                   x2.log.res = log10(x2) - (alpha0+alpha1*log10(x1)),
#
#           therefore the third model is
#
#                   log10(y) = gamma0 + gamma1*log10(x1) + gamma2*x2.log.res + gamma6*x6 + epsilon
#                            = gamma0 + gamma1*log10(x1) + gamma2*(log10(x2)-alpha0-alpha1*log10(x1)) + gamma6*x6 + epsilon
#                            = beta0 + beta1*log10(x1) + beta2*log10(x2) + beta6*x6 + epsilon,
#
#           which is nothing else than the 2nd model with
#
#                   beta0 = gamma0 - gamma2*alpha0 = 0.787021 - 1.133755*3.193558 = -2.833691
#                   beta1 = gamma1 - gamma2*alpha1 = 0.903672 - 1.133755*0.336835 =  0.5217836
#                   beta2 = gamma2 =  1.133755
#                   beta6 = gamma6 = -0.001422


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Summary and Questions which can be discussed with the experts
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   - 2nd model: Is there a physical explanation of the high collinearity?
#
#                   log10(y) = beta0 + beta1*log10(x1) + beta2*log10(x2) + beta6*x6 + epsilon
#
#   - 3rd model: Is there an interpretation of x2.log.res?
#
#                   log10(y) = gamma0 + gamma1*log10(x1) + gamma2*x2.log.res + gamma6*x6 + epsilon
#
#   - Observation with index i=20 could be an outlier.
#   - Both models are only valid in the range explanatory variables away from observation with index i=20.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Addon: Principal Component Regression (https://en.wikipedia.org/wiki/Principal_component_regression)
#--------------------------------------------------------------------------------------------------------------------------------------------------
X <- data[, c("x1.log", "x2.log", "x3.log", "x4.log")]
#   correlation matrix without the obvious outlier
EV <- eigen(cor(X[-20,]))
R <- EV$vectors
EV$values

#   REMARK: The first principal component explains most of the variance.
#           Therefore the first principal compente will be enough.

Z <- scale(as.matrix(X[-20,])) %*% R
colnames(Z) <- c("z1", "z2", "z3", "z4")
cor(Z)

data.PCA <- cbind(Z, data[-20,c("x5","x6", "y.log")])
pairs(data.PCA,
      upper.panel=panel.smooth,
      lower.panel=panel.cor,
      diag.panel=panel.hist,
      pch=20)

#   linear model
summary(   lm(y.log ~ z1 + z2 + z3 + z4 + x5 + x6, data.PCA))
summary(   lm(y.log ~ z1 + z2 + z3      + x5 + x6, data.PCA))
summary(   lm(y.log ~ z1 + z2 +         + x5 + x6, data.PCA))
mod.PCA <- lm(y.log ~ z1 +              + x5 + x6, data.PCA)
summary(mod.PCA)

#   diagnostic tools
op <- par(mfcol=c(2,4))
#   Tukey-Anscombe plot
plot(mod.PCA, which=1, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.PCA, which=1, SEED=1)
grid()
abline(h=0, lty=3)

#   scale-location plot
plot(mod.PCA, which=3, pch=20)
grid()
abline(h=0, lty=3)
plot.lmSim(mod.PCA, which=3, SEED=1)
grid()
abline(h=0, lty=3)

#   q-q plot
plot(mod.PCA, which=2, pch=20)
grid()
plot.lmSim(mod.PCA, which=2, SEED=1)
grid()

#   residuals against leverages
plot(mod.PCA, which=5, pch=20)
grid()
abline(h=0, lty=3)

#   fitted versus measured
plot(fitted(mod.PCA), data.PCA$y.log, pch=20, xlab="Fitted values", ylab="Measured values",
     main="mod.PCA")
grid()
abline(0,1, col="red")
par(op)

#   REMARK: Nice new model with the (huge) disadvantage that the variables z1, z2, z3 and z4 have no physical interpretation.