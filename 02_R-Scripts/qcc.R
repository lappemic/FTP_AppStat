#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Part I: SPC
#   "qcc.R"
#   Author: Marcel Steiner-Curtis
#   Date: 15.10.2008    sml
#         07.05.2018    English version
#--------------------------------------------------------------------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   load package
require(qcc)

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Shewart Quality Control Charts, qcc
#--------------------------------------------------------------------------------------------------------------------------------------------------
##    Description: Create an object of class 'qcc' to perform statistical quality control.
##                 This object may then be used to plot Shewhart charts, Cusum and EWMA plotting,
##                 drawing OC curves, computes capability indices, and more.
##    Usage:       qcc(data, type, sizes, center, std.dev, limits, target,
##                     data.name, labels, newdata, newsizes, newlabels,
##                     nsigmas = 3, confidence.level, rules = shewhart.rules, plot = TRUE, ...)


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Example: Continous Data, pistonrings
#   from "Montgomery, D.C. (1991) Introduction to Statistical Quality Control, 2nd ed, New York, John Wiley & Sons, pp. 206-213"
#--------------------------------------------------------------------------------------------------------------------------------------------------
data(pistonrings)
#   attach data
attach(pistonrings)
help(pistonrings)
##    A data frame with 200 observations on the following 3 variables.
##      diameter:   a numeric vector
##      sample:     sample ID
##      trial:      trial sample indicator (TRUE/FALSE)

pistonrings
##        diameter sample trial
##    1     74.030      1  TRUE
##    2     74.002      1  TRUE
##    3     74.019      1  TRUE
##    4     73.992      1  TRUE
##    ...
##    125   74.013     25  TRUE
##    126   74.012     26 FALSE
##    ...
##    200   74.020     40 FALSE

str(pistonrings)
##    'data.frame':   200 obs. of  3 variables:
##    $ diameter: num  74 74 74 74 74 ...
##    $ sample  : int  1 1 1 1 1 2 2 2 2 2 ...
##    $ trial   : logi  TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE ...

#   summary of data
summary(pistonrings)
##        diameter         sample        trial
##    Min.   :73.97   Min.   : 1.00   Mode :logical
##    1st Qu.:74.00   1st Qu.:10.75   FALSE:75
##    Median :74.00   Median :20.50   TRUE :125
##    Mean   :74.00   Mean   :20.50
##    3rd Qu.:74.01   3rd Qu.:30.25
##    Max.   :74.04   Max.   :40.00

#   graphical presentation with mean value
plot(sample, diameter, cex=0.7)
lines(tapply(diameter, sample, mean))


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   group data
diameter <- qcc.groups(diameter, sample)
##        [,1]   [,2]   [,3]   [,4]   [,5]
##    1  74.030 73.995 73.988 74.002 73.992
##    2  74.009 73.995 73.985 74.008 73.998
##    ...
##    40 74.024 74.019 74.026 74.026 74.020


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   xbar chart (only with trial data)
qcc.xbar <- qcc(diameter[1:25,], type="xbar")
summary(qcc.xbar)
##    Call:
##    qcc(data = diameter[1:25, ], type = "xbar")
##
##    xbar chart for diameter[1:25, ]
##
##    Summary of group statistics:
##       Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##      73.99   74.00   74.00   74.00   74.00   74.01
##
##    Group sample size:  5
##    Number of groups:  25
##    Center of group statistics:  74.00118
##    Standard deviation:  0.009887547
##
##    Control limits:
##          LCL      UCL
##     73.98791 74.01444

#   xbar chart (with trial data and new data)
qcc.xbar.new <- qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,])
summary(qcc.xbar.new)
##    Call:
##    qcc(data = diameter[1:25, ], type = "xbar", newdata = diameter[26:40,     ])
##
##    xbar chart for diameter[1:25, ]
##
##    Summary of group statistics:
##       Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##      73.99   74.00   74.00   74.00   74.00   74.01
##
##    Group sample size:  5
##    Number of groups:  25
##    Center of group statistics:  74.00118
##    Standard deviation:  0.009887547
##
##    Summary of group statistics in diameter[26:40, ]:
##       Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##      73.99   74.00   74.01   74.01   74.01   74.02
##
##    Group sample size:  5
##    Number of groups:  15
##
##    Control limits:
##          LCL      UCL
##     73.98791 74.01444


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   different limits
#       nsigmas: a numeric value specifying the number of sigmas to use for computing control limits.
qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], nsigmas=2)
#       confidence.level: a numeric value between 0 and 1 specifying the confidence level of the computed probability limits.
qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], confidence.level=0.99)


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   chart for R
qcc(diameter[1:25,], type="R")
qcc(diameter[1:25,], type="R", newdata=diameter[26:40,])


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   chart for s
qcc(diameter[1:25,], type="S")
qcc(diameter[1:25,], type="S", newdata=diameter[26:40,])


#   detach data
detach(pistonrings)

#--------------------------------------------------------------------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Example: Count Data, orangejuice
#   from "Montgomery, D.C. (1991) Introduction to Statistical Quality Control, 2nd ed, New York, John Wiley & Sons, pp. 152-155."
#--------------------------------------------------------------------------------------------------------------------------------------------------
data(orangejuice)
#   attach data
attach(orangejuice)
help(orangejuice)
##    A data frame with 54 observations on the following 4 variables:
##      sample:   sample id
##      D:        number of defectives
##      size:     sample sizes
##      trial:    trial samples (TRUE/FALSE)

orangejuice
##       sample  D size trial
##    1       1 12   50  TRUE
##    2       2 15   50  TRUE
##    ...
##    30     30  6   50  TRUE
##    31     31  9   50 FALSE
##    ...
##    54     54  5   50 FALSE

str(orangejuice)
##    'data.frame':   54 obs. of  4 variables:
##    $ sample: int  1 2 3 4 5 6 7 8 9 10 ...
##    $ D     : int  12 15 8 10 4 7 16 9 14 10 ...
##    $ size  : int  50 50 50 50 50 50 50 50 50 50 ...
##    $ trial : logi  TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE ...

#   summary of data
summary(orangejuice)
##        sample            D               size      trial
##    Min.   : 1.00   Min.   : 2.000   Min.   :50   Mode :logical
##    1st Qu.:14.25   1st Qu.: 5.000   1st Qu.:50   FALSE:24
##    Median :27.50   Median : 7.000   Median :50   TRUE :30
##    Mean   :27.50   Mean   : 8.889   Mean   :50
##    3rd Qu.:40.75   3rd Qu.:12.000   3rd Qu.:50
##    Max.   :54.00   Max.   :24.000   Max.   :50

#   boxplot
boxplot(D ~ trial)

#   graphical presentation
mark <- ifelse(trial, 1, 2)
plot(sample, D, type="b", col=mark, pch=mark)

#   chart for attributes data
qcc.p <- qcc(D[trial], sizes=size[trial], type="p")
summary(qcc.p)
##    Call:
##    qcc(data = D[trial], type = "p", sizes = size[trial])
##
##    p chart for D[trial]
##
##    Summary of group statistics:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##    0.0800  0.1600  0.2100  0.2313  0.2950  0.4800
##
##    Group sample size:  50
##    Number of groups:  30
##    Center of group statistics:  0.2313333
##    Standard deviation:  0.421685
##
##    Control limits:
##           LCL       UCL
##    0.05242755 0.4102391


#   attach data
detach(orangejuice)

#--------------------------------------------------------------------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------------------------------------------------------------
#   CUSUM Chart
#--------------------------------------------------------------------------------------------------------------------------------------------------
##    Description:  Draw a Cusum chart from an object of class 'qcc'.
##
##    Usage:    cusum(object, decision.int = 5, se.shift = 1,
##                    label.bounds = c("LDB", "UDB"), add.stats = TRUE, chart.all = TRUE,
##                    ylim = NULL, axes.las = 0, restore.par = TRUE, ...)
##    Details:  Cusum charts display how the group summary statistics deviate above or below the process center or target value,
##              relative to the standard errors of the summary statistics. Useful to detect small
##              and permanent variation on the mean of the process.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   example pistonrings
data(pistonrings)
attach(pistonrings)
#   group data
diameter <- qcc.groups(diameter, sample)

#   CUSUM for trial data
q.cusum <- cusum(diameter[1:25,], type="xbar", nsigmas=3, plot=FALSE)
summary(q.cusum)
##    Call:
##    cusum(data = diameter[1:25, ], plot = FALSE, type = "xbar", nsigmas = 3)
##
##    cusum chart for diameter[1:25, ]
##
##    Summary of group statistics:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##    73.99   74.00   74.00   74.00   74.01   74.01
##
##    Group sample size:  5
##    Number of groups:  25
##    Center of group statistics:  74.00282
##    Standard deviation:  0.01128117
##
##    Decision interval (std.err.): 5
##    Shift detection  (std. err.): 1
plot(q.cusum)

#   CUSUM for all data
q.cusum.new <- cusum(diameter[1:25,], type="xbar", newdata=diameter[26:40,], plot=FALSE)
summary(q.cusum.new)
##    Call:
##    cusum(data = diameter[1:25, ], newdata = diameter[26:40, ], plot = FALSE,     type = "xbar")
##
##    cusum chart for diameter[1:25, ]
##
##    Summary of group statistics:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##    73.99   74.00   74.00   74.00   74.01   74.01
##
##    Group sample size:  5
##    Number of groups:  25
##    Center of group statistics:  74.00282
##    Standard deviation:  0.01128117
##
##    Summary of group statistics in diameter[26:40, ]:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##    74.00   74.00   74.01   74.00   74.01   74.01
##
##    Group sample size:  5
##    Number of groups:  15
##
##    Decision interval (std.err.): 5
##    Shift detection  (std. err.): 1
plot(q.cusum.new)
#   CUSUM for all non-trial data
plot(q.cusum.new, chart.all=FALSE)

#   detach data
detach(pistonrings)

#--------------------------------------------------------------------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------------------------------------------------------------
#   EWMA Chart
#--------------------------------------------------------------------------------------------------------------------------------------------------
##    Description:  Draw an EWMA chart from an object of class 'qcc'.
##    Usage:        ewma(object, lambda=0.2, nsigmas=object$nsigmas,
##                       add.stats = TRUE, xlab, ylab, ylim = NULL, axes.las = 0,
##                       restore.par = TRUE, ...)
##    Details:      EWMA chart smooths a series of data based on a moving average with weights which decay exponentially.
##                  Useful to detect small and permanent variation on the mean of the process.

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   example pistonrings
data(pistonrings)
attach(pistonrings)
#   group data
diameter <- qcc.groups(diameter, sample)

#   EWMA  for trial data
q.ewma <- ewma(diameter[1:25,], type="xbar", nsigmas=3, lambda=0.2, plot=FALSE)
summary(q.ewma)
##    Call:
##    ewma(data = diameter[1:25, ], lambda = 0.2, nsigmas = 3, newdata = diameter[26:40,     ], plot = FALSE, type = "xbar")
##
##    ewma chart for diameter[1:25, ]
##
##    Summary of group statistics:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##    74.00   74.00   74.00   74.00   74.01   74.02
##
##    Group sample size:  5
##    Number of groups:  25
##    Center of group statistics:  74.00305
##    Standard deviation:  0.01186586
##
##    Smoothing parameter: 0.2
##    Control limits:
##            LCL      UCL
##    1   73.99986 74.00623
##    2   73.99897 74.00713
##    ...
##    25  73.99774 74.00835
plot(q.ewma)

#   EWMA  for all data
q.ewma.new <- ewma(diameter[1:25,], newdata=diameter[26:40,], type="xbar", lambda=0.2, nsigmas=3, plot=FALSE)
summary(q.ewma.new)
##    Call:
##    ewma(data = diameter[1:25, ], lambda = 0.2, nsigmas = 3, newdata = diameter[26:40,     ], plot = FALSE, type = "xbar")
##
##    ewma chart for diameter[1:25, ]
##
##    Summary of group statistics:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##    74.00   74.00   74.00   74.00   74.01   74.02
##
##    Group sample size:  5
##    Number of groups:  25
##    Center of group statistics:  74.00305
##    Standard deviation:  0.01186586
##
##    Summary of group statistics in diameter[26:40, ]:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##    74.00   74.00   74.00   74.00   74.01   74.02
##
##    Group sample size:  5
##    Number of groups:  15
##
##    Smoothing parameter: 0.2
##    Control limits:
##            LCL      UCL
##    1   73.99986 74.00623
##    2   73.99897 74.00713
##    ...
##    40  73.99774 74.00835
plot(q.ewma.new)

#   detach data
detach(pistonrings)

#--------------------------------------------------------------------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------------------------------------------------------------
#   OC Curves
#--------------------------------------------------------------------------------------------------------------------------------------------------
##    Description:  Draws the operating characteristic curves for a 'qcc' object.
##    Usage:        oc.curves(object, ...)
##    Details       An operating characteristic curve graphically provides information about the probability of not detecting a shift
##                  in the process. oc.curves is a generic function which calls the proper function depending on the type of 'qcc' object.
##                  Further arguments provided through ... are passed to the specific function depending on the type of chart.

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   example pistonrings
data(pistonrings)
attach(pistonrings)
#   group data
diameter <- qcc.groups(diameter, sample)

#   OC curves
qcc.xbar.new <- qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], plot=FALSE)
oc.curves(qcc.xbar.new)

#   detach data
detach(pistonrings)

#--------------------------------------------------------------------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Process Capability Analysis
#--------------------------------------------------------------------------------------------------------------------------------------------------
##    Description:  Computes process capability indices for a 'qcc' object of type "xbar" and plot the histogram.
##    Usage:        process.capability(object,spec.limits, target, std.dev, nsigmas,
##                                     confidence.level = 0.95, breaks = "scott",
##                                     add.stats = TRUE, print = TRUE, restore.par = TRUE)
##    Details:      This function calculates confidence limits for C_p using the method described by Chou et al. (1990).
##                  Approximate confidence limits for C_pl, C_pu and C_pk are computed using the method in Bissell (1990).
##                  Confidence limits for C_pm are based on the method of Boyles (1991); this method is approximate and it assumes
##                  that the target is midway between the specification limits.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   example pistonrings
data(pistonrings)
attach(pistonrings)
#   group data
diameter <- qcc.groups(diameter, sample)

#   process capability analysis
qcc.xbar <- qcc(diameter[1:25,], type="xbar", plot=FALSE)

process.capability(qcc.xbar, spec.limits=c(73.95,74.05))
process.capability(qcc.xbar, spec.limits=c(73.95,74.05), target=74.02)
process.capability(qcc.xbar, spec.limits=c(73.99,74.01))
process.capability(qcc.xbar, spec.limits=c(73.99,74.10))

#   detach data
detach(pistonrings)

#--------------------------------------------------------------------------------------------------------------------------------------------------
