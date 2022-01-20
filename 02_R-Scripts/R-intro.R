#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Applied Statistics and Data Analysis
#   "R-intro.R"
#   Author: Marcel Steiner-Curtis
#   Date: 17.09.2008    sml
#         02.05.2018    English version
#--------------------------------------------------------------------------------------------------------------------------------------------------



#--------------------------------------------------------------------------------------------------------------------------------------------------
#   1. Mathematical operations
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   basic mathematics
2+4.3
##  6.3
sin(2*pi)
##  -2.449213e-16


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   variables
x <- 2
y <- 4.3
x+y
##  6.3


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   vectors and vector operations
v1 <- c(1, 2, 3)
v2 <- c(2, 4, 6)

v1+v2
##  3 6 9
v1*v2
##  2  8 18

v1+pi
#   4.141593 5.141593 6.141593

1/v1
##  1.0000000 0.5000000 0.3333333

exp(v1)
##  2.718282  7.389056 20.085537

v.3 <- c("Fritz", "Hans", "Urs", 2, 3, NA, TRUE)
v.3
##  "Fritz" "Hans"  "Urs"   "2"     "3"     NA      "TRUE"


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   matrices
A <- matrix(c(1,2,3,4,5,6), ncol=2, nrow=3)
A
##         [,1] [,2]
##    [1,]    1    4
##    [2,]    2    5
##    [3,]    3    6

B <- matrix(c(1,2,3,4,5,6), ncol=1, nrow=6)
B
##         [,1]
##    [1,]    1
##    [2,]    2
##    [3,]    3
##    [4,]    4
##    [5,]    5
##    [6,]    6

t(B)
##         [,1] [,2] [,3] [,4] [,5] [,6]
##    [1,]    1    2    3    4    5    6


#   matrix multiplication with mismatched matrices
A %*% A
#   Error in A %*% A
A %*% t(A)
##         [,1] [,2] [,3]
##    [1,]   17   22   27
##    [2,]   22   29   36
##    [3,]   27   36   45

#   BUT: component-wise multiplication
A * A
##         [,1] [,2]
##    [1,]    1   16
##    [2,]    4   25
##    [3,]    9   36


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   basic graphics
?seq
x <- seq(from=0, to=1, by=.1)
y <- x^2

plot(x, y)
lines(x,y)




#--------------------------------------------------------------------------------------------------------------------------------------------------
#   2. Statistics
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   distributions
?dnorm

#   probalility
pnorm(q=0, mean=0, sd=1)
##  0.5

#   quantile
qnorm(0.95)
##  1.644854

#   first plot
x <- seq(from=-4, to=4, by=.1)
y1 <- dnorm(x)
y2 <- pnorm(x)
plot(x, y1, type="l", ylim=c(0,1))
points(x, y2, type="l", col="red")

#   or easier with the command "curve"
curve(dnorm(x), from=-4, to=4, ylim=c(0,1))
curve(pnorm(x), from=-4, to=4, add=TRUE, col="red")


#   generate normally distributed random numbers
set.seed(1)
ran <- rnorm(10000, 0, 1)
ran
##     [1] -0.626453811  0.183643324 -0.835628612  1.595280802  0.329507772
##     [6] -0.820468384  0.487429052  0.738324705  0.575781352 -0.305388387
##    [11]  ...
plot(ran)

#   less basic graphics
ran.mat <- matrix(ran,100,100)
image(ran.mat)
image(solve(ran.mat))


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   read data

#   reading the data from a local source (notice the "\\", resp "/" in the path)
file <- "C:\\Dropbox\\FHNW\\Unterricht\\Module\\FTAL-MSE\\AppStat (e)\\04 Datasets\\sample.dat"
file <- "C:/Dropbox/FHNW/Unterricht/Module/FTAL-MSE/AppStat (e)/04 Datasets/sample.dat"

#   or
file <- "U:/Eigene Dateien/Unterricht/Module/FTAL-MSE/AppStat (e)/04 Datasets/sample.dat"
data <- read.table(file, header=TRUE)
data
##         x1   x2   x3
##    1  15.4 15.6 15.3
##    2  15.4 17.1 15.2
##    3  16.1 16.1 13.5
##    4  13.5 12.5 10.2
##    5  18.3 16.1 17.0
##    6  19.2 17.2 19.4
##    7  14.1 12.4 11.7
##    8  15.6 13.3 13.6
##    9  13.9 14.9 15.5
##    10 18.7 21.2 20.1
##    11 15.3 13.1 13.7
##    12 16.6 18.0 18.0
##    13 17.0 15.2 18.1
##    14 16.3 16.5 17.7
##    15  8.4  7.7  8.4
##    16 11.1 13.8 11.9
##    17 16.5 17.1 18.5
##    18 18.0 14.1 15.9
##    19 17.8 17.3 12.0
##    20 11.5 10.8 11.2

str(data)
##    'data.frame':   20 obs. of  3 variables:
##    $ x1: num  15.4 15.4 16.1 13.5 18.3 19.2 14.1 15.6 13.9 18.7 ...
##    $ x2: num  15.6 17.1 16.1 12.5 16.1 17.2 12.4 13.3 14.9 21.2 ...
##    $ x3: num  15.3 15.2 13.5 10.2 17 19.4 11.7 13.6 15.5 20.1 ...

#   get the first record x1 from data set
data[,1]
#   or
data$x1


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   mean, median, etc.
mean(data$x1)
##  15.435
median(data$x1)
##  15.85
sd(data$x1)
##  2.743850
var(data$x1)
##  7.52871

#   for the whole data set
apply(X=data, MARGIN=2, FUN=mean)
##         x1     x2     x3
##     15.435 15.000 14.845


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (a) Graphics
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   histogram
hist(data$x1)

#   Box and whisker plot of the first record in the data set
boxplot(data$x1)
#   Box and whisker plot of all records in the data set
boxplot(data)


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (b) Statistical Tests
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Student t-test
t.test(x=data$x1, y=data$x2, alternative="two.sided", paired=FALSE, var.equal=TRUE)
##            Two Sample t-test
##
##    data:  data$x1 and data$x2
##    t = 0.4838, df = 38, p-value = 0.6313
##    alternative hypothesis: true difference in means is not equal to 0
##    95 percent confidence interval:
##     -1.385194  2.255194
##    sample estimates:
##    mean of x mean of y
##       15.435    15.000

#   REMARK: Since p-value = 0.6313> 0.05 = alpha, the null hypothesis is accepted, i.e. no difference between x1 and x2.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   ANOVA (cf. Part III, Design of Experiment)
#   prepare data
data.aov <- data.frame(Y=c(data$x1,data$x2,data$x3),
                       fac=c(rep("x1", length=nrow(data)),
                             rep("x2", length=nrow(data)),
                             rep("x3", length=nrow(data)) )
                             )
str(data.aov)
##    'data.frame':   60 obs. of  2 variables:
##    $ Y  : num  15.4 15.4 16.1 13.5 18.3 19.2 14.1 15.6 13.9 18.7 ...
##    $ fac: Factor w/ 3 levels "x1","x2","x3": 1 1 1 1 1 1 1 1 1 1 ...

anova(aov(Y ~ fac, data=data.aov))
##    Analysis of Variance Table
##
##    Response: Y
##              Df Sum Sq Mean Sq F value Pr(>F)
##    fac        2   3.74    1.87  0.2084 0.8125
##    Residuals 57 511.88    8.98

#   REMARK: Since p-value = 0.8125> 0.05 = alpha, the null hypothesis is accepted, i.e. no difference between the individual groups.


#--------------------------------------------------------------------------------------------------------------------------------------------------
#   (c) Regression (cf. Part II, Multivariate Regression)
#--------------------------------------------------------------------------------------------------------------------------------------------------
#   graphics
plot(data$x1, data$x2)

mod <- lm(x2 ~ x1, data)
summary(mod)
##    Call:
##    lm(formula = x2 ~ x1, data = data)
##
##    Residuals:
##        Min      1Q  Median      3Q     Max
##    -3.1290 -1.2372 -0.2678  1.1894  3.3626
##
##    Coefficients:
##                Estimate Std. Error t value Pr(>|t|)
##    (Intercept)   1.5866     2.3129   0.686    0.501
##    x1            0.8690     0.1476   5.886 1.43e-05 ***
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    Residual standard error: 1.766 on 18 degrees of freedom
##    Multiple R-Squared: 0.6581,     Adjusted R-squared: 0.6391
##    F-statistic: 34.64 on 1 and 18 DF,  p-value: 1.425e-05

#   add red line to the scatter plot
abline(mod, col="red")


#--------------------------------------------------------------------------------------------------------------------------------------------------
