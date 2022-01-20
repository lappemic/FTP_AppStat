plot.lmSim <- function(obj, which=c(1L:3L), SEED=NULL, Nsim=19)
{
    X <- model.matrix(obj)
    x.n <- nrow(X)
    yh <- fitted(obj)
    sigma <- summary(obj)$sigma
    show <- rep(FALSE, 3)
    show[which] <- TRUE

    ## Tukey-Anscombe Plot
    if(show[1]){
        res <- resid(obj)
        ylim <- extendrange(r=range(res, na.rm = TRUE), f = 0.08)
        plot(yh, res, xlab = "Fitted values", ylab = "Residuals", main="",
             ylim = ylim, type = "n")
        abline(h = 0, lty = 3) ## , col = "gray"
        lines(lowess(yh, res, f=2/3, iter=3), lwd=1.5, col="red")

        if(!is.null(SEED)) set.seed(SEED)
        for(i in 1:Nsim){
            FIT <- lm.fit(x=X, y=yh + rnorm(x.n,0,sigma))
            lines(lowess(fitted(FIT), resid(FIT), f=2/3, iter=3),col="grey")
        }
    }
    ## normal plot
    if(show[2]){
        require(MASS)
        SIM <- matrix(NA, ncol=Nsim, nrow=x.n)
        if(Nsim>0){
            if(!is.null(SEED)) set.seed(SEED)
            for(i in 1: Nsim){
                FIT <- lm.fit(x=X, y=yh + rnorm(x.n,0,sigma))
                SIM[,i] <- sort(qqnorm(stdres(FIT), plot.it=FALSE)$y)
            }
        }

        RQQN <- qqnorm(stdres(obj), plot.it=FALSE)
        ylim <- range(c(RQQN$y, SIM))
        plot(range(RQQN$x), ylim, type="n",
             xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")
        if(Nsim>0)
            points(rep(sort(RQQN$x),Nsim), as.vector(SIM), col="gray")
        points(RQQN$x, RQQN$y, lwd=1, pch=20)
    }
    ## Scale-Location Plot
    if(show[3]){
        require(MASS)
        sqrtabsR <- sqrt(abs(stdres(obj)))
        ylim <- c(0, max(sqrtabsR, na.rm = TRUE))
        yl <- as.expression(substitute(sqrt(abs(YL)),
            list(YL=as.name("Standardized residuals"))))
        plot(yh, sqrtabsR, xlab="Fitted values", ylab=yl, main="",
             ylim=ylim, type="n")
        lines(lowess(yh, sqrtabsR, f=2/3, iter=3), lwd=1.5, col="red")

        if(!is.null(SEED)) set.seed(SEED)
        for(i in 1:Nsim){
            FIT <- lm.fit(x=X, y=yh + rnorm(x.n,0,sigma))
            lines(lowess(fitted(FIT), sqrt(abs(stdres(FIT))), f=2/3, iter=3),
                  col="grey")
        }
    }
    invisible()
}
