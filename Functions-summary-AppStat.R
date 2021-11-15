################################################################################
# Function script for Appstat
################################################################################
# Capturing code in the lecture which is useful for future applications
#------------------------------------------------------------------------------

#*******************************************************************************
#*  Part I: Statistical Process Control (SPC)
#*******************************************************************************

#*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#*  Chapter 4
#*  CUSUMS
#*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#* Plotting the CUSUM-chart of a measurement with 1 sample 
#*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

plotting_CUSUM_chart_of_individual_measurement <- function(df) {
  # Process standard deviation using the standard deviation of the data
  sigma.hat <- sd(df[, 1]); sigma.hat
  
  # Reference value and the decision interval form the process standard deviation
  # Reference value:
  K <- 0.5 * sigma.hat
  
  # Decision interval:
  H <- 5 * sigma.hat
  
  # Calculation of the recursions C^+ and C^- for the CUSUMs
  # C^+ and C^- CUSUMs
  C.plus <- NULL
  C.plus[1] <- 0
  
  C.minus <- NULL
  C.minus[1] <- 0
  
  
  n <- length(df[, 1])
  
  for(i in 1:n) {
    C.plus[i+1] <- max(0, df$diam[i] - (mu0+K) + C.plus[i])
    C.minus[i+1] <- max(0, (mu0-K) - df$diam[i] + C.minus[i])
  }
  C.plus
  C.minus
  
  # Plot the CUSUM chart
  plot(0:n, C.plus, col = "black", pch = 20, ylim = c(-1, 1),
       xlab = "Index", ylab = "Cumulative Sums", main = "CUSUM")
  lines(0:n, C.plus, col = "black")
  points(0:n, -C.minus, pch = 20, col = "gray")
  lines(0:n, -C.minus, col = "gray")
  abline(h = c(-1, 0, 1) * H, lty = c(2, 1, 2))
}

#*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#* Plotting the CUSUM-chart of measurements with several samples
#*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

plotting_CUSUM_of_random_samples <- function(df, mu0, c_value) {
  m <- ncol(df)
  # means, standard deviations and Ranges
  df$mean <- apply(df[, 1:m], 1, mean)
  df$s <- apply(df[, 1:m], 1, sd)
  df$R <- apply(df[, 1:m], 1, function(x){ max(x) - min(x)})
  
  # mean of standard deviations
  s.bar <- mean(df$s); s.bar
  
  # process standard deviation
  sigma.hat <- s.bar / c4; sigma.hat
  
  # reference value
  K <- 0.5 * sigma.hat; K
  
  # decision interval
  H <- 5 * sigma.hat; H
  
  # C^+ and C^- CUSUMs
  C.plus <- NULL
  C.plus[1] <- 0
  C.minus <- NULL
  C.minus[1] <- 0
  
  n <- nrow(df)
  for(i in 1:n) {
    C.plus[i+1] <- max(0, df$mean[i] - (mu0+K) + C.plus[i])
    C.minus[i+1] <- max(0, (mu0-K) - df$mean[i] + C.minus[i])
  }
  C.plus
  C.minus
  
  # CUSUM Chart
  plot(0:n, C.plus, col = "black", pch = 20, ylim = c(-1.5, 1.5),
       xlab = "Index", ylab = "Cumulative Sums", main = "CUSUM of Random Samples")
  lines(0:n, C.plus, col = "black")
  points(0:n, -C.minus, col = "gray")
  lines(0:n, -C.minus, col = "gray")
  abline(h = c(-1, 0, 1) * H, lty = c(2, 1, 2))
  text(rep(2, 3), c(-1, 1) * H, label = c("Lower CUSUM", "Upper CUSUM"), pos = 3)
}

#*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Plotting EWMA-Charts of Random samples
#*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

plotting_EWMA_charts_of_random_samples <- function(df, mu0, c_value, lambda) {
  # number of columns
  n <- ncol(df)
  # mean and standard deviation
  df$mean <- apply(df[, 1:n], 1, mean)
  df$sd <- apply(df[, 1:n], 1, sd)
  
  # mean of standard deviations
  s.bar <- mean(df$sd)
  
  # process standard deviation
  sigma.hat <- s.bar / c_value; sigma.hat
  
  # recursion
  y <- NULL
  y[1] <- mu0
  k <- nrow(df)
  
  for(i in 1:k) {
    y[i+1] <- (1 - lambda) * y[i] + lambda * df$mean[i]
  }
  
  # Calculating the control limits
  sigma.pro <- sigma.hat / sqrt(n) *
    sqrt(lambda / (2-lambda) * (1 - (1-lambda)^(2*(0:k))))
  
  UCL <- mu0 + 3*sigma.pro; UCL
  LCL <- mu0 - 3*sigma.pro; LCL
  
  # EWMA Chart
  plot(0:k, y, pch = 20, ylim = c(0.999, 1.001) * c(min(y), max(y)),
       xlab = "Index", ylab = "EWMA", main = "EWMA")
  lines(0:k, y)
  
  # add control limits
  abline(h = mu0)
  lines(0:k, UCL, lty = 2, type = "S")
  lines(0:k, LCL, lty = 2, type = "S")
  text(rep(k, 2), c(LCL[k], UCL[k]), label = c("LCL", "UCL"), pos = 1)
}


#*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#*  Chapter 5
#*  Acceptance Sampling
#*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#* Operating characteristic function
#*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

operating_characteristic_function <- function(p, N, n, c) {
  #' Function to estimate the operating function
  #' p: Percent defective
  #' N: Lot sizen
  #' n: Sample size
  #' c: Acceptance number
  phyper(q = c, m = p*N, n = (1-p) * N, k = n)
}


#*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#* xy 
#*""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
