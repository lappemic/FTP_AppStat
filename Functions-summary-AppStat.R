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

plotting_CUSUM_chart_of_individual_measurement(df)
