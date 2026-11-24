rsi <- function(data, period) {
  if (length(data) <= period || period < 1) {
    stop("Invalid period")
  }
  
  diff_values <- diff(data)
  gains <- pmax(diff_values, 0)
  losses <- pmax(-diff_values, 0)
  
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])
  
  rsi_values <- rep(NA_real_, length(data))
  
  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) +
                   gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) +
                   losses[i - 1]) / period
    
    rs <- avg_gain / avg_loss
    rsi_values[i] <- 100 - (100 / (1 + rs))
  }
  
  return(rsi_values)
}