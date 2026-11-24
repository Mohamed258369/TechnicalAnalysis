sma <- function(data, period) {
  if (length(data) < period) {
    stop("Data length must be at least the period")
  }
  
  sma_values <- numeric(length(data) - period + 1)
  
  for (i in seq_along(sma_values)) {
    sma_values[i] <- mean(data[i:(i + period - 1)])
  }
  
  return(sma_values)
}