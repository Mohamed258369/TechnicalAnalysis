linreg <- function(regressionSource, regressionLength, regressionOffset) {
  n <- length(regressionSource)
  
  if (regressionLength > n) {
    stop("Regression length is too large")
  }
  
  if (regressionOffset >= regressionLength) {
    stop("Invalid regression offset")
  }
  
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  
  if (end_index <= start_index) {
    stop("At least two regression points are required")
  }
  
  y <- regressionSource[start_index:end_index]
  x <- seq_along(y)
  
  slope <- sum((x - mean(x)) * (y - mean(y))) /
    sum((x - mean(x))^2)
  
  intercept <- mean(y) - slope * mean(x)
  predicted_values <- slope * x + intercept
  
  return(list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  ))
}