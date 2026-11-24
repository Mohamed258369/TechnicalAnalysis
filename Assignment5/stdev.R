stdev <- function(data) {
  mean_value <- mean(data)
  variance <- sum((data - mean_value)^2) / length(data)
  return(sqrt(variance))
}