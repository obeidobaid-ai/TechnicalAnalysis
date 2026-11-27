# Population standard deviation using core R functions.
stdev <- function(data) {
  
  # Validate the input.
  if (!is.numeric(data) || length(data) == 0L ||
      any(!is.finite(data))) {
    stop("Data must be a nonempty vector of finite numbers.")
  }
  
  # Calculate the mean.
  data_mean <- sum(data) / length(data)
  
  # Calculate deviations and their squares.
  differences <- data - data_mean
  squared_differences <- differences^2
  
  # Population variance divides by n.
  variance <- sum(squared_differences) / length(data)
  
  # Take the square root.
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}