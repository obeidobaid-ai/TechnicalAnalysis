# Simple Moving Average using core R functions.
sma <- function(data, period) {
  
  # Validate the input values.
  if (!is.numeric(data) || any(!is.finite(data))) {
    stop("Data must contain only finite numeric values.")
  }
  
  if (!is.numeric(period) || length(period) != 1L ||
      !is.finite(period) || period < 1 ||
      period != floor(period)) {
    stop("Period must be a positive integer.")
  }
  
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period.")
  }
  
  # Create one result for each complete window.
  sma_values <- numeric(length(data) - period + 1L)
  
  for (i in seq_along(sma_values)) {
    current_window <- data[i:(i + period - 1L)]
    sma_values[i] <- sum(current_window) / period
  }
  
  return(sma_values)
}