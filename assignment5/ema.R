# Exponential Moving Average using core R functions.
ema <- function(data, period) {
  
  # Validate the input values.
  if (!is.numeric(data) || length(data) == 0L ||
      any(!is.finite(data))) {
    stop("Data must be a nonempty vector of finite numbers.")
  }
  
  if (!is.numeric(period) || length(period) != 1L ||
      !is.finite(period) || period < 1 ||
      period != floor(period)) {
    stop("Period must be a positive integer.")
  }
  
  # Follow the assignment: start with the first data value.
  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))
  ema_values[1] <- data[1]
  
  if (length(data) > 1L) {
    for (i in 2:length(data)) {
      ema_values[i] <- (data[i] - ema_values[i - 1]) *
        multiplier + ema_values[i - 1]
    }
  }
  
  return(ema_values)
}