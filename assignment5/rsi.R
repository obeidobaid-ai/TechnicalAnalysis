# RSI using the smoothing sequence in the assignment pseudocode.
rsi <- function(data, period) {
  
  # Validate the inputs.
  if (!is.numeric(data) || any(!is.finite(data))) {
    stop("Data must contain only finite numeric values.")
  }
  
  if (!is.numeric(period) || length(period) != 1L ||
      !is.finite(period) || period < 1 ||
      period != floor(period)) {
    stop("Period must be a positive integer.")
  }
  
  if (length(data) <= period) {
    stop("Data must contain at least period + 1 values.")
  }
  
  # Calculate consecutive changes.
  diff_values <- diff(data)
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }
  
  # Initialize averages from the first period changes.
  avg_gain <- sum(gains[seq_len(period)]) / period
  avg_loss <- sum(losses[seq_len(period)]) / period
  
  # Keep initial positions unavailable.
  rsi_values <- rep(NA_real_, length(data))
  
  # Follow the assignment's smoothing loop.
  for (i in seq.int(period + 1L, length(data))) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) /
      period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) /
      period
    
    # Handle zero losses and completely flat prices.
    if (avg_gain == 0 && avg_loss == 0) {
      rsi_values[i] <- 50
    } else if (avg_loss == 0) {
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - 100 / (1 + rs)
    }
  }
  
  return(rsi_values)
}