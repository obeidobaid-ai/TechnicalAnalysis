# Stochastic RSI following the assignment pseudocode.
# Load rsi.R and sma.R before calling this function.
stoch_rsi <- function(data, period, k_period, d_period) {
  
  # Calculate RSI using our custom function.
  rsi_values <- rsi(data, period)
  
  # Remove the initial unavailable RSI values.
  available_rsi <- rsi_values[!is.na(rsi_values)]
  
  # Use the minimum and maximum available RSI,
  # as specified in the assignment pseudocode.
  min_rsi <- min(available_rsi)
  max_rsi <- max(available_rsi)
  
  if (max_rsi == min_rsi) {
    stop("Stochastic RSI is undefined when all RSI values are equal.")
  }
  
  # Normalize RSI to values between 0 and 1.
  k_values <- (available_rsi - min_rsi) /
    (max_rsi - min_rsi)
  
  # Smooth using our custom SMA function.
  k_line <- sma(k_values, k_period)
  d_line <- sma(k_line, d_period)
  
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}