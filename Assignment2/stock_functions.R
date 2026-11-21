# BDA400 Assignment 2
# Student: Obeid Obaid

load_stock_data <- function(
    portfolio_file = "Assignment2/portfolio.txt",
    from = "2026-01-01",
    to = "2026-11-20") {
  
  symbols <- trimws(readLines(portfolio_file))
  symbols <- unique(symbols[nzchar(symbols)])
  
  if (length(symbols) == 0) {
    stop("The portfolio file contains no stock symbols.")
  }
  
  stocks <- list()
  
  for (symbol in symbols) {
    prices <- quantmod::getSymbols(
      symbol,
      src = "yahoo",
      from = from,
      to = to,
      auto.assign = FALSE
    )
    
    stock_df <- data.frame(
      Date = as.Date(zoo::index(prices)),
      Open = as.numeric(quantmod::Op(prices)),
      High = as.numeric(quantmod::Hi(prices)),
      Low = as.numeric(quantmod::Lo(prices)),
      Close = as.numeric(quantmod::Cl(prices)),
      Volume = as.numeric(quantmod::Vo(prices)),
      Adjusted = as.numeric(quantmod::Ad(prices))
    )
    
    stocks[[symbol]] <- stock_df
  }
  
  return(stocks)
}# Calculate statistics using daily closing prices
calculate_statistics <- function(stock_df, window = 20) {
  
  prices <- stock_df$Close
  
  if (!is.numeric(prices) || any(!is.finite(prices))) {
    stop("Closing prices must be numeric with no missing values.")
  }
  
  if (window < 2 || window != as.integer(window) ||
      length(prices) < window) {
    stop("The moving-average window must fit the available data.")
  }
  
  # Find all most frequent closing prices
  frequencies <- table(prices)
  
  mode_value <- if (max(frequencies) == 1) {
    "No repeated value"
  } else {
    paste(
      names(frequencies)[frequencies == max(frequencies)],
      collapse = ", "
    )
  }
  
  moving_average <- as.numeric(TTR::SMA(prices, n = window))
  
  summary <- data.frame(
    Observations = length(prices),
    Mean = mean(prices),
    Mode = mode_value,
    Median = median(prices),
    Standard_Deviation = sd(prices),
    MA_Window = window,
    Latest_Moving_Average = tail(moving_average, 1)
  )
  
  series <- data.frame(
    Date = stock_df$Date,
    Close = prices,
    Moving_Average = moving_average
  )
  
  return(list(summary = summary, series = series))
}
