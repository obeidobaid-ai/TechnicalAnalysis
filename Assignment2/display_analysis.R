# BDA400 Assignment 2
# Student: Obeid Obaid
# Run from the TechnicalAnalysis project folder.

source("Assignment2/stock_functions.R")

stocks <- load_stock_data()
results <- lapply(stocks, calculate_statistics)

output_dir <- "Assignment2/outputs"
dir.create(output_dir, showWarnings = FALSE)

# Display the first and last records for each stock.
display_stock_data <- function(symbol, stock_df) {
  cat("\nSTOCK:", symbol, "\n")
  cat("Number of records:", nrow(stock_df), "\n")
  cat("\nFirst six records:\n")
  print(head(stock_df))
  cat("\nLast six records:\n")
  print(tail(stock_df))
}

# Plot closing prices and their moving average.
plot_stock <- function(symbol, result) {
  series <- result$series
  
  plot(
    series$Date, series$Close,
    type = "l",
    col = "steelblue",
    lwd = 2,
    main = paste(symbol, "Closing Price and Moving Average"),
    xlab = "Date",
    ylab = "Price (USD)"
  )
  
  lines(
    series$Date, series$Moving_Average,
    col = "darkorange",
    lwd = 2
  )
  
  legend(
    "topleft",
    legend = c("Closing price", "20-day moving average"),
    col = c("steelblue", "darkorange"),
    lty = 1,
    lwd = 2,
    cex = 0.8,
    bty = "n"
  )
}

# Combine the statistics for all stocks.
statistics <- do.call(rbind, lapply(names(results), function(symbol) {
  data.frame(Symbol = symbol, results[[symbol]]$summary)
}))
rownames(statistics) <- NULL

cat("\nSUMMARY STATISTICS — DAILY CLOSING PRICES\n")
print(statistics)

write.csv(
  statistics,
  file.path(output_dir, "summary_statistics.csv"),
  row.names = FALSE
)

# Display records and save data and charts.
for (symbol in names(stocks)) {
  display_stock_data(symbol, stocks[[symbol]])
  
  write.csv(
    stocks[[symbol]],
    file.path(output_dir, paste0(symbol, "_data.csv")),
    row.names = FALSE
  )
  
  write.csv(
    results[[symbol]]$series,
    file.path(output_dir, paste0(symbol, "_moving_average.csv")),
    row.names = FALSE
  )
  
  png(
    file.path(output_dir, paste0(symbol, "_chart.png")),
    width = 1200, height = 700, res = 120
  )
  plot_stock(symbol, results[[symbol]])
  dev.off()
}

# Show all three charts in RStudio.
par(mfrow = c(length(results), 1), mar = c(3, 4, 3, 1))

for (symbol in names(results)) {
  plot_stock(symbol, results[[symbol]])
}

par(mfrow = c(1, 1))
cat("\nFinished. Files saved in Assignment2/outputs.\n")