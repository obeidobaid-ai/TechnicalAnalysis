# Select the folder containing the nine indicator scripts.
indicator_dir <- choose.dir(
  caption = "Select your assignment5 folder"
)

if (is.na(indicator_dir)) {
  stop("No folder selected.")
}

# Load the functions into a fresh environment.
test_env <- new.env(parent = baseenv())

indicator_files <- c(
  "sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
  "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R"
)

for (filename in indicator_files) {
  source(file.path(indicator_dir, filename), local = test_env)
}

# Run independent checks against known results.
test_log <- capture.output(
  with(test_env, {
    
    # 1. SMA: averages of consecutive pairs.
    stopifnot(isTRUE(all.equal(
      sma(c(2, 4, 6), 2), c(3, 5)
    )))
    cat("SMA passed\n")
    
    # 2. EMA: multiplier is 0.5.
    stopifnot(isTRUE(all.equal(
      ema(c(2, 4, 6), 3), c(2, 3, 4.5)
    )))
    cat("EMA passed\n")
    
    # 3. MACD: compare all three components.
    m <- macd(c(100, 105, 110), 3, 5, 2)
    stopifnot(
      isTRUE(all.equal(m$macd_line, c(0, 5/6, 65/36))),
      isTRUE(all.equal(m$signal_line, c(0, 5/9, 25/18))),
      isTRUE(all.equal(m$histogram, c(0, 5/18, 5/12)))
    )
    cat("MACD passed\n")
    
    # 4. Population standard deviation.
    stopifnot(
      stdev(c(2, 4, 4, 4, 5, 5, 7, 9)) == 2,
      stdev(c(5, 5, 5)) == 0
    )
    cat("Standard deviation passed\n")
    
    # 5. Linear regression.
    fit <- linreg(c(2, 4, 6, 8, 10), 5, 0)
    stopifnot(
      isTRUE(all.equal(fit$slope, 2)),
      isTRUE(all.equal(fit$intercept, 0)),
      isTRUE(all.equal(fit$predicted_values, c(2, 4, 6, 8, 10)))
    )
    cat("Linear regression passed\n")
    
    # 6. RSI: rising, falling, and flat prices.
    stopifnot(
      all(rsi(1:6, 3)[4:6] == 100),
      all(rsi(6:1, 3)[4:6] == 0),
      all(rsi(rep(5, 6), 3)[4:6] == 50),
      all(is.na(rsi(1:6, 3)[1:3]))
    )
    cat("RSI passed\n")
    
    # 7. Stochastic RSI: independently recorded expected values.
    sr <- stoch_rsi(
      c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62),
      5, 2, 2
    )
    stopifnot(
      isTRUE(all.equal(
        sr$k_line,
        c(0.3323036, 0.7171563, 0.8848527, 0.7949911),
        tolerance = 1e-6
      )),
      isTRUE(all.equal(
        sr$d_line,
        c(0.5247300, 0.8010045, 0.8399219),
        tolerance = 1e-6
      ))
    )
    cat("Stochastic RSI passed\n")
    
    # 8. Crossover.
    stopifnot(identical(
      crossover(c(1, 3, 1, 3), rep(2, 4)),
      c("None", "Up", "Down", "Up")
    ))
    cat("Crossover passed\n")
    
    # 9. Crossunder.
    stopifnot(identical(
      crossunder(c(3, 1, 3, 1), rep(2, 4)),
      c("None", "True", "False", "True")
    ))
    cat("Crossunder passed\n")
    
    cat("\nAll nine indicator test groups passed.\n")
  })
)

# Display and save the test evidence.
cat(test_log, sep = "\n")
writeLines(
  test_log,
  file.path(indicator_dir, "test_results.txt")
)