# Linear regression using the assignment's indexing rules.
linreg <- function(regressionSource, regressionLength,
                   regressionOffset) {
  
  # Validate the source data.
  if (!is.numeric(regressionSource) ||
      any(!is.finite(regressionSource))) {
    stop("Source must contain only finite numeric values.")
  }
  
  # Validate integer arguments.
  valid_integer <- function(x, minimum) {
    is.numeric(x) && length(x) == 1L &&
      is.finite(x) && x >= minimum && x == floor(x)
  }
  
  if (!valid_integer(regressionLength, 2)) {
    stop("regressionLength must be an integer of at least 2.")
  }
  
  if (!valid_integer(regressionOffset, 0)) {
    stop("regressionOffset must be a nonnegative integer.")
  }
  
  n <- length(regressionSource)
  
  if (regressionLength > n) {
    stop("regressionLength cannot exceed the source length.")
  }
  
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength.")
  }
  
  # Follow the supplied pseudocode for selecting points.
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  
  if (end_index - start_index + 1 < 2) {
    stop("The selected range must contain at least two points.")
  }
  
  source_subset <- regressionSource[start_index:end_index]
  index_values <- seq_along(source_subset)
  
  # Calculate means.
  sum_index <- sum(index_values)
  sum_source <- sum(source_subset)
  mean_index <- sum_index / length(index_values)
  mean_source <- sum_source / length(source_subset)
  
  # Calculate slope and intercept.
  numerator <- sum(
    (index_values - mean_index) * (source_subset - mean_source)
  )
  denominator <- sum((index_values - mean_index)^2)
  
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  
  # Calculate fitted values.
  predicted_values <- slope * index_values + intercept
  
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}