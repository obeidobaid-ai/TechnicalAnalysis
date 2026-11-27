# Detect crossings using the assignment's string outputs.
crossover <- function(arr1, arr2) {
  
  # Validate the inputs.
  if (!is.numeric(arr1) || !is.numeric(arr2) ||
      any(!is.finite(arr1)) || any(!is.finite(arr2))) {
    stop("Both arrays must contain only finite numeric values.")
  }
  
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length.")
  }
  
  # Initialize every position as no crossing.
  crossover_signals <- rep("None", length(arr1))
  
  if (length(arr1) > 1L) {
    for (i in 2:length(arr1)) {
      if (arr1[i] > arr2[i] &&
          arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
        
      } else if (arr1[i] < arr2[i] &&
                 arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      }
    }
  }
  
  return(crossover_signals)
}