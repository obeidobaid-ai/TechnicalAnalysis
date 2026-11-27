# Crossunder using the assignment's string outputs.
crossunder <- function(arr1, arr2) {
  
  # Validate the inputs.
  if (!is.numeric(arr1) || !is.numeric(arr2) ||
      any(!is.finite(arr1)) || any(!is.finite(arr2))) {
    stop("Both arrays must contain only finite numeric values.")
  }
  
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length.")
  }
  
  # Initialize the output.
  crossunder_signals <- rep("False", length(arr1))
  
  if (length(arr1) > 0L) {
    crossunder_signals[1] <- "None"
  }
  
  # Detect a move from on or above arr2 to below arr2.
  if (length(arr1) > 1L) {
    for (i in 2:length(arr1)) {
      if (arr1[i] < arr2[i] &&
          arr1[i - 1] >= arr2[i - 1]) {
        crossunder_signals[i] <- "True"
      }
    }
  }
  
  return(crossunder_signals)
}