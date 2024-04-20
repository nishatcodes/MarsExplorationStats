#' Summary for MARS Model Objects
#'
#' Provides a comprehensive summary of a Multivariate Adaptive Regression Splines (MARS) model, including model formula, number of terms, coefficients, goodness-of-fit measures (R-squared and Adjusted R-squared), and residual statistics (mean and standard deviation).
#'
#' @param object A MARS model object, expected to contain components like the model formula, coefficients, residuals, and observed values (`$y`). Ensure the object is the result of fitting a MARS model.
#'
#' @return Invisibly returns a list with components of the model summary, including formula, number of terms, coefficients, R-squared, Adjusted R-squared, residual mean, and residual standard deviation. This function is primarily used for its side effect of printing the summary to the console.
#'
#'
#' @importFrom stats rnorm
#' @export
summary.mars <- function(object) {
  # Extract key components
  formula <- object$formula
  n_terms <- length(object$coefficients)
  coefficients <- object$coefficients
  residuals <- object$residuals
  
  # Calculate goodness-of-fit measures if not part of the object
  rss <- sum(residuals^2)
  tss <- sum((object$y - mean(object$y))^2)
  r_squared <- 1 - rss / tss
  adj_r_squared <- 1 - (1 - r_squared) * (length(object$y) - 1) / (length(object$y) - n_terms - 1)
  
  # Calculate residual statistics
  residual_mean <- mean(residuals)
  residual_sd <- sd(residuals)
  
  # Optionally, calculate more diagnostics or summary statistics...
  
  # Print the summary
  cat("Summary of MARS Model\n\n")
  cat("Formula:\n")
  print(formula)
  cat("\nNumber of Terms: ", n_terms, "\n")
  cat("Coefficients:\n")
  print(coefficients)
  
  # Print goodness-of-fit measures
  cat("\nGoodness-of-Fit Measures:\n")
  cat("R-Squared: ", round(r_squared, 4), "\n")
  cat("Adjusted R-Squared: ", round(adj_r_squared, 4), "\n")
  
  # Print residual statistics
  cat("\nResidual Statistics:\n")
  cat("Residual Mean: ", round(residual_mean, 4), "\n")
  cat("Residual Standard Deviation: ", round(residual_sd, 4), "\n")
  
  # Return a structured summary
  return(list(formula = formula,
              n_terms = n_terms,
              coefficients = coefficients,
              r_squared = r_squared,
              adj_r_squared = adj_r_squared,
              residual_mean = residual_mean,
              residual_sd = residual_sd))
}
