#' Displays a summary of a Multivariate Adaptive Regression Splines (MARS) model,
#' including the original function call and the coefficients for each of the basis
#' functions in the model. This function is designed to enhance the interpretability
#' and transparency of MARS model outputs by providing a clear and structured summary.
#'
#' @param x A MARS model object that contains the details of the model, including
#'          the model's call and basis function coefficients.
#' @return Invisible. This function is called for its side effect, which is
#'         printing the MARS model summary to the console.
#'
#' @examples
#' # Assuming 'mars_model' is a MARS model object you've created:
#' # print.mars(mars_model)
#'
#' @export
#' @method print mars
print.mars<- function(x) {
  
  cat("FUNCTION CALL:\n\t")
  print(x$call)
  
  # Printing the results, specifically the coefficients
  cat("RESULT:\n\tCoefficients:\n")
  
  # Iterate over the coefficients and print each with its corresponding name
  coefs <- coefficients(x)
  for (i in seq_along(coefs)) {
    line <- sprintf("\t%s\t%s\n", coefs[i], colnames(x$B)[i])
    cat(line)
  }
  
  invisible(x) # Return the object invisibly for consistency with print methods
}
