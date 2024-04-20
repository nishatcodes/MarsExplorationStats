#' ANOVA for MARS Model Objects
#'
#' Performs an ANOVA-like variance decomposition for a fitted MARS (Multivariate Adaptive Regression Splines) model object.
#' It calculates and displays the variance explained by each term in the model, as well as the residual and fitted value variances associated with each term.
#'
#' @param obj A fitted MARS model object. This object should contain the basis function matrix `B`,
#' the response variable `y`, model coefficients, residuals, and fitted values as its components.
#'
#' @details
#' The function iterates over each term in the basis function matrix `B` of the provided MARS model object.
#' For each term, it calculates the variance explained by the term, the variance of the residuals associated with the term,
#' and the variance of the fitted values. It handles the intercept term separately, identifying it with a special notation if present.
#' This analysis can help in understanding the contribution of each term to the model's explanatory power.
#'
#' @return
#' The function does not return a value. It performs its analysis and prints the results to the console for each term in the model.
#' For each term, the following metrics are printed:
#' \itemize{
#'   \item Variance explained by the term
#'   \item Residual variance for the term
#'   \item Fitted value variance for the term
#' }
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' # Assuming mars_model is a fitted MARS model object
#' anova.mars(mars_model)
#' }
#'
#' @export
anova.mars = function(obj){
  X <- obj$B
  y <- obj$y
  
  # Iterate over each column in the basis function matrix
  for (i in 1:ncol(X)) {
    termName <- colnames(X)[i]
    # Handle intercept term naming
    if (termName == "B0") {
      cat("Intercept:\n")
    } else {
      cat(paste0("Term: ", termName, "\n"))
    }
    
    # Calculating variance contributions
    termVariance <- var(X[, i] * obj$coefficients[i])
    residualVariance <- var(X[, i] * obj$residuals)
    fittedVariance <- var(X[, i] * obj$fitted.values)
    
    cat(paste0("* Variance explained by term: ", round(termVariance, 4), "\n"))
    cat(paste0("* Residual variance for term: ", round(residualVariance, 4), "\n"))
    cat(paste0("* Fitted value variance for term: ", round(fittedVariance, 4), "\n\n"))
  }
  
  
}
