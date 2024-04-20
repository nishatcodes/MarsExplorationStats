#' Plot Diagnostics for MARS Model Objects
#'
#' Generates diagnostic plots for assessing the fit of a MARS (Multivariate Adaptive Regression Splines) model,
#' including a Residuals vs. Fitted plot and a Residual QQ plot. These plots are useful for evaluating the
#' assumption of homoscedasticity and normality of the residuals.
#'
#' @param x An object of class `mars`, typically the output from fitting a MARS model.
#'          It must contain at least the residuals and fitted values.
#'
#' @details
#' The function creates two diagnostic plots:
#' \enumerate{
#'   \item{Residuals vs Fitted:} Plots the residuals against the fitted values to check for constant variance
#'       (homoscedasticity) and identify any obvious patterns (which may suggest non-linearity).
#'   \item{Residual QQ Plot:} Plots the quantiles of residuals against the theoretical quantiles from a normal
#'       distribution. This plot is useful for checking the normality of residuals.
#' }
#' Both plots are essential diagnostics for linear regression analysis and can similarly inform the quality
#' of a MARS model fit.
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' mars_model <- mars(mpg ~ wt + hp, data=mtcars)
#' plot.mars(mars_model)
#' }
#'
#' @export
plot.mars <- function(x){
  # Fitted vs Residuals Plot
  residual <- x$residuals
  fit <- x$fitted.values
  smooth_fit <- lowess(residual ~ fit)
  
  plot(fit, residual,
       main = "Residuals vs Fitted",
       xlab = "Fitted Values",
       ylab = "Residuals",
       pch = 20, col = "darkblue")
  abline(h = 0, lty = 2, col = "pink") # Highlight the 0 line for residuals
  lines(smooth_fit, col = "deepskyblue", lwd = 2) # Add a smoothed trend line
  
  # Residual QQ Plot
  qqnorm(residual,
         main = "Residual QQ Plot",
         xlab = "Theoretical Quantiles",
         ylab = "Sample Quantiles",
         pch = 20, col = "darkblue")
  qqline(residual, col = "red", lwd = 2)
  
}
