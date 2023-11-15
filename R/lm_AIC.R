# Calculate the AIC manually
# n is the number of observations
# rss is the residual sum of squares
# k is the number of parameters (including the intercept)

#' Multiply Linear Regression
#'
#' @param formula
#' @param data
#'
#' @return List containing the following elements
#' \itemize{
#' \item beta estimates(coefficients)
#' \item fitted_values
#' \item residuals
#' \item rsquared
#' }
#' @export
#'
#' @examples
#' data = mtcars
#' formula = mpg ~ disp + hp
#' my_lm(formula, data)

my_lm <- function(formula, data) {
  terms <- as.formula(formula)
  X <- model.matrix(terms, data)
  Y <- model.response(model.frame(terms, data))
  # Calculate the estimates for beta
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
  my_coefficients = t(beta_hat)
  fitted_values <- X %*% beta_hat
  my_residuals = Y - fitted_values
  rss <- sum(my_residuals^2)
  tss <- sum((Y - mean(Y))^2)
  rsquared <- 1 - rss/tss
  return(list(coefficients=my_coefficients, fitted.values=fitted_values,
              residuals=my_residuals, rsquared = rsquared))
}
#' Calculate the AIC manually
#'
#' @param formula
#' @param data
#'
#' @return List containing the following elements
#' \itemize{
#' \item beta estimates(coefficients)
#' \item fitted_values
#' \item residuals
#' \item rsquared
#' }
#' @export
#'
#' @examples
#' data = mtcars
#' formula = mpg ~ disp + hp
#' n is the number of observations
#' rss is the residual sum of squares
#' k is the number of parameters (including the intercept)
#' calculate_aic(formula, data)

calculate_aic <- function(formula, data) {
  my_lm <- function(formula, data) {
    terms <- as.formula(formula)
    X <- model.matrix(terms, data)
    Y <- model.response(model.frame(terms, data))
    # Calculate the estimates for beta
    beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
    my_coefficients = t(beta_hat)
    fitted_values <- X %*% beta_hat
    my_residuals = Y - fitted_values
    rss <- sum(my_residuals^2)
    tss <- sum((Y - mean(Y))^2)
    rsquared <- 1 - rss/tss
    return(list(coefficients=my_coefficients, fitted.values=fitted_values,
                residuals=my_residuals, rsquared = rsquared))
  }
  lm_model = my_lm(formula, data)
  n <- length(lm_model$residuals)
  p <- length(lm_model$coefficients)
  rss <- sum((lm_model$residuals)^2)
  k <- length(lm_model$coefficients)
  aic_value <- n * (log(2* pi * rss / (n - p)) + 1) + 2 * k
  return(aic_value)
}
