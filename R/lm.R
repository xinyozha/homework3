#' Multiply Linear Regression
#'
#' @param formula input formula such as y ~ x1 + x2
#' @param data input data such as mtcars
#'
#' @return List containing the following elements
#' \itemize{
#' \item beta estimates(coefficients)
#' \item fitted_values
#' \item residuals
#' \item mse
#' \item rsquared
#' \item adj_rsquared
#' \item SE
#' \item t
#' \item p.value
#' \item F_stat
#' \item df1
#' \item df2
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
  my_coefficients <- t(beta_hat)
  fitted_values <- X %*% beta_hat
  my_residuals <- Y - fitted_values
  n <- nrow(X)
  p <- ncol(X)
  rss <- sum(my_residuals^2)
  #sigma squared
  mse <- as.numeric(rss)/(n-p)
  tss <- sum((Y - mean(Y))^2)
  #R.squared
  rsquared <- 1 - rss / tss
  adj_rsquared <- 1-(mse)/((tss)/(n-1))
  #Calculate the estimated standard deviation
  XtX_inv <- solve(t(X) %*% X)
  var.betahat <- diag(XtX_inv)*c(mse)
  sd.betahat <- sqrt(var.betahat)
  #t.test
  t.statistic <- c(my_coefficients/sd.betahat)
  p.value <- c(2*pmin(1-pt(t.statistic,n-p),pt(t.statistic,n-p)))
  #F
  F.statistic <- ((tss - rss)/(p-1))/(rss/(n-p))

  return(list(coefficients = my_coefficients, fitted.values = fitted_values,
              residuals = my_residuals, mse = mse, rsquared = rsquared,
              adj_rsquared = adj_rsquared, SE = sd.betahat, t = t.statistic,
              p.value = p.value, F_stat = F.statistic, df1 = p-1, df2 = n-p))
}

