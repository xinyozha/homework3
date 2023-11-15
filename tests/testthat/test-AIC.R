test_that("AIC works", {
  data = mtcars
  formula = mpg ~ disp + hp

  expect_equal(calculate_aic(formula, data),
               step(lm(formula, data)))
})
