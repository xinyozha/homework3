test_that("my_lm() works", {

  data <- mtcars
  formula <- mpg ~ disp + hp

  expect_equal(as.numeric(my_lm(formula, data)$coefficients),
               as.numeric(lm(formula, data)$coefficients))
  expect_equal(as.numeric(my_lm(formula, data)$residuals),
               as.numeric(lm(formula, data)$residuals))
  expect_equal(as.numeric(my_lm(formula, data)$rsquared),
               as.numeric(summary(lm(formula, data))$r.squared))
})
