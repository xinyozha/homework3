test_that("my_lm() works", {

  data <- mtcars
  formula <- mpg ~ disp + hp
  m1 <- lm(formula, data)
  s_m1 <- summary(m1)
  ff <- c(my_lm(formula, data)$F_stat, my_lm(formula, data)$df1,
               my_lm(formula, data)$df2)

  expect_equal(as.numeric(my_lm(formula, data)$coefficients),
               as.numeric(m1$coefficients))
  expect_equal(as.numeric(my_lm(formula, data)$residuals),
               as.numeric(m1$residuals))
  expect_equal(as.numeric(my_lm(formula, data)$fitted.values),
               as.numeric(m1$fitted.values))
  expect_equal(my_lm(formula, data)$mse, s_m1$sigma^2)
  expect_equal(as.numeric(my_lm(formula, data)$rsquared),
               as.numeric(s_m1$r.squared))
  expect_equal(as.numeric(my_lm(formula, data)$adj_rsquared),
               as.numeric(s_m1$adj.r.squared))
  expect_equal(as.numeric(my_lm(formula, data)$SE),
               as.numeric(s_m1$coefficients[,2]))
  expect_equal(unname(cbind(my_lm(formula, data)$t,my_lm(formula, data)$p.value)),
               unname(s_m1$coefficients[,c(3,4)]))
  expect_equal(ff, as.numeric(s_m1$fstatistic))
})
