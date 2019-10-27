context("ridgereg.R")

library(MASS)

test_that("Results are similar to lm.ridge", {
  mod_object = lm.ridge(Petal.Length~Species, data = iris, lambda = 1)
  reg1 = ridgereg(Petal.Length~Species, data = iris, lambda = 1)
  expect_true(mean(abs(mod_object$coef - reg1$beta_hat)) < 0.01)
  
  mod_object = lm.ridge(Petal.Length~Species, data = iris, lambda = 2)
  reg1 = ridgereg(Petal.Length~Species, data = iris, lambda = 2)
  expect_true(mean(abs(mod_object$coef - reg1$beta_hat)) < 0.01)
  
  mod_object = lm.ridge(Petal.Length~Species, data = iris, lambda = 3)
  reg1 = ridgereg(Petal.Length~Species, data = iris, lambda = 3)
  expect_true(mean(abs(mod_object$coef - reg1$beta_hat)) < 0.01)
})

