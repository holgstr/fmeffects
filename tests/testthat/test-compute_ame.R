# Train model:
library(mlr3verse)
library(ranger)
data(bikes, package = "fmeffects")
set.seed(123)
task = as_task_regr(x = bikes, target = "count")
forest = lrn("regr.ranger")$train(task)

testthat::test_that("FME computation correct for univariate numeric vignette example", {

  ame = fme(model = forest,
            data = bikes,
            features = list(temp = 1),
            ep.method = "envelope")$ame

  testthat::expect_equal(ame, 58.2, tolerance = 1)
})

testthat::test_that("FME computation correct for multivariate vignette example", {

  ame = fme(model = forest,
            data = bikes,
            features = list(temp = -3, humidity = -0.1),
            ep.method = "envelope")$ame

  testthat::expect_equal(ame, -120, tolerance = 1)
})

testthat::test_that("FME computation correct for categorical vignette example", {

  ame = fme(model = forest,
            data = bikes,
            features = list(weather = "rain"))$ame

  testthat::expect_equal(ame, -736, tolerance = 1)
})

testthat::test_that("FME computation correct for categorical interactions vignette example", {

  ame = fme(model = forest,
            data = bikes,
            features = list(weather = "clear", workingday = "no"))$ame

  testthat::expect_equal(ame, 332, tolerance = 1)
})
