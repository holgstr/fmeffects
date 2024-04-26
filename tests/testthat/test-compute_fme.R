testthat::test_that("FME computation correct", {

  library(mlr3verse)
  library(rpart)

  n = 100
  x = runif(n, -5, 5)
  y = x^2
  df = data.frame(x, y)
  tree = lrn("regr.rpart")$train(as_task_regr(x = df, id = "df", target = "y"))

  step = 2
  df.forward = df
  df.forward$x = df.forward$x + step

  fme.manual = predict(tree, df.forward) - predict(tree, df)

  fme.results = fme(model = tree,
                    data = df,
                    features = list("x" = 2),
                    ep.method = "none")

  testthat::expect_equal(fme.results$results$fme, fme.manual)
})
