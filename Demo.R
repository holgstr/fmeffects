#### Package Dependencies ------------------------------------------------------------------
library("R6")
library("data.table")
library("checkmate")
library("mlr3verse")

#### Packages for Demo Purposes ------------------------------------------------------------------
library("iml")
library("randomForest")

#### Load Package Content  ------------------------------------------------------------------
files = list.files(pattern = "(.R)$")
sapply(files[which(files != "Demo.R")], source)

# Demo ------------------------------------------------------------------
set.seed(123)
data("Boston", package = "MASS")
Boston$chas = as.factor(Boston$chas)




### Example 1 --------------------------------------
forest = randomForest(medv ~ ., data = Boston)
predictor1 = Predictor$new(forest, data = Boston)

# Example categorical step
a = ForwardMarginalEffect$new(feature = c("chas"),
                          predictor = predictor1,
                          step.size = "0",
                          ep.method = "envelope", # atm envelope is only checked for numerical features
                          nlm.intervals = 1)
#a$results

# Example numerical step
b = ForwardMarginalEffect$new(feature = c("rm", "tax"),
                              predictor = predictor1,
                              step.size = c(2, 100),
                              ep.method = "envelope",
                              nlm.intervals = 1)
#b$results




### Example 2 --------------------------------------
task = as_task_regr(Boston, id = "BostonHousing", target = "medv")
learner = lrn("regr.rpart")
learner$train(task)
Boston = as.data.table(Boston)
predictor2= Predictortest$new(model = learner, data = Boston, y = "medv")

# so far only test version of my own predictor class
predictor2$predict(Boston)
predictor2$feature.types
predictor2$feature.names
predictor2$X
