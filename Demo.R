#### Package Dependencies ------------------------------------------------------------------
require("R6")
require("data.table")
require("checkmate")
require("mlr3verse")

#### Packages for Demo Purposes ------------------------------------------------------------------
require("iml")
require("randomForest")

#### Load Package Content  ------------------------------------------------------------------
files = list.files(pattern = "(.R)$")
sapply(files[which(files != "Demo.R" & files != "Demo_Partition.R")], source)

# Demo ------------------------------------------------------------------
set.seed(123)
data("Boston", package = "MASS")
Boston$chas = as.factor(Boston$chas)

### Example 1 --------------------------------------
forest = randomForest(medv ~ ., data = Boston)

# Example categorical step
a = FME$new(makePredictor(forest, Boston, "medv"),
                              feature = c("chas"),
                              step.size = "0",
                              ep.method = "envelope", # atm envelope is only checked for numerical features
                              nlm.intervals = 1)

a$compute()
a$results

# Example numerical step, class architecture also allows for method chaining like this:
FME$new(makePredictor(forest, Boston, "medv"),
                          feature = c("rm", "tax"),
                          step.size = c(3, 100),
                          ep.method = "envelope",
                          nlm.intervals = 1)$compute()$results

### Example 2 --------------------------------------
task = as_task_regr(Boston, id = "BostonHousing", target = "medv")
learner = lrn("regr.rpart")$train(task)

# Abstract Predictor Class throws error upon initialization
predictor = Predictor$new(model = learner, data = Boston, target = "medv")

# Predictor for MLR3 models
predictor = PredictorMLR3$new(data = Boston, model = learner, target ="medv")
predictor$predict(Boston)
predictor$feature.types
predictor$feature.names
predictor$X

# Predictor for randomForest models
predictor = PredictorRandomForest$new(data = Boston, model = learner, target ="medv")
predictor$predict(Boston)
predictor$feature.types
predictor$feature.names
predictor$X
