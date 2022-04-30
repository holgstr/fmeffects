#### Package Dependencies ------------------------------------------------------------------
library("R6")
library("data.table")
library("checkmate")

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
forest = randomForest(medv ~ ., data = Boston)
predictor = Predictor$new(forest, data = Boston)

#eff <- FeatureEffect$new(predictor, feature = "rm", grid.size = 30, method = "pdp+ice")
#plot(eff)

# Example categorical step
a = ForwardMarginalEffect$new(feature = c("chas"),
                          predictor = predictor,
                          step.size = "0",
                          ep.method = "envelope", # atm envelope is only checked for numerical features
                          nlm.intervals = 1)
#a$results

# Example numerical step
b = ForwardMarginalEffect$new(feature = c("ptratio", "tax"),
                              predictor = predictor,
                              step.size = c(3, 100),
                              ep.method = "envelope",
                              nlm.intervals = 1)
b$results

