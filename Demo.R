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
data("Boston", package = "MASS")
Boston$chas = as.factor(Boston$chas)
forest = randomForest(medv ~ ., data = Boston)
predictor = Predictor$new(forest, data = Boston)
#observation = data.table::copy(predictor$data$X[2,])

#eff <- FeatureEffect$new(predictor, feature = "rm", grid.size = 30, method = "pdp+ice")
#plot(eff)

a = ForwardMarginalEffect$new(feature = c("age"),
                          predictor = predictor,
                          step.size = c(6),
                          ep.method = "envelope")
NonLinearityMeasure$new(predictor, predictor$data$X[3:32], feature, step.size, nlm.intervals = 1)$nlm

