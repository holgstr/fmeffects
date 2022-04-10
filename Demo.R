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

#eff <- FeatureEffect$new(predictor, feature = "rm", grid.size = 30, method = "pdp+ice")
#plot(eff)
fme = ForwardMarginalEffect$new(feature = c("age"),
                                predictor = predictor,
                                step.size = c(9),
                                ep.method = "envelope")
fme$fme
summary(fme$fme$fme)

