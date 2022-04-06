#### Package Dependencies ------------------------------------------------------------------
library("R6")
library("data.table")

#### Packages for Demo Purposes ------------------------------------------------------------------
library("iml")
library("randomForest")

#### Load Package Content  ------------------------------------------------------------------
source("R.R")


# Demo ------------------------------------------------------------------
data("Boston", package = "MASS")
Boston$chas = as.factor(Boston$chas)
forest <- randomForest(medv ~ ., data = Boston)
predictor <- Predictor$new(forest, data = Boston)
#eff <- FeatureEffect$new(mod, feature = "rm", grid.size = 30)
#plot(eff)

fme = ForwardMarginalEffect$new(feature = c("rm", "age"),
                                predictor = predictor,
                                step.size = c(1, 1),
                                ep.method = "envelope")
