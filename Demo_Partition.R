#### Package Dependencies ------------------------------------------------------------------
require("R6")
require("data.table")
require("checkmate")
require("mlr3verse")
require("partykit")
require("rpart")

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

a = FME$new(makePredictor(forest, Boston, "medv"),
                          feature = c("rm", "tax"),
                          step.size = c(1, 100),
                          ep.method = "envelope",
                          nlm.intervals = 1)$compute()

# tree with exactly 5 partitions
b = PartitioningCtree$new(a, "partitions", 5)$compute()
plot(b$tree)

# the smallest tree that satisfies cov < max.cov in every partition
c = PartitioningRpart$new(a, "max.cov", 0.8)$compute()
plot(c$tree)
