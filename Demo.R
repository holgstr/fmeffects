##### Package Dependencies ------------------------------------------------------------------
require("R6")
require("data.table")
require("checkmate")
require("mlr3verse")
require("partykit")
require("rpart")

##### Packages for Demo Purposes ------------------------------------------------------------------
require("iml")
require("randomForest")

##### Load Package Content  ------------------------------------------------------------------
files = list.files(pattern = "(.R)$")
sapply(files[which(files != "Demo.R" & files != "Demo_Partition.R")], source)

##### Demo ------------------------------------------------------------------
set.seed(123)
data("Boston", package = "MASS")
Boston$chas = as.factor(Boston$chas)




##### CATEGORICAL STEP EXAMPLE ------------------------------------------------------------------

forest = randomForest(medv ~ ., data = Boston)
a = FME$new(makePredictor(forest, Boston, "medv"),
                              feature = c("chas"),
                              step.size = "0",
                              ep.method = "envelope", # atm envelope is only checked for numerical features
                              nlm.intervals = 1)
a$compute()$results

b = PartitioningCtree$new(a, "partitions", 3)$compute()
plot(b$tree)

c = PartitioningRpart$new(a, "max.cov", 6.5)$compute()
plot(c$tree)



##### NUMERICAL STEP EXAMPLE ------------------------------------------------------------------
### FME without NLMs

d1 = FME$new(makePredictor(forest, Boston, "medv"),
             feature = c("rm", "tax"),
             step.size = c(1, 100),
             ep.method = "envelope",
             compute.nlm = FALSE,
             nlm.intervals = 1)$compute()
d1$results


d = FME$new(makePredictor(forest, Boston, "medv"),
                          feature = c("rm", "tax"),
                          step.size = c(1, 100),
                          ep.method = "envelope",
                          nlm.intervals = 1)$compute()

### Partitioning
## R6
e = PartitioningCtree$new(d, "max.cov", 2)$compute()
e$plot()

f = PartitioningRpart$new(d, "partitions", 3)$compute()
plot(f$tree)

## User-friendly
# with default partitioning setting (ctree)
g = came(d, number.partitions = 7)
plot(g$tree)

# or with custom partitioning settings (works for both rpart and ctree)
h = came(d, number.partitions = 4, tree.control = ctree_control(alpha = 0.01))
h$plot()


