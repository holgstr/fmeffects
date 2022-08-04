#####--------------------------------------------------------------------------
## DEMO: INTERPRETING FORWARD MARGINAL EFFECTS ON WASHINGTON BIKESHARE DATA
#####--------------------------------------------------------------------------


##### INSTALLATION ------------------------------------------------------------

set.seed(123)
library(devtools)
install_github("holgstr/fme", force = TRUE, upgrade = "never")
library(fme)



##### DATA --------------------------------------------------------------------

# Import data from the OpenML database
require(OpenML)
require(farff)
bikes = as.data.table(getOMLDataSet(data.id = 42712)$data)
bikes$year = as.factor(bikes$year)
bikes = bikes[,-c(10,13,14)]
bikes = bikes[hour == 7]
bikes = bikes[, -c(4)]



#### TRAIN MODEL --------------------------------------------------------------

# Train a random forest with the ranger package, implemented within mlr3:
library(mlr3verse)
task = as_task_regr(x = bikes, id = "bikes", target = "count")
forest = lrn("regr.ranger")$train(task)
forest$model



#### COMPUTE MARGINAL EFFECTS -------------------------------------------------

# In order to adapt the model to fme, one can use the R6 syntax:
pred = PredictorMLR3$new(model = forest, data = bikes, target = "count")

# Alternatively, one can use the more intuitive wrapper function:
pred = makePredictor(model = forest, data = bikes, target = "count")

# However, computing fMEs effectively requires only the fme() funtion:
?fme
# !!! If you have just installed fme in the same RStudio session, this might show "internal server error" !!!
# !!! In this case, restart the session and simply reload fme via 'library(fme)' !!!



### CATEGORICAL FEATURES ------------------------------------------------------

# Compute MEs for the cat. feature "weather", with ref. category "rain":
effects = fme(model = forest,
              data = bikes,
              target = "count",
              feature = "hour",
              step.size = "rain")

# We have created an object of class 'FME':
class(effects)

# We can produce a summary to inspect the object:
summary(effects)

# We can extract the AME with:
effects$ame

# Or we can inspect MEs of individual observations:
head(effects$results)

# Finally, we visualize the MEs with plot():
plot(effects)



### NUMERICAL FEATURES --------------------------------------------------------

# Compute fMEs for the num. feature "temp", with step size 3
# This corresponds to a temperature increase of 3 degrees celsius
# Caveat: We compute NLMs. This might take several minutes depending on your computer:
effects2 = fme(model = forest,
               data = bikes,
               target = "count",
               feature = "temp",
               step.size = 3,
               ep.method = "envelope",
               compute.nlm = TRUE)

# We can extract the AME and the ANLM with:
effects2$ame
effects2$anlm

# Finally, we visualize the MEs with plot()
# We include NLMs and jitter the points (see ?geom_jitter) to avoid overlapping:
plot(effects2, with.nlm = TRUE, jitter = c(0.2, 0))

# Compute fMEs for the num. features "temp" and "humidity", with step sizes -2, -0.1
# This corresponds to a temperature decrease of 2 degrees celsius and a decrease in humidity by 10 percentage points:
effects3 = fme(model = forest,
               data = bikes,
               target = "count",
               feature = c("temp", "humidity"),
               step.size = c(-2, -0.1),
               ep.method = "envelope",
               compute.nlm = TRUE)

# Finally, we visualize the MEs with plot()
# We include NLMs and jitter the points (see ?geom_jitter) to avoid overlapping:
plot(effects3, with.nlm = TRUE, jitter = c(0.02, 0.02))



### SEMI-GLOBAL INTERPRETATIONS -----------------------------------------------

# We can identify feature subspaces with more homogeneous effects with the came() function
# Let us assume we want to find three partitions:
subspaces = came(effects = effects3, number.partitions = 3)

# We can produce a summary to inspect the object:
summary(subspaces)

# Finally, we visualize the partitioning with plot()
plot(subspaces)

# Let us assume we want to find a partitioning for 'effects',
# i.e., the categorical feature change of 'weather' to 'rain'
subspaces2 = came(effects = effects,
                  max.cov = 3,
                  rp.method = "rpart",
                  tree.control = rpart.control(minsplit = 100, cp= 0.1))

# We visualize the partitioning with plot()
plot(subspaces2)

# In fme, we use assertions to ensure a user make sensible inputs:
came(effects = effects2, max.cov = 1.5, number.partitions = 8)

# A partitioning with 8 partitions can be created and plotted with:
plot(came(effects = effects2, number.partitions = 8))
