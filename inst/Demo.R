#####--------------------------------------------------------------------------
## DEMO: INTERPRETING FORWARD MARGINAL EFFECTS ON WASHINGTON BIKESHARE DATA
#####--------------------------------------------------------------------------

##### INSTALLATION ------------------------------------------------------------

set.seed(123)
library(devtools)
install_github("holgstr/fme", force = TRUE)
library(fme)

##### DATA --------------------------------------------------------------------

# Import data from the OpenML database
require(OpenML)
require(farff)
bikes = as.data.table(getOMLDataSet(data.id = 42712)$data)
bikes = bikes[,-c(10,13,14)]
bikes = bikes[hour %inrange% c(7,8)]
bikes = bikes[year == 1]
bikes = bikes[, -c(2)]

#### TRAIN MODEL --------------------------------------------------------------

# Train a random forest with the ranger package, implemented within mlr3:
library(mlr3verse)
task = as_task_regr(x = bikes, id = "bikes", target = "count")
forest = lrn("regr.ranger")$train(task)

#### COMPUTE MARGINAL EFFECTS -------------------------------------------------

# In order to adapt the model to fme, one can use the R6 syntax:
pred = PredictorMLR3$new(model = forest, data = bikes, target = "count")

# Alternatively, one can use the more intuitive wrapper function:
pred = makePredictor(model = forest, data = bikes, target = "count")

# However, computing fMEs effectively requires only the fme() funtion:
?fme

# Compute fME for the cat. feature "weather", with ref. category "rain":
effects = fme(model = forest, data = bikes, target = "count", feature = "weather", step.size = "rain")
plot(effects)
p = plot(effects)
ggsave("weather_rain.pdf", p, units = "cm", width = 14, height = 8.4)
