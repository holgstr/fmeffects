#--------------------------------------------------------------------------
# DEMO: INTERPRETING FORWARD MARGINAL EFFECTS ON WASHINGTON BIKESHARE DATA
#--------------------------------------------------------------------------

set.seed(123)

# DATA --------------------------------------------------------------------

# Import data from the OpenML database
require(OpenML)
require(farff)
Bikes = as.data.table(getOMLDataSet(data.id = 42712)$data)
Bikes = Bikes[,-c(10,13,14)]
Bikes = Bikes[hour %inrange% c(7,8)]
Bikes = Bikes[year == 1]
Bikes = Bikes[, -c(2)]

# TRAIN MODEL -------------------------------------------------------------

# Use an
task = as_task_regr(x = Bikes, id = "Bikes", target = "count")
forest = lrn("regr.ranger")$train(task)
forest$model

