#--------------------------------------------------------------------------
# DEMO: INTERPRETING FORWARD MARGINAL EFFECTS ON WASHINGTON BIKESHARE DATA
#--------------------------------------------------------------------------

# DATA --------------------------------------------------------------------

# Import data from the OpenML database
require(OpenML)
require(farff)
miami = as.data.table(getOMLDataSet(data.id = 42712)$data)
miami = miami[,-c(10,13,15)]
miami = miami[hour %inrange% c(15,17)]
miami = miami[year == 1]
miami = miami[, -c(2)]

# TRAIN MODEL -------------------------------------------------------------

# Use an
task = as_task_regr(miami, id = "miami", target = "registered")
forest = lrn("regr.ranger", num.trees = 400, mtry= 5, importance = "permutation")$train(task)
forest$model

