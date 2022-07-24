#------------------------------------------------------------------------
# DEMO: INTERPRETING FORWARD MARGINAL EFFECTS ON BOSTON HOUSING DATA
#------------------------------------------------------------------------

# DATA ------------------------------------------------------------------

require("MASS")
data(Boston, package = "MASS")
Boston$chas = as.factor(Boston$chas)

# TRAIN MODEL -----------------------------------------------------------

# Use an
