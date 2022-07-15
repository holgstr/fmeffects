# Abstract FMEPlot Class
FMEPlot = R6Class("FMEPlot",
  public = list(

    initialize = function(...) {
      stop(paste(class(self)[1], "is an abstract class that cannot be initialized."))
    },

    feature = NULL,
    step.size = NULL,
    df = NULL

  ),
  private = list(

    initializeSubclass = function(results, data, feature, step.size) {

      # Check if results is a data.table with a minimum of one observation
      assertDataTable(results, min.rows = 1)

      self$feature = feature
      self$step.size = step.size

      results = data.table::copy(results)
      add = data[, .SD, .SDcols = feature]
      add = add[i = results$obs.id]
      results = cbind(results, add)
      self$df = results

    }
  )
)


# Partioning for Ctree from the 'partykit' package
FMEPlotUnivariate = R6Class("FMEPlotUnivariate",

  inherit = FMEPlot,

  public = list(

    initialize = function(results, data, feature, step.size) {
      private$initializeSubclass(results, data, feature, step.size)
    },

    plot = function() {
      plot(self$df$fme, self$df$nlm)
    }

  )
)
