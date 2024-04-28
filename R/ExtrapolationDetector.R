ExtrapolationDetector = R6::R6Class("ExtrapolationDetector",
  public = list(
    initialize = function(data, data.step, feature.types, method, step.type) {

      self$data = data
      self$data.step = data.step
      self$feature.types = feature.types
      self$method = method
      self$step.type = step.type

      # Identify extrapolation points (for now only for numerical steps)
      if (self$method == "envelope" & self$step.type == "numerical") {
        self$extrapolation.ids = private$computeEP(self$data,
                                                    self$data.step,
                                                    self$feature.types)
      }
    },
    data = NULL,
    data.step = NULL,
    feature.types = NULL,
    method = NULL,
    step.type = NULL,
    extrapolation.ids = integer()
  ),
  private = list(
    computeEP = function(data, data.step, feature.types) {
      data = data.table::copy(data)
      data.step = data.table::copy(data.step)

      # Check if numerical features in data.step are in [min, max] of data
      names.num = names(feature.types[feature.types == "numerical"])
      data.step.num = data.step[ , names.num, with=FALSE]
      minmax = rbind(data[, lapply(.SD, FUN = function(x) min(x, na.rm = TRUE)), .SDcols = names.num],
                     data[, lapply(.SD, FUN = function(x) max(x, na.rm = TRUE)), .SDcols = names.num])
      data.step.num$envelope = apply(data.step.num, 1, function(x) !all(unlist(x) >= minmax[1,] & unlist(x) <= minmax[2,]))

      extrapolation.ids = which((data.step.num$envelope == TRUE))
      return(extrapolation.ids)
    }
  )
)






