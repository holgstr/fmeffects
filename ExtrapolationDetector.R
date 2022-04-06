ExtrapolationDetector <- R6Class("ExtrapolationDetector",
  public = list(
    initialize = function(data, data.step, feature.types, method) {
      self$data = data
      self$data.step = data.step
      self$feature.types = feature.types
      self$method = method
      self$extrapolation.points = private$compute.ep(self$data, self$data.step, self$feature.types, self$method)
      self$non.ep.data = data.table::copy(self$data)
      self$non.ep.data.step = data.table::copy(self$data.step)
      if (!is.null(self$extrapolation.points)) {
        self$non.ep.data = self$non.ep.data[-self$extrapolation.points,]
        self$non.ep.data.step = self$non.ep.data.step[-self$extrapolation.points,]
      }
    },
    data = NULL,
    data.step = NULL,
    feature.types = NULL,
    method = NULL,
    extrapolation.points = NULL,
    non.ep.data = NULL,
    non.ep.data.step = NULL
  ),
  private = list(
    
    compute.ep = function(data, data.step, feature.types, method) {
      data = data.table::copy(data)
      data.step = data.table::copy(data.step)
      if (method == "envelope") {
        extrapolation.ids = private$compute.ep.envelope(data, data.step, feature.types)
      } else if (method == "mcec") {
        extrapolation.ids = private$compute.ep.mcec(data, data.step, feature.types)
      } else {
        extrapolation.ids = NULL
      }
      extrapolation.ids
    },
    
    compute.ep.envelope = function(data, data.step, feature.types) {
      # Firstly, check if numerical features in data.step are in [min, max] of data
      names.num = names(feature.types[feature.types == "numerical"])
      data.step.num = data.step[ ,..names.num]
      minmax = rbind(data[, lapply(.SD, FUN = function(x) min(x, na.rm = TRUE)), .SDcols = names.num],
                     data[, lapply(.SD, FUN = function(x) max(x, na.rm = TRUE)), .SDcols = names.num])
      data.step.num$envelope = apply(data.step.num, 1, function(x) !all(unlist(x) >= minmax[1,] & unlist(x) <= minmax[2,]))
      # Secondly, check if categorical variables are ???
      names.cat = names(feature.types[feature.types == "categorical"])
      data.step.cat = data.step[ ,..names.cat]
      data.step.cat$envelope = apply(data.step.cat, 1, function(x) !is.null(unlist(x))) # nonsense function that needs replacement
      # ids of observations in data.step outside of the multivariate envelope of features in data
      extrapolation.ids = which((data.step.num$envelope == TRUE) & (data.step.cat$envelope == TRUE))
    }
  )
)






