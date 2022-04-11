NonLinearityMeasure <- R6Class("NonLinearityMeasure",
  public = list(
    initialize = function(predictor, data, feature, step.size, nlm.intervals) {
    
      self$predictor = predictor
      self$data = data
      self$feature = feature
      self$step.size = step.size
      self$nlm.intervals = nlm.intervals
      self$nlm = private$nlm.compute(self$predictor,
                                     self$data[1,],
                                     self$feature,
                                     self$step.size,
                                     self$nlm.intervals)
      
    },
    predictor = NULL,
    data = NULL,
    feature = NULL,
    step.size = NULL,
    nlm.intervals = NULL,
    nlm = NULL
  ),
  private = list(
    
    nlm.compute = function(predictor, observation, feature, step.size, subintervals) {
      ## Helper function for the path at t
      path.t = function(observation, feature, step.size, t) {
        observation = data.table::copy(observation)
        for (n_col in seq_len(length(feature))) {
          colname = feature[n_col]
          data.table::set(observation, j = colname, value = observation[, ..colname] + t * step.size[n_col])
        }
        return(observation)
      }
      ## Helper function for Simpson's 3/8 Rule (Composite)
      simpson = function(f, subintervals) {
        s = subintervals
        integrals = NULL
        for (i in seq_len(s)) {
          m = (i-1)/s
          integrals[i] = 1/8/s * (f(0/s + m) + 3 * f(1/3/s + m) + 3 * f(2/3/s + m) + f(1/s + m))
        }
        return(sum(integrals))
      }
      # Deviation Predictor and Secant
      f1 = function(t) {
        # Compute Predictor at t
        observation.t = path.t(observation, feature, step.size, t)
        pred = predictor$predict(observation.t)
        # Compute Secant at t
        secant.start = predictor$predict(observation)
        observation = data.table::copy(observation)
        for (n_col in seq_len(length(feature))) {
          colname = feature[n_col]
          data.table::set(observation, j = colname, value = observation[, ..colname] + step.size[n_col])
        }
        secant.step = predictor$predict(observation)
        secant = as.numeric(secant.start) + (t * as.numeric(secant.step - secant.start))
        # Compute Deviation
        return(as.numeric((pred - secant)^2))
      }
      # Deviation Predictor and Mean Prediction
      f2 = function(t) {
        # Compute Predictor at t
        observation.t = path.t(observation, feature, step.size, t)
        pred = predictor$predict(observation.t)
        # Compute Mean Prediction
        prediction.s = function(s) {
          observation.t = path.t(observation, feature, step.size, t = s)
          pred = as.numeric(predictor$predict(observation.t))
          return(pred)
        }
        mean.pred = simpson(prediction.s, subintervals)
        # Compute Deviation
        return(as.numeric((pred - mean.pred)^2))
      }
      # Approximate with Simpson's Rule
      integral1 = simpson(f1, subintervals)
      integral2 = simpson(f2, subintervals)
      
      return(1 - (integral1 / integral2))
    }
  )
)