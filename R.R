ForwardMarginalEffect <- R6Class("ForwardMarginalEffect",
  public = list(
    initialize = function(feature, predictor, step.size, ep.method) {
      self$feature = feature
      self$predictor = predictor
      self$step.size = step.size
      self$ep.method = ep.method
      
      self$data.step = private$make.step(self$feature, self$predictor, self$step.size)
      
      self$extrapolation.detector = ExtrapolationDetector$new(data = self$predictor$data$X,
                                                              data.step = self$data.step,
                                                              feature.types = self$predictor$data$feature.types,
                                                              method = self$ep.method)
    },
    feature = NULL,
    predictor = NULL,
    step.size = NULL,
    data.step = NULL,
    ep.method = NULL,
    extrapolation.detector = NULL
  ),
  private = list(
    
    # Function that computes feature values after the step
    make.step = function(feature, predictor, step.size) {
      df = predictor$data$X
      df = as.data.frame(df)
      df[, feature] = df[, feature] + rep(step.size, each = nrow(df))
      as.data.table(df)
    }
  )
)
