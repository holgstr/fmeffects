ForwardMarginalEffect <- R6Class("ForwardMarginalEffect",
  public = list(
    initialize = function(feature, predictor, step.size, ep.method) {
      
      # Check if feature is unique character vector of length 1 or 2 and matches names in data
      assertCharacter(feature, min.len = 1, max.len = 2, unique = TRUE, any.missing = FALSE)
      assertSubset(feature, choices = predictor$data$feature.names)
      
      # Check if feature.types are numeric when feature is of length 2
      if (length(feature) == 2) {
        feature.types = predictor$data$feature.types[which(predictor$data$feature.names %in% feature)]
        assert_set_equal(feature.types, y = c("numerical"))
      }
      
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
      df = data.table::copy(predictor$data$X)
      for (n_col in 1:length(feature)) {
        colname = feature[n_col]
        data.table::set(df, j = colname, value = df[, ..colname] + step.size[n_col])
      }
      df
    }
  )
)
