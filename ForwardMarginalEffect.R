ForwardMarginalEffect <- R6Class("ForwardMarginalEffect",
  public = list(
    initialize = function(feature, predictor, step.size, ep.method = "none") {
      
      # Check if feature is unique character vector of length 1 or 2 and matches names in data
      assert_character(feature, min.len = 1, max.len = 2, unique = TRUE, any.missing = FALSE)
      assert_subset(feature, choices = predictor$data$feature.names)
      
      # Check if feature.types are numeric when feature is of length 2
      feature.types = predictor$data$feature.types[which(predictor$data$feature.names %in% feature)]
      if (length(feature) == 2) {
        assert_set_equal(feature.types, y = c("numerical"))
      }
      
      # Check if step.size corresponds to feature in length and format
      if (length(feature) == 2) { # bivariate
        assert_numeric(step.size, len = 2)
        self$step.type = "numerical"
      } else if (feature.types == "numerical"){ # univariate numerical 
        assert_numeric(step.size, len = 1)
        self$step.type = "numerical"
      } else { # univariate categorical
        assert_character(step.size, len = 1)
        assert_true(step.size %in% predictor$data$X[,..feature][[1]])
        self$step.type = "categorical"
      }
      
      # Check if ep.method is one of the options provided
      assert_choice(ep.method, choices = c("none", "mcec", "envelope"))
      
      self$feature = feature
      self$predictor = predictor
      self$step.size = step.size
      self$ep.method = ep.method
      
      self$data.step = private$make.step(self$feature, self$predictor, self$step.size, self$step.type)
      
      self$extrapolation.detector = ExtrapolationDetector$new(data = self$predictor$data$X,
                                                              data.step = self$data.step,
                                                              feature.types = self$predictor$data$feature.types,
                                                              method = self$ep.method,
                                                              step.type = self$step.type)
      
      self$fme = private$compute.fme(self$feature,
                                     self$predictor,
                                     self$extrapolation.detector$non.ep.data,
                                     self$extrapolation.detector$non.ep.data.step,
                                     self$step.size,
                                     self$step.type,
                                     self$extrapolation.detector$extrapolation.ids)
    },
    feature = NULL,
    predictor = NULL,
    step.size = NULL,
    data.step = NULL,
    ep.method = NULL,
    extrapolation.detector = NULL,
    fme = NULL,
    step.type = NULL
  ),
  private = list(
    
    # Function that computes feature values after the step
    make.step = function(feature, predictor, step.size, step.type) {
      df = data.table::copy(predictor$data$X)
      if (step.type == "numerical") {
        for (n_col in seq_len(length(feature))) {
          colname = feature[n_col]
          data.table::set(df, j = colname, value = df[, ..colname] + step.size[n_col])
        }
      } else {
        setkeyv(df, feature)
        df = df[!step.size]
        data.table::set(df, j = feature, value = step.size)
      }
      df
    },
    
    # Function that computes the forward marginal effect after extrapolation point detection
    compute.fme = function(feature, predictor, data, data.step, step.size, step.type, extrapolation.ids) {
      df = data.table::copy(predictor$data$X)
      # For categorical features, we can only compare observations which are not in the reference category
      if (step.type == "categorical") {
        setkeyv(df, feature)
        df = df[!step.size]
        setkeyv(data, feature)
        data = data[!step.size]
      }
      y.hat.diff = predictor$predict(data.step) - predictor$predict(data)
      df[!self$extrapolation.detector$extrapolation.ids, fme := y.hat.diff]
      df[]
    }
  )
)
