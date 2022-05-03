ForwardMarginalEffect = R6Class("ForwardMarginalEffect",
  public = list(
    initialize = function(feature, predictor, step.size, ep.method = "none", nlm.intervals = 1) {
      
      # Check if feature is unique character vector of length 1 or 2 and matches names in data
      assert_character(feature, min.len = 1, max.len = 2, unique = TRUE, any.missing = FALSE)
      assert_subset(feature, choices = predictor$feature.names)
      
      # Check if feature.types are numeric when feature is of length 2
      feature.types = predictor$feature.types[which(predictor$feature.names %in% feature)]
      if (length(feature) == 2) {
        assert_set_equal(feature.types, y = c("numerical"))
      }
      
      # Check if step.size corresponds to feature in length, format and range
      if (length(feature) == 2) { # bivariate
        assert_numeric(step.size, len = 2)
        range1 = diff(range(predictor$X[,..feature][,1]))
        assert_numeric(step.size[1], len = 1, lower = (-range1), upper = range1)
        range2 = diff(range(predictor$X[,..feature][,2]))
        assert_numeric(step.size[2], len = 1, lower = (-range2), upper = range2)
        self$step.type = "numerical"
      } else if (feature.types == "numerical"){ # univariate numerical 
        range = diff(range(predictor$X[,..feature]))
        assert_numeric(step.size, len = 1, lower = (-range), upper = range)
        self$step.type = "numerical"
      } else { # univariate categorical
        assert_character(step.size, len = 1)
        assert_true(step.size %in% predictor$X[,..feature][[1]])
        self$step.type = "categorical"
      }
      
      # Check if ep.method is one of the options provided
      assert_choice(ep.method, choices = c("none", "mcec", "envelope"))
      
      # Check if nlm.intervals is an integer of length 1 and >= 1
      assert_integerish(nlm.intervals, lower = 1, len = 1)
      
      self$feature = feature
      self$predictor = predictor
      self$step.size = step.size
      self$ep.method = ep.method
      self$nlm.intervals = as.integer(nlm.intervals)
      
      self$data.step = private$make.step(self$feature, self$predictor, self$step.size, self$step.type)
      
      self$extrapolation.ids = ExtrapolationDetector$new(data = self$predictor$X,
                                                         data.step = self$data.step,
                                                         feature.types = self$predictor$feature.types,
                                                         method = self$ep.method,
                                                         step.type = self$step.type)$extrapolation.ids
      
      # Check if there is at least one non-extrapolation point
      assert_true(length(self$extrapolation.ids) < nrow(predictor$X))
      
      self$results = private$compute.fme.nlm(self$feature,
                                     self$predictor,
                                     self$data.step,
                                     self$step.size,
                                     self$step.type,
                                     self$extrapolation.ids,
                                     self$nlm.intervals)
    },
    feature = NULL,
    predictor = NULL,
    step.size = NULL,
    data.step = NULL,
    ep.method = NULL,
    nlm.intervals = NULL,
    step.type = NULL,
    extrapolation.ids = integer(),
    results = NULL
  ),
  private = list(
    
    # Function that computes feature values after the step
    make.step = function(feature, predictor, step.size, step.type) {
      df = data.table::copy(predictor$X)
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
    
    # Function that computes the fME (and NLM) after extrapolation point detection
    compute.fme.nlm = function(feature,
                               predictor,
                               data.step,
                               step.size,
                               step.type,
                               extrapolation.ids,
                               nlm.intervals) {
      
      # Add observation.id as column to data
      data = data.table::copy(predictor$X)
      data.table::set(data, j = "obs.id", value = 1:nrow(data))
      
      # Exclude extrapolation points
      setkeyv(data, "obs.id")
      data = data[!extrapolation.ids]
      
      # For numerical features:
      if (step.type == "numerical") {
        # Add observation.id as column to data.step
        data.step = data.table::copy(data.step)
        data.table::set(data.step, j = "obs.id", value = 1:nrow(data.step))
        #E xclude extrapolation points in data.step
        setkeyv(data.step, "obs.id")
        data.step = data.step[!extrapolation.ids]
      # For categorical features:
      } else {   
        # Exclude extrapolation points in data.step
        data.step = data.table::copy(data)
        setkeyv(data.step, feature)
        data.step = data.step[!step.size]
        data.table::set(data.step, j = feature, value = step.size)
        # Exclude observations in data that are not in the reference category
        setkeyv(data, feature)
        data = data[!step.size]
      }

      # Compute fMEs, alternative 1:
      y.hat.diff = predictor$predict(data.step) - predictor$predict(data)
      data[, fme := y.hat.diff]
      
      # Compute fMEs, alternative 2:
      #non.extrapolation.ids = setdiff(1:nrow(df), extrapolation.ids)
      #for (n_row in seq_len(length(non.extrapolation.ids))) {
        #data.table::set(df,
                        #i = non.extrapolation.ids[n_row],
                        #j = "fme",
                        #value = as.numeric(predictor$predict(data.step[n_row,]) - predictor$predict(data[n_row,])))
      #}
      
      # For numerical features, compute NLMs:
      if (step.type == "numerical") {
        # Exclude observations with fME = 0 from loop:
        ids = setdiff(1:nrow(data), which(data$fme == 0))
        for (n_row in seq_len(length(ids))) {
          data.table::set(data,
                          i = ids[n_row],
                          j = "nlm",
                          value = NonLinearityMeasure$new(predictor,
                                                          data[ids[n_row],],
                                                          feature,
                                                          step.size,
                                                          nlm.intervals)$nlm)
        }
        data[, .(obs.id, fme, nlm)]
      } else {
        data[, .(obs.id, fme)]
      }
    }
  )
)
