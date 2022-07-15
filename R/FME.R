FME = R6Class("FME",
  public = list(

    initialize = function(predictor, feature, step.size, ep.method = "none", compute.nlm = FALSE, nlm.intervals = 1) {

      # Check if feature is unique character vector of length 1 or 2 and matches names in data
      assertCharacter(feature, min.len = 1, max.len = 2, unique = TRUE, any.missing = FALSE)
      assertSubset(feature, choices = predictor$feature.names)

      # Check if feature.types are numeric when feature is of length 2
      feature.types = predictor$feature.types[which(predictor$feature.names %in% feature)]
      if (length(feature) == 2) {
        assertSetEqual(feature.types, y = c("numerical"))
      }

      # Check if step.size corresponds to feature in length, format and range
      if (length(feature) == 2) { # bivariate
        assertNumeric(step.size, len = 2)
        range1 = diff(range(predictor$X[, feature, with=FALSE][,1]))
        assertNumeric(step.size[1], len = 1, lower = (-range1), upper = range1)
        range2 = diff(range(predictor$X[, feature, with=FALSE][,2]))
        assertNumeric(step.size[2], len = 1, lower = (-range2), upper = range2)
        self$step.type = "numerical"
      } else if (feature.types == "numerical"){ # univariate numerical
        range = diff(range(predictor$X[, feature, with=FALSE]))
        assertNumeric(step.size, len = 1, lower = (-range), upper = range)
        self$step.type = "numerical"
      } else { # univariate categorical
        assertCharacter(step.size, len = 1)
        assertTRUE(step.size %in% predictor$X[, feature, with=FALSE][[1]])
        self$step.type = "categorical"
      }

      # Check if ep.method is one of the options provided
      assertChoice(ep.method, choices = c("none", "mcec", "envelope"))

      # Check if compute.nlm is TRUE
      assertLogical(compute.nlm, len = 1)

      # Check if nlm.intervals is an integer of length 1 and >= 1
      assertIntegerish(nlm.intervals, lower = 1, len = 1)

      self$feature = feature
      self$step.size = step.size
      self$ep.method = ep.method
      self$compute.nlm = compute.nlm
      self$nlm.intervals = as.integer(nlm.intervals)
      self$predictor = predictor

    },

    compute = function() {
      self$data.step = private$makeStep(self$feature, self$predictor, self$step.size, self$step.type)
      self$extrapolation.ids = ExtrapolationDetector$new(data = self$predictor$X,
                                                         data.step = self$data.step,
                                                         feature.types = self$predictor$feature.types,
                                                         method = self$ep.method,
                                                         step.type = self$step.type)$extrapolation.ids
      # Check if there is at least one non-extrapolation point
      assertTRUE(length(self$extrapolation.ids) < nrow(self$predictor$X))
      self$results = private$fme(self$feature,
                                 self$predictor,
                                 self$data.step,
                                 self$step.size,
                                 self$step.type,
                                 self$extrapolation.ids,
                                 self$compute.nlm,
                                 self$nlm.intervals)

      invisible(self)

    },

    feature = NULL,
    predictor = NULL,
    step.size = NULL,
    data.step = NULL,
    ep.method = NULL,
    compute.nlm = TRUE,
    nlm.intervals = NULL,
    step.type = NULL,
    extrapolation.ids = integer(),
    results = NULL
  ),
  private = list(

    makeStep = function(feature, predictor, step.size, step.type) {
      df = data.table::copy(predictor$X)
      if (step.type == "numerical") {
        for (n_col in seq_len(length(feature))) {
          colname = feature[n_col]
          data.table::set(df, j = colname, value = df[, colname, with=FALSE] + step.size[n_col])
        }
      } else {
        setkeyv(df, feature)
        df = df[!step.size]
        data.table::set(df, j = feature, value = step.size)
      }
      df
    },

    fme = function(feature,
                   predictor,
                   data.step,
                   step.size,
                   step.type,
                   extrapolation.ids,
                   compute.nlm,
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
        #Exclude extrapolation points in data.step
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

      # Compute fMEs:
      y.hat.diff = predictor$predict(data.step) - predictor$predict(data)
      data[, fme := y.hat.diff]

      # For numerical features, compute NLMs:
      if (step.type == "numerical" & compute.nlm == TRUE) {
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
        return(data[, .(obs.id, fme, nlm)])
      } else {
        return(data[, .(obs.id, fme)])
      }
    }
  )
)


# User-friendly function


# User-friendly function
fme = function(model, data, target, feature, step.size, ep.method = "none", compute.nlm = TRUE, nlm.intervals = 1) {
  return(FME$new(makePredictor(model, data, target),
          feature = feature,
          step.size = step.size,
          ep.method = ep.method,
          compute.nlm = compute.nlm,
          nlm.intervals = nlm.intervals)$compute())
}
