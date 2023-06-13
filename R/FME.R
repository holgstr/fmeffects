#' @title R6 Class representing a forward marginal effect (FME)
#'
#' @description The FME is a forward difference in prediction due to a specified change in feature values.
#' @export
ForwardMarginalEffect = R6::R6Class("ForwardMarginalEffect",
  public = list(
    #' @description
    #' Create a new ForwardMarginalEffect object.
    #' @param predictor `Predictor` object.
    #' @param feature Feature vector.
    #' @param step.size Vector of step sizes.
    #' @param ep.method String specifying extrapolation detection method.
    #' @param compute.nlm Compute NLM with FMEs? Defaults to `FALSE`.
    #' @param nlm.intervals How many intervals for NLM computation. Defaults to `1`.
    #' @return A new `ForwardMarginalEffect` object.
    #' @examples
    #'
    #' # Train a model:
    #'
    #' library(mlr3verse)
    #' library(ranger)
    #' data(bikes, package = "fmeffects")
    #' forest = lrn("regr.ranger")$train(as_task_regr(x = bikes, id = "bikes", target = "count"))
    #'
    #' # Create an `ForwardMarginalEffect` object:
    #' effects = ForwardMarginalEffect$new(makePredictor(forest, bikes, "count"),
    #'                   feature = c("temp", "humidity"),
    #'                   step.size = c(1, 0.01),
    #'                   ep.method = "envelope")
    initialize = function(predictor, feature, step.size, ep.method = "none", compute.nlm = FALSE, nlm.intervals = 1) {

      # Check if feature is unique character vector of length 1 or 2 and matches names in data
      checkmate::assertCharacter(feature, min.len = 1, max.len = 2, unique = TRUE, any.missing = FALSE)
      checkmate::assertSubset(feature, choices = predictor$feature.names)

      # Check if feature.types are numeric when feature is of length 2
      feature.types = predictor$feature.types[which(predictor$feature.names %in% feature)]
      if (length(feature) == 2) {
        checkmate::assertSetEqual(feature.types, y = c("numerical"))
      }

      # Check if step.size corresponds to feature in length, format and range
      if (length(feature) == 2) { # bivariate
        checkmate::assertNumeric(step.size, len = 2)
        range1 = diff(range(predictor$X[, feature, with=FALSE][,1]))
        checkmate::assertNumeric(step.size[1], len = 1, lower = (-range1), upper = range1)
        range2 = diff(range(predictor$X[, feature, with=FALSE][,2]))
        checkmate::assertNumeric(step.size[2], len = 1, lower = (-range2), upper = range2)
        self$step.type = "numerical"
      } else if (feature.types == "numerical"){ # univariate numerical
        range = diff(range(predictor$X[, feature, with=FALSE]))
        checkmate::assertNumeric(step.size, len = 1, lower = (-range), upper = range)
        self$step.type = "numerical"
      } else { # univariate categorical
        checkmate::assertCharacter(step.size, len = 1)
        checkmate::assertTRUE(step.size %in% predictor$X[, feature, with=FALSE][[1]])
        self$step.type = "categorical"
      }

      # Check if ep.method is one of the options provided
      checkmate::assertChoice(ep.method, choices = c("none", "mcec", "envelope"))

      # Check if compute.nlm is TRUE
      checkmate::assertLogical(compute.nlm, len = 1)

      # Check if nlm.intervals is an integer of length 1 and >= 1
      checkmate::assertIntegerish(nlm.intervals, lower = 1, len = 1)


      self$feature = feature
      self$step.size = step.size
      self$ep.method = ep.method
      self$compute.nlm = compute.nlm
      self$nlm.intervals = as.integer(nlm.intervals)
      self$predictor = predictor

    },

    #' @description
    #' Computes results, i.e., FME (and NLMs) for non-extrapolation points, for a `ForwardMarginalEffect` object.
    #' @return A `ForwardMarginalEffect` object with results.
    #' @examples
    #' # Compute results:
    #' effects$compute()
    compute = function() {
      self$data.step = private$makeStep(self$feature, self$predictor, self$step.size, self$step.type)
      self$extrapolation.ids = ExtrapolationDetector$new(data = self$predictor$X,
                                                         data.step = self$data.step,
                                                         feature.types = self$predictor$feature.types,
                                                         method = self$ep.method,
                                                         step.type = self$step.type)$extrapolation.ids
      # Check if there is at least one non-extrapolation point
      checkmate::assertTRUE(length(self$extrapolation.ids) < nrow(self$predictor$X))
      self$results = private$fme(self$feature,
                                 self$predictor,
                                 self$data.step,
                                 self$step.size,
                                 self$step.type,
                                 self$extrapolation.ids,
                                 self$compute.nlm,
                                 self$nlm.intervals)

      # Descriptive statistics for the results
      self$ame = mean(self$results$fme)
      if ("nlm" %in% names(self$results)) {
        self$anlm = round(mean(self$results$nlm, na.rm = TRUE), 2)
      }
      self$computed = TRUE
      invisible(self)

    },
    #' @description
    #' Plots results, i.e., FME (and NLMs) for non-extrapolation points, for an `FME` object.
    #' @param with.nlm Plots NLMs if computed, defaults to `FALSE`.
    #' @param jitter Jitters data. A two-dimensional numeric vector, corresponds to `"width"` and `"height"`. See `?ggplot2::geom_jitter` for details.
    #' Not available if `step.type` is categorical.
    #' Defaults to no jittering, i.e., c(0, 0).
    #' @examples
    #' # Compute results:
    #' effects$plot()
    plot = function(with.nlm = FALSE, jitter = c(0, 0)) {
      if (self$step.type == "categorical") {
        FMEPlotCategorical$new(self$results, self$predictor$X, self$feature, self$step.size)$plot(with.nlm, jitter)
      } else if (length(self$feature) == 1) {
        FMEPlotUnivariate$new(self$results, self$predictor$X, self$feature, self$step.size)$plot(with.nlm, jitter)
      } else {
        FMEPlotBivariate$new(self$results, self$predictor$X, self$feature, self$step.size)$plot(with.nlm, jitter)
      }
    },

    #' @field feature vector of features
    feature = NULL,
    #' @field predictor `Predictor` object
    predictor = NULL,
    #' @field step.size vector of step sizes for features specified by "feature"
    step.size = NULL,
    #' @field data.step the data.table with the data matrix after the step
    data.step = NULL,
    #' @field ep.method string specifying extrapolation detection method
    ep.method = NULL,
    #' @field compute.nlm logical specifying if NLM should be computed
    compute.nlm = TRUE,
    #' @field nlm.intervals number of intervals for computing NLMs
    nlm.intervals = NULL,
    #' @field step.type `"numerical"` or `"categorical"`
    step.type = NULL,
    #' @field extrapolation.ids vector of observation ids classified as extrapolation points
    extrapolation.ids = integer(),
    #' @field results data.table with FMEs and NLMs computed
    results = NULL,
    #' @field ame Average Marginal Effect (AME) of observations in `results`
    ame = NULL,
    #' @field anlm Average Non-linearity Measure (ANLM) of observations in `results`
    anlm = NULL,
    #' @field computed logical specifying if compute() has been run
    computed = FALSE

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
        data.table::setkeyv(df, feature)
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
      data.table::setkeyv(data, "obs.id")
      data = data[!extrapolation.ids]

      # For numerical features:
      if (step.type == "numerical") {
        # Add observation.id as column to data.step
        data.step = data.table::copy(data.step)
        data.table::set(data.step, j = "obs.id", value = 1:nrow(data.step))
        #Exclude extrapolation points in data.step
        data.table::setkeyv(data.step, "obs.id")
        data.step = data.step[!extrapolation.ids]
      # For categorical features:
      } else {
        # Exclude extrapolation points in data.step
        data.step = data.table::copy(data)
        data.table::setkeyv(data.step, feature)
        data.step = data.step[!step.size]
        data.table::set(data.step, j = feature, value = step.size)
        # Exclude observations in data that are not in the reference category
        data.table::setkeyv(data, feature)
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

#' @title Computes FMEs.
#'
#' @description This is a wrapper function for `FME$new(...)$compute()`.
#' It computes forward marginal effects (FMEs) for a specified change in feature values.
#' @param model The (trained) model, with the ability to predict on new data. This must be an `Learner` (`mlr3`) or `train` (`caret`) object.
#' @param data The data used for computing FMEs, must be data.frame or data.table.
#' @param target A string specifying the model's target variable.
#' @param feature A character vector of the names of the feature variables affected by the step.
#' For numerical steps, this must have length 1 or 2.
#' For categorical steps, this must have length 1.
#' @param step.size A numeric vector of the step lengths in the features affected by the step.
#' For numerical steps, this must have length 1 or 2.
#' For categorical steps, this is the name of the reference category.
#' @param ep.method String specifying the method used for extrapolation detection. One of `"none"` or `"envelope"`. Defaults to `"none"`.
#' @param compute.nlm Compute NLMs for FMEs for numerical steps. Defaults to `FALSE`.
#' @param nlm.intervals Number of intervals for computing NLMs. Results in longer computing time but more accurate approximation of NLMs. Defaults to `1`.
#' @return `FME` Object with FMEs computed.
#' @references
#' Scholbeck, C. A., Casalicchio, G., Molnar, C., Bischl, B., & Heumann, C. (2022). Marginal Effects for Non-Linear Prediction Functions.
#' @examples
#' # Train a model:
#'
#' library(mlr3verse)
#' library(ranger)
#' data(bikes, package = "fmeffects")
#' forest = lrn("regr.ranger")$train(as_task_regr(x = bikes, id = "bikes", target = "count"))
#'
#' # Compute FMEs:
#' effects = fme(model = forest, data = bikes, target = "count", feature = "temp",
#'               step.size = 1, ep.method = "envelope")
#'
#' # Analyze results:
#' summary(effects)
#' plot(effects)
#'
#' # Extract results:
#' effects$results
#' @export
fme = function(model, data, target, feature, step.size, ep.method = "none", compute.nlm = FALSE, nlm.intervals = 1) {
  return(ForwardMarginalEffect$new(makePredictor(model, data, target),
          feature = feature,
          step.size = step.size,
          ep.method = ep.method,
          compute.nlm = compute.nlm,
          nlm.intervals = nlm.intervals)$compute())
}
