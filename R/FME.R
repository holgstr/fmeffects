#' @title R6 Class representing a forward marginal effect (FME)
#'
#' @description The FME is a forward difference in prediction due to a specified change in feature values.
#' @export
ForwardMarginalEffect = R6::R6Class("ForwardMarginalEffect",
  public = list(
    #' @description
    #' Create a new ForwardMarginalEffect object.
    #' @param predictor `Predictor` object.
    #' @param features A named list with the feature name(s) and step size(s).
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
    #' forest = lrn("regr.ranger")$train(as_task_regr(x = bikes, target = "count"))
    #'
    #' # Create an `ForwardMarginalEffect` object:
    #' effects = ForwardMarginalEffect$new(makePredictor(forest, bikes),
    #'                   features = list("temp" = 1, "humidity" = 0.01),
    #'                   ep.method = "envelope")
    initialize = function(predictor, features, ep.method = "none", compute.nlm = FALSE, nlm.intervals = 1) {

      feature = names(features)
      step.size = unlist(features, use.names = FALSE)

      if (!checkmate::test_character(feature, min.len = 1, max.len = length(predictor$feature.names), unique = TRUE, any.missing = FALSE)) {
        cli::cli_abort(paste("Number of features in {.arg features} must be between 1 and"), length(predictor$feature.names), ".")
      }
      if (!checkmate::test_subset(feature, choices = predictor$feature.names)) {
        cli::cli_abort("{.arg features} must correspond to features in the data.")
      }

      feature.types = predictor$feature.types[which(predictor$feature.names %in% feature)]
      if (checkmate::test_set_equal(feature.types, y = "numerical")) {
        self$step.type = "numerical"
      } else if (checkmate::test_set_equal(feature.types, y = "categorical")) {
        self$step.type = "categorical"
      } else {
        cli::cli_abort("{.arg features} cannot contain both numeric and categorical features.")
      }
      if (self$step.type == "numerical") {
        if(!checkmate::test_numeric(step.size, min.len = 1)) {
          cli::cli_abort("{.arg features} must have numeric step lengths for numeric features.")
        }
        range_check = function(feature_number) {
          range = diff(range(predictor$X[, feature, with = FALSE][, ..feature_number]))
          if (!checkmate::test_numeric(step.size[feature_number], len = 1, lower = (-range + 1e-10), upper = (range - 1e-10))) {
            cli::cli_abort(paste("Feature", feature[feature_number], "must have numeric step length between", (-range + 1e-10), "and", (range - 1e-10), "."))
          }
        }
        lapply(X = seq_len(length(feature)), FUN = function(x) {range_check(x)})
      } else { # step.type categorical
        category_check = function(feature_number) {
          if (!checkmate::test_string(step.size[feature_number])) {
            cli::cli_abort(paste("The reference category of feature", feature[feature_number], "must be specified with a string."))
          }
          levels = levels(predictor$X[, feature[feature_number], with=FALSE][[1]])
          if (!checkmate::test_choice(step.size[feature_number], levels)) {
            cli::cli_abort(paste("The reference category of feature", feature[feature_number], "must be one of its levels, e.g.:", paste0(levels[seq_len(min(length(levels), 5))], collapse = ", "), "."))
          }
        }
        lapply(X = seq_len(length(feature)), FUN = function(x) {category_check(x)})
      }

      # Check if ep.method is one of the options provided
      checkmate::assertChoice(ep.method, choices = c("none", "envelope"))

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
        self$anlm = round(mean(self$results$nlm, na.rm = TRUE), 4)
      }
      self$computed = TRUE
      invisible(self)

    },
    #' @description
    #' Plots results, i.e., FME (and NLMs) for non-extrapolation points, for an `FME` object.
    #' @param with.nlm Plots NLMs if computed, defaults to `FALSE`.
    #' @param bins Numeric vector giving number of bins in both vertical and horizontal directions.  Applies only to univariate or bivariate numeric effects.
    #'   See [ggplot2::stat_summary_hex()] for details.
    #' @param binwidth Numeric vector giving bin width in both vertical and horizontal directions. Overrides bins if both set. Applies only to univariate or bivariate numeric effects.
    #'   See [ggplot2::stat_summary_hex()] for details.
    #' @examples
    #' # Compute results:
    #' effects$plot()
    plot = function(with.nlm = FALSE, bins = 40, binwidth = NULL) {
      if (self$step.type == "categorical") {
        FMEPlotCategorical$new(self$results, self$predictor$X, self$feature, self$step.size)$plot(with.nlm)
      } else if (length(self$feature) == 1) {
        FMEPlotUnivariate$new(self$results, self$predictor$X, self$feature, self$step.size)$plot(with.nlm, bins, binwidth)
      } else if (length(self$feature) == 2){
        FMEPlotBivariate$new(self$results, self$predictor$X, self$feature, self$step.size)$plot(with.nlm, bins, binwidth)
      } else if (length(self$feature) >= 3){
      FMEPlotHigherOrder$new(self$results, self$predictor$X, self$feature, self$step.size)$plot(with.nlm)
      } else {
        stop("Cannot plot effects for more than two numerical features.")
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
        for (n_col in seq_len(length(feature))) {
          data.table::setkeyv(df, feature[n_col])
          df = df[!step.size[n_col]]
          data.table::set(df, j = feature[n_col], value = step.size[n_col])
        }
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
        for (n_col in seq_len(length(feature))) {
          data.table::setkeyv(data.step, feature[n_col])
          data.step = data.step[!step.size[n_col]]
          data.table::set(data.step, j = feature[n_col], value = step.size[n_col])
          # Exclude observations in data that are not in the reference category
          data.table::setkeyv(data, feature[n_col])
          data = data[!step.size[n_col]]
        }
      }

      # Compute fMEs:
      y.hat.diff = predictor$predict(data.step) - predictor$predict(data)
      data[, fme := y.hat.diff]

      # For numerical features, compute NLMs:
      if (step.type == "numerical" & compute.nlm == TRUE) {

        # Exclude observations with fME = 0 from loop:
        ids = setdiff(1:nrow(data), which(data$fme == 0))

        nlm_id = function(row_id) {
          NonLinearityMeasure$new(predictor, data[row_id, ], feature, step.size, nlm.intervals)$nlm
        }
        #nlm_values = sapply(ids, nlm_id)

        oplan <- future::plan(future::multisession,
                              workers = parallelly::availableCores(omit = 2))
        on.exit(future::plan(oplan), add = TRUE)

        # Ensure parallel-safe RNG
        options(future.rng.onMisuse = "ignore", future.seed = TRUE)

        pb <- utils::txtProgressBar(min = 0, max = nrow(data), style = 3)

        nlm_values <- furrr::future_map_dbl(ids, ~ {
          furrr::furrr_options(
            globals = list(predictor = predictor,
                           data = data,
                           feature = feature,
                           step.size = step.size,
                           nlm.intervals = nlm.intervals),
            packages = c("fmeffects")
          )
          utils::setTxtProgressBar(pb, .x)
          nlm_id(.x)
        })
        close(pb)

        data.table::set(data, i = ids, j = "nlm", value = nlm_values)
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
#' @param model The (trained) model, with the ability to predict on new data. This must be a `train.formula` (`tidymodels`), `Learner` (`mlr3`), `train` (`caret`), `lm` or `glm` object.
#' @param data The data used for computing FMEs, must be data.frame or data.table.
#' @param features A named list with the feature name(s) and step size(s). The list names should correspond to the names of the feature variables affected by the step.
#' The list must exclusively contain either numeric or categorical features, but not a combination of both.
#' Numeric features must have a number as step size, categorical features the name of the reference category.
#' @param ep.method String specifying the method used for extrapolation detection. One of `"none"` or `"envelope"`. Defaults to `"none"`.
#' @param compute.nlm Compute NLMs for FMEs for numerical steps. Defaults to `FALSE`.
#' @param nlm.intervals Number of intervals for computing NLMs. Results in longer computing time but more accurate approximation of NLMs. Defaults to `1`.
#' @return `ForwardsMarginalEffect` object with the following fields:
#' * `ame` average marginal effect (AME).
#' * `anlm` average non-linearity measure (NLM).
#' * `extrapolation.ids` observations that have been identified as extrapolation points and not included in the analysis.
#' * `data.step`, a `data.table` of the feature matrix after the step has been applied.
#' * `results`, a `data.table` of the individual FMEs (and NLMs, if applicable) for all observations that are not extrapolation points.
#' @details
#' If one or more numeric features are passed to the `features` argument, FMEs are computed as \deqn{FME_{x, h_{S}} = f(x + h_{S}, x_{-S}) - f(x)} where \eqn{h_{S}} is the step size vector and \eqn{x_{-S}} the other features.
#' If one or more categorical features are passed to `features`, \deqn{FME_{x, c_{J}} = f(c_{J}, x_{-J}) - f(x)} where \eqn{c_{J}} is the set of selected reference categories in `features` and \eqn{x_{-J}} the other features.
#' @references
#' Scholbeck, C.A., Casalicchio, G., Molnar, C. et al. Marginal effects for non-linear prediction functions. Data Min Knowl Disc (2024). https://doi.org/10.1007/s10618-023-00993-x
#' @examples
#' # Train a model:
#'
#' library(mlr3verse)
#' library(ranger)
#' data(bikes, package = "fmeffects")
#' forest = lrn("regr.ranger")$train(as_task_regr(x = bikes, target = "count"))
#'
#' # Compute FMEs for a numerical feature:
#' effects = fme(model = forest, data = bikes, features = list("temp" = 1), ep.method = "envelope")
#'
#' # Analyze results:
#' summary(effects)
#' plot(effects)
#'
#' # Extract results:
#' effects$results
#'
#' # Compute the AME for a categorial feature:
#' fme(model = forest, data = bikes, features = list("weather" = "rain"))$ame
#' @export
fme = function(model, data, features, ep.method = "none", compute.nlm = FALSE, nlm.intervals = 1) {
  return(ForwardMarginalEffect$new(makePredictor(model, data),
          features = features,
          ep.method = ep.method,
          compute.nlm = compute.nlm,
          nlm.intervals = nlm.intervals)$compute())
}
