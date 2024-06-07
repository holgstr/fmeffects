#' @title R6 Class computing Average Marginal Effects (AME) based on Forward Marginal Effects (FME) for a model
#'
#' @description The AME is a simple mean FME and computed w.r.t. a feature variable and a model.
#' @export
AverageMarginalEffects = R6::R6Class("AverageMarginalEffects",
  public = list(
    #' @description
    #' Create a new AME object.
    #' @param model The (trained) model, with the ability to predict on new data. This must be a `train.formula` (`tidymodels`), `Learner` (`mlr3`), `train` (`caret`), `lm` or `glm` object.
    #' @param data The data used for computing AMEs, must be data.frame or data.table.
    #' @param features If not NULL, a named list of the names of the feature variables for which AMEs should be computed, together with the desired step sizes.
    #' For numeric features, the step size must be a single number.
    #' For categorial features, the step size must be a character vector of category names that is a subset of the levels of the factor variable.
    #' @param ep.method String specifying the method used for extrapolation detection. One of `"none"` or `"envelope"`. Defaults to `"none"`.
    #' @return A new `AME` object.
    #' @examples
    #' # Train a model:
    #'
    #' library(mlr3verse)
    #' library(ranger)
    #' set.seed(123)
    #' data(bikes, package = "fmeffects")
    #' task = as_task_regr(x = bikes, id = "bikes", target = "count")
    #' forest = lrn("regr.ranger")$train(task)
    #'
    #' # Compute AMEs for all features:
    #' \dontrun{
    #' overview = AverageMarginalEffects$new(
    #'   model = forest,
    #'   data = bikes)$compute()
    #' summary(overview)
    #'
    #' # Compute AMEs for a subset of features with non-default step.sizes:
    #' overview = AverageMarginalEffects$new(model = forest,
    #'                                       data = bikes,
    #'                                       features = list(humidity = 0.1,
    #'                                                    weather = c("clear", "rain")))$compute()
    #' summary(overview)
    #' }
    initialize = function(model, data, features = NULL, ep.method = "none") {

      # Initialize predictor (this includes the relevant assertions)
      predictor = makePredictor(model = model, data = data)

      features = unlist(features)

      # Check whether feature names in 'features' argument match predictor
      is_feature_allowed = function(feature, allowed_base_features) {
        base_feature = sub("([0-9]+)$", "", feature)
        is_base_feature = base_feature %in% allowed_base_features
        is_numbered = grepl("^[a-zA-Z]+[0-9]+$", feature)
        return(is_base_feature | (is_numbered & is_base_feature))
      }
      allowed_features = sapply(names(features), is_feature_allowed, predictor$feature.names)
      if (!all(allowed_features)) {
        invalid_features = names(features)[!allowed_features]
        stop("AMEs cannot be computed for features that are not contained in the model")
      }


      self$predictor = predictor
      self$features = features
      self$ep.method = ep.method

    },

    #' @description
    #' Computes results, i.e., AMEs including the SD of FMEs, for an `AME` object.
    #' @return An `AME` object with results.
    #' @examples
    #' # Compute results:
    #' \dontrun{
    #' overview$compute()
    #' }
    compute = function() {

      predictor = self$predictor
      features = self$features
      ep.method = self$ep.method

      res = data.frame(matrix(ncol=7,nrow=0))
      # if features is not NULL
      if (!(is.null(features))) {
        for (i in seq_len(length(predictor$feature.names))) {
          feature = predictor$feature.names[i]
          pattern = paste0(feature, "[[:digit:]]{0,2}")
          if (any(grepl(pattern, names(features)))) {
            # compute sth:
            # Numerical Features
            if (predictor$feature.types[i] == "numerical") {
              # Check if step.size is sensible
              if (suppressWarnings(is.na(as.numeric(features[grep(pattern, names(features))])))) {
                stop(paste("The step size for", feature, "must be numeric"))
              }
              step.size = as.numeric(features[grep(pattern, names(features))])
              if (!(range(predictor$X[,..feature])[2] - range(predictor$X[,..feature])[1] >= step.size)) {
                stop(paste("The step size for", feature, "is larger than the range in the data. Please choose smaller step size"))
              }
              fme = ForwardMarginalEffect$new(predictor = predictor,
                                              features = setNames(list(step.size), feature),
                                              ep.method = ep.method)$compute()
              res = rbind(res, c(feature,
                                 step.size,
                                 round(fme$ame, 5),
                                 round(sd(fme$results$fme), 5),
                                 round(quantile(x = fme$results$fme, probs = c(0.25)), 5),
                                 round(quantile(x = fme$results$fme, probs = c(0.75)), 5),
                                 nrow(fme$results)))
            }
            # Categorical Features
            if (predictor$feature.types[i] == "categorical") {
              # Check if step.size is sensible
              categories = features[grep(pattern, names(features))]
              if (!(all(categories %in% unique(as.vector(predictor$X[,..feature])[[1]])))) {
                stop(paste("Not all step.sizes supplied are categories of feature", feature))
              }
              for (j in seq_len(length(categories))) {
                #catname = paste0(feature, ".", categories[j])
                fme = ForwardMarginalEffect$new(predictor = predictor,
                                                features = setNames(list(as.character(categories[j])), feature))$compute()
                featurename = feature

                res = rbind(res, c(featurename,
                                   as.character(categories[j]),
                                   round(fme$ame, 5),
                                   round(sd(fme$results$fme), 5),
                                   round(quantile(x = fme$results$fme, probs = c(0.25)), 5),
                                   round(quantile(x = fme$results$fme, probs = c(0.75)), 5),
                                   nrow(fme$results)))
              }
            }
          }
        }
      } else {
        # if features is NULL
        for (i in seq_len(length(predictor$feature.names))) {
          feature = predictor$feature.names[i]
          if (predictor$feature.types[i] == "numerical") {
            # Check if step.sizes = 1 is sensible (for default)
            if (range(predictor$X[,..feature])[2] - range(predictor$X[,..feature])[1] <= 1) {
              step.size = 0.01
            } else {
              step.size = 1
            }
            fme = ForwardMarginalEffect$new(predictor = predictor,
                                            features = setNames(list(step.size), feature),
                                            ep.method = ep.method)$compute()
            res = rbind(res, c(feature,
                               step.size,
                               round(fme$ame, 4),
                               round(sd(fme$results$fme), 4),
                               round(quantile(x = fme$results$fme, probs = c(0.25)), 4),
                               round(quantile(x = fme$results$fme, probs = c(0.75)), 4),
                               nrow(fme$results)))
          }
          if (predictor$feature.types[i] == "categorical") {
            categories = unique(as.vector(predictor$X[,..feature])[[1]])
            for (j in seq_len(length(categories))) {
              #catname = paste0(feature, ".", categories[j])
              fme = ForwardMarginalEffect$new(predictor = predictor,
                                              features = setNames(list(as.character(categories[j])), feature))$compute()
              featurename = feature

              res = rbind(res, c(featurename,
                                 as.character(categories[j]),
                                 round(fme$ame, 4),
                                 round(sd(fme$results$fme), 4),
                                 round(quantile(x = fme$results$fme, probs = c(0.25)), 4),
                                 round(quantile(x = fme$results$fme, probs = c(0.75)), 4),
                                 nrow(fme$results)))
            }
          }
        }
      }
      names(res) = c("Feature",  "step.size", "AME", "SD", "0.25", "0.75", "n")

      self$results = res
      self$computed = TRUE
      invisible(self)

    },


    #' @field predictor `Predictor` object
    predictor = NULL,
    #' @field features vector of features for which AMEs should be computed
    features = NULL,
    #' @field ep.method string specifying extrapolation detection method
    ep.method = NULL,
    #' @field results data.table with AMEs computed
    results = NULL,
    #' @field computed logical specifying if compute() has been run
    computed = FALSE

  )
)


# User-friendly function

#' @title Computes AMEs for every feature (or a subset of features) of a model.
#'
#' @description This is a wrapper function for `AverageMarginalEffects$new(...)$compute()`.
#' It computes Average Marginal Effects (AME) based on Forward Marginal Effects (FME) for a model. The AME is a simple mean FME and computed w.r.t. a feature variable and a model.
#' @param model The (trained) model, with the ability to predict on new data. This must be a `train.formula` (`tidymodels`), `Learner` (`mlr3`), `train` (`caret`), `lm` or `glm` object.
#' @param data The data used for computing AMEs, must be data.frame or data.table.
#' @param features If not NULL, a named list of the names of the feature variables for which AMEs should be computed, together with the desired step sizes.
#' For numeric features, the step size must be a single number.
#' For categorial features, the step size must be a character vector of category names that is a subset of the levels of the factor variable.
#' @param ep.method String specifying the method used for extrapolation detection. One of `"none"` or `"envelope"`. Defaults to `"none"`.
#' @return An `AverageMarginalEffects` object, with a field `results` containing a list of summary statistics, including
#' * `Feature`: The name of the feature.
#' * `step.size`: The step.size w.r.t. the specified feature.
#' * `AME`: The Average Marginal Effect for a step of length step.size w.r.t. the specified feature.
#' * `SD`: The standard deviation of FMEs for the specified feature and step.size.
#' * `0.25`: The 0.25-quantile of FMEs for the specified feature and step.size.
#' * `0.75`: The 0.75-quantile of FMEs for the specified feature and step.size.
#' * `n`: The number of observations included for the computation of the AME. This can vary for the following reasons:
#' For categorical features, FMEs are only computed for observations where the original category is not the step.size category.
#' For numerical features, FMEs are only computed for observations that are not extrapolation points (if ep.method is set to `"envelope"`).
#' @references
#' Scholbeck, C.A., Casalicchio, G., Molnar, C. et al. Marginal effects for non-linear prediction functions. Data Min Knowl Disc (2024). https://doi.org/10.1007/s10618-023-00993-x
#' @examples
#' # Train a model:
#'
#' library(mlr3verse)
#' library(ranger)
#' data(bikes, package = "fmeffects")
#' set.seed(123)
#' task = as_task_regr(x = bikes, id = "bikes", target = "count")
#' forest = lrn("regr.ranger")$train(task)
#'
#' # Compute AMEs for all features:
#' \dontrun{
#' overview = ame(model = forest, data = bikes)
#' summary(overview)
#'
#' # Compute AMEs for a subset of features with non-default step.sizes:
#' overview = ame(model = forest,
#'                data = bikes,
#'                features = list(humidity = 0.1, weather = c("clear", "rain")))
#' summary(overview)
#'
#' # Extract results:
#' overview$results
#' }
#' @export
ame = function(model, data, features = NULL, ep.method = "none") {
  return(AverageMarginalEffects$new(model, data, features = features, ep.method = ep.method)$compute())
}
