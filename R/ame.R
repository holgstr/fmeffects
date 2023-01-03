#' @title Computes AMEs for all (or a subset of the) features of a model.
#'
#' @description This function computes Average Marginal Effects (AME) for all or only a number of features of a model.
#' In principle, `fme()` is applied to all selected features, and the AME extracted together with its quantiles.
#' For numerical features, the default step.size is 1, or 0.01 if the range of the feature is smaller than 1.
#' The default step.sizes can be overwritten using the `features` argument.
#' @param model The (trained) model, with the ability to predict on new data. This must be an `LearnerRegr` (`mlr3`) or `train` (`caret`) object.
#' @param data The data used for computing FMEs, must be data.frame or data.table.
#' @param target A string specifying the target variable.
#' @param features A named character vector of the names of the feature variables for which AMEs should be computed, together with the desired step sizes.
#' @param ep.method String specifying the method used for extrapolation detection. One of `"none"` or `"envelope"`. Defaults to `"none"`.
#' @return A list of summary statistics, including
#' * `AME`: The Average Marginal Effect for a step of length step.size w.r.t. the specified feature.
#' * `0.25`: The 0.25-quantile of FMEs for the specified feature and step.size.
#' * `0.75`: The 0.75-quantile of FMEs for the specified feature and step.size.
#' * `n`: The number of observations included for the computation of the AME. This can vary for the following reasons:
#' For categorical features, FMEs are only computed for observations where the original category is not the step.size category.
#' For numerical features, FMEs are only computed for observations that are not extrapolation points (if ep.method is set to `"envelope"`).
#' @references
#' Scholbeck, C. A., Casalicchio, G., Molnar, C., Bischl, B., & Heumann, C. (2022). Marginal Effects for Non-Linear Prediction Functions.
#' @examples
#' # Train a model:
#'
#' library(mlr3verse)
#' data(bikes, package = "fme")
#' forest = lrn("regr.ranger")$train(as_task_regr(x = bikes, id = "bikes", target = "count"))
#'
#' # Compute AMEs for all features:
#' ame(model = forest, data = bikes, target = "count")
#'
#' # Compute AMEs for a subset of features with non-default step.sizes:
#' ame(model = forest, data = bikes, target = "count", features = c(humidity = 0.1, weather = c("clear", "rain")))
#'
#' @export
ame = function(model, data, target, features = NULL, ep.method = "none") {
  predictor = makePredictor(model = model, data = data, target = target)
  res = data.frame(matrix(ncol=6,nrow=0))
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
          fme = fme(model = model,
                    data = data,
                    target = target,
                    feature = feature,
                    step.size = step.size,
                    ep.method = ep.method)
          res = rbind(res, c(feature,
                             step.size,
                             round(fme$ame, 4),
                             round(quantile(x = fme$results$fme, probs = c(0.25)), 4),
                             round(quantile(x = fme$results$fme, probs = c(0.75)), 4),
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
            fme = fme(model = model,
                      data = data,
                      target = target,
                      feature = feature,
                      step.size = as.character(categories[j]))
            if (j == 1) {
              featurename = feature
            } else {
              featurename = ""
            }
            res = rbind(res, c(featurename,
                               as.character(categories[j]),
                               round(fme$ame, 4),
                               round(quantile(x = fme$results$fme, probs = c(0.25)), 4),
                               round(quantile(x = fme$results$fme, probs = c(0.75)), 4),
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
        fme = fme(model = model,
                  data = data,
                  target = target,
                  feature = feature,
                  step.size = step.size,
                  ep.method = ep.method)
        res = rbind(res, c(feature,
                           step.size,
                           round(fme$ame, 4),
                           round(quantile(x = fme$results$fme, probs = c(0.25)), 4),
                           round(quantile(x = fme$results$fme, probs = c(0.75)), 4),
                           nrow(fme$results)))
      }
      if (predictor$feature.types[i] == "categorical") {
        categories = unique(as.vector(predictor$X[,..feature])[[1]])
        for (j in seq_len(length(categories))) {
          #catname = paste0(feature, ".", categories[j])
          fme = fme(model = model,
                    data = data,
                    target = target,
                    feature = feature,
                    step.size = as.character(categories[j]))
          if (j == 1) {
            featurename = feature
          } else {
            featurename = ""
          }
          res = rbind(res, c(featurename,
                             as.character(categories[j]),
                             round(fme$ame, 4),
                             round(quantile(x = fme$results$fme, probs = c(0.25)), 4),
                             round(quantile(x = fme$results$fme, probs = c(0.75)), 4),
                             nrow(fme$results)))
        }
      }
    }
  }
  res = rbind(c("Feature",  "step.size", "AME", "0.25", "0.75", "n"), res)
  names(res) = c("", "", "", "", "", "")
  res[,c(1,2)] = format(res[,c(1,2)], justify = "left")
  cat("Model Summary Using Average Marginal Effects:\n")
  print(res, row.names = FALSE)
}
