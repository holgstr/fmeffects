#' @title R6 Class representing a predictor
#'
#' @description This is the abstract superclass for predictor objects like [PredictorMLR3] and [PredictorCaret].
#' A Predictor contains information about an ML model's prediction function and training data.
#' @export
Predictor = R6::R6Class("Predictor",

  public = list(

    #' @description Create a Predictor object
    #' @param ...
    #' Predictor cannot be initialized, only its subclasses
    initialize = function(...) {
      stop(paste(class(self)[1], "is an abstract class that cannot be initialized."))
    },

    #' @field model The (trained) model, with the ability to predict on new data.
    model = NULL,
    #' @field X A data.table with feature and target variables.
    X = NULL,
    #' @field feature.names A character vector with the names of the features in X.
    feature.names = NULL,
    #' @field feature.types A character vector with the types (numerical or categorical) of the features in X.
    feature.types = NULL

  ),
  private = list(

    initializeSubclass = function(model, data, target) {

      # Check if data is data.frame
      checkmate::assertDataFrame(data, all.missing = FALSE)

      # Transform data into data.table if necessary
      if (!checkmate::testDataTable(data)) {
        data = as.data.table(data)
      }

      self$model = model
      self$feature.names = private$getFeatureNames(data, target)
      self$X = private$getX(data, self$feature.names)
      self$feature.types = private$getFeatureTypes(self$X, self$feature.names)

    },

    getX = function(data, feature.names) {
      #data[, ..feature.names]
      data[, feature.names, with=FALSE]
    },

    getFeatureNames = function(data, target) {
      return(setdiff(names(data), target))
    },

    getFeatureTypes = function(X, feature.names) {
      feature.types = c("integer" = "numerical",
                         "numeric" = "numerical",
                         "character" = "categorical",
                         "factor" = "categorical",
                         "ordered" = "categorical")
      feature.types = feature.types[unlist(lapply(X, function(x){class(x)[1]}))]
      names(feature.types) = feature.names
      return(feature.types)
    }

  )
)


#' @title User-friendly function to create a [Predictor].
#'
#' @description A wrapper function that creates the correct subclass of `Predictor` by automatically from `model`. Can be passed to the constructor of `FME`.
#' @param model the (trained) model, with the ability to predict on new data.
#' @param data the data used for computing FMEs, must be data.frame or data.table.
#' @param target a string specifying the target variable.
#' @examples
#' # Train a model:
#'
#' library(mlr3verse)
#' data(bikes, package = "fmeffects")
#' task = as_task_regr(x = bikes, id = "bikes", target = "count")
#' forest = lrn("regr.ranger")$train(task)
#'
#' # Create the predictor:
#' predictor = makePredictor(forest, bikes, "count")
#'
#' # This instantiated an object of the correct subclass of `Predictor`:
#' class(predictor)
#' @export
makePredictor = function(model, data, target) {
  if ("Learner" %in% class(model)) {
    return(PredictorMLR3$new(model, data, target))
  }
  if ("train" %in% class(model) & "train.formula" %in% class(model)) {
    return(PredictorCaret$new(model, data, target))
  }
  if ("model_fit" %in% class(model)) {
    return(PredictorParsnip$new(model, data, target))
  }
}
