#' @title PredictorRandomForest
#'
#' @include Predictor.R
#'
#' @description
#' This task specializes [Predictor] for `randomForest` regression models.
#' The `model` is assumed to be a `"randomForest"`.
#'
#' It is recommended to use [makePredictor()] for construction of Predictor objects.
#' @export
PredictorRandomForest = R6Class("PredictorRandomForest",

  inherit = Predictor,

  public = list(

    #' @description
    #' Create a new PredictorRandomForest object.
    #' @param model `randomForest` object.
    #' @param data The data used for computing FMEs, must be data.frame or data.table.
    #' @param target A string specifying the target variable.
    initialize = function(model, data, target) {
      private$initializeSubclass(model, data, target)
    },

    #' @description
    #' Predicts on an observation `"newdata"`.
    #' @param newdata the feature vector for which the target should be predicted.
    predict = function(newdata) {
      prediction = as.data.table(predict(self$model, newdata = newdata))
      names(prediction) = "prediction"
      return(prediction)
    }

  )
)
