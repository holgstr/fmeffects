#' @title PredictorCaret
#'
#' @include Predictor.R
#'
#' @description
#' This task specializes [Predictor] for `caret` regression models.
#' The `model` is assumed to be a `c("train", "train.formula")`.
#'
#' It is recommended to use [makePredictor()] for construction of Predictor objects.
#' @export
PredictorCaret = R6::R6Class("PredictorCaret",

  inherit = Predictor,

  public = list(

    #' @description
    #' Create a new PredictorCaret object.
    #' @param model `train, train.formula` object.
    #' @param data The data used for computing FMEs, must be data.frame or data.table.
    #' @param target A string specifying the target variable.
    initialize = function(model, data, target) {
      private$initializeSubclass(model, data, target)
    },

    #' @description
    #' Predicts on an observation `"newdata"`.
    #' @param newdata The feature vector for which the target should be predicted.
    predict = function(newdata) {
      if (self$model$modelType == "Regression") {
        prediction = as.data.table(predict(self$model, newdata = newdata))
      }
      if (self$model$modelType == "Classification") {
        prediction = as.data.table(predict(self$model, newdata = newdata, type = "prob")[1])
      }
      names(prediction) = "prediction"
      return(prediction)
   }

  )
)
