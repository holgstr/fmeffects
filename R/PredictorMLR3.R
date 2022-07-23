#' @title PredictorMLR3
#'
#' @include Predictor.R
#'
#' @description
#' This task specializes [Predictor] for `mlr3` regression models.
#' The `model` is assumed to be a `"LearnerRegr"`.
#'
#' It is recommended to use [makePredictor()] for construction of Predictor objects.
#' @export
PredictorMLR3 = R6Class("PredictorMLR3",

  inherit = Predictor,

  public = list(

    #' @description
    #' Create a new PredictorMLR3 object.
    #' @param model `LearnerRegr` object.
    #' @param data the data used for computing FMEs, must be data.frame or data.table.
    #' @param target a string specifying the target variable.
    initialize = function(model, data, target) {
      private$initializeSubclass(model, data, target)
    },

    #' @description
    #' Predicts on an observation `"newdata"`.
    #' @param newdata the feature vector for which the target should be predicted.
    predict = function(newdata) {
      prediction = as.data.table(self$model$predict_newdata(newdata))[,3]
      names(prediction) = "prediction"
      return(prediction)
   }

  )
)
