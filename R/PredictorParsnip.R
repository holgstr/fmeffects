#' @title PredictorParsnip
#'
#' @include Predictor.R
#'
#' @description
#' This task specializes [Predictor] for `parsnip` models.
#' The `model` is assumed to be a `model_fit` object.
#'
#' It is recommended to use [makePredictor()] for construction of Predictor objects.
#' @export
PredictorParsnip = R6::R6Class("PredictorParsnip",

  inherit = Predictor,

  public = list(

    #' @description
    #' Create a new PredictorParsnip object.
    #' @param model `model_fit` object.
    #' @param data The data used for computing FMEs, must be data.frame or data.table.
    initialize = function(model, data) {
      private$initializeSubclass(model, data)
    },

    #' @description
    #' Predicts on an observation `"newdata"`.
    #' @param newdata The feature vector for which the target should be predicted.
    predict = function(newdata) {
      if (self$model$spec$mode == "regression") {
        prediction = as.data.table(predict(self$model, newdata))
      }
      if (self$model$spec$mode == "classification") {
        # the target class for the probability is the second category in parsnip
        prediction = as.data.table(predict(self$model, newdata, type = "prob")[2])
      }
      names(prediction) = "prediction"
      return(prediction)
   }

  ),
  private = list(

    getTarget = function(model) {
      return(model$preproc$y_var)
    }

  )
)
