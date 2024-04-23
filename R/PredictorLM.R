#' @title PredictorLM
#'
#' @include Predictor.R
#'
#' @description
#' This task specializes [Predictor] for `lm` and `lm`-type models.
#' The `model` is assumed to be a `lm`.
#'
#' It is recommended to use [makePredictor()] for construction of Predictor objects.
#' @export
PredictorLM = R6::R6Class("PredictorLM",

  inherit = Predictor,

  public = list(

    #' @description
    #' Create a new PredictorCaret object.
    #' @param model `train, train.formula` object.
    #' @param data The data used for computing FMEs, must be data.frame or data.table.
    initialize = function(model, data) {
      private$initializeSubclass(model, data)
    },

    #' @description
    #' Predicts on an observation `"newdata"`.
    #' @param newdata The feature vector for which the target should be predicted.
    predict = function(newdata) {
      # Classification
      if (!is.null(self$model$family) & model$family$family %in% c("binomial", "quasibinomial")) {
        prediction = data.table(predict(self$model, newdata = newdata, type = "response"))
      # Regression
      } else {
        prediction = data.table(predict(self$model, newdata = newdata))
      }
      names(prediction) = "prediction"
      return(prediction)
   }

  ),
  private = list(

    getTarget = function(model) {
      return(all.vars(formula(model))[1])
    }

  )
)
