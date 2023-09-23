#' @title PredictorMLR3
#'
#' @include Predictor.R
#'
#' @description
#' This task specializes [Predictor] for `mlr3` models.
#' The `model` is assumed to be a `LearnerRegr` or `LearnerClassif`.
#'
#' It is recommended to use [makePredictor()] for construction of Predictor objects.
#' @export
PredictorMLR3 = R6::R6Class("PredictorMLR3",

  inherit = Predictor,

  public = list(

    #' @description
    #' Create a new PredictorMLR3 object.
    #' @param model `LearnerRegr` or `LearnerClassif` object.
    #' @param data The data used for computing FMEs, must be data.frame or data.table.
    #' @param target A string specifying the target variable.
    initialize = function(model, data, target) {
      private$initializeSubclass(model, data, target)
    },

    #' @description
    #' Predicts on an observation `"newdata"`.
    #' @param newdata The feature vector for which the target should be predicted.
    predict = function(newdata) {
      if ("LearnerRegr" %in% class(self$model)) {
        prediction = as.data.table(self$model$predict_newdata(newdata))[,3]
      }
      if ("LearnerClassif" %in% class(self$model)) {
        if (!"prob" %in% self$model$predict_type) {
          stop(paste(class(self)[1], "Your learner needs predict_type = `prob`"))
        }
        # the target class for the probability is the last item of the levels of the target
        prediction = as.data.table(self$model$predict_newdata(newdata))[,4]
      }
      names(prediction) = "prediction"
      return(prediction)
   }

  )
)
