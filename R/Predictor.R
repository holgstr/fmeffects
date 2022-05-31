# Abstract Predictor Class
Predictor = R6Class("Predictor",

  public = list(

    initialize = function(...) {
      stop(paste(class(self)[1], "is an abstract class that cannot be initialized."))
    },

    model = NULL,
    X = NULL,
    feature.names = NULL,
    feature.types = NULL

  ),
  private = list(

    initializeSubclass = function(model, data, target) {

      # Check if data is data.frame
      assertDataFrame(data, all.missing = FALSE)

      # Transform data into data.table if necessary
      if (!testDataTable(data)) {
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


# Predictor for regression models of the 'mlr3' package
PredictorMLR3 = R6Class("PredictorMLR3",

  inherit = Predictor,

  public = list(

    initialize = function(model, data, target) {
      private$initializeSubclass(model, data, target)
    },

    predict = function(newdata) {
      prediction = as.data.table(self$model$predict_newdata(newdata))[,3]
      names(prediction) = "prediction"
      return(prediction)
    }

  )
)


# Predictor for regression models of the 'randomForest' package
PredictorRandomForest = R6Class("PredictorRandomForest",

  inherit = Predictor,

  public = list(

    initialize = function(model, data, target) {
      private$initializeSubclass(model, data, target)
    },

    predict = function(newdata) {
      prediction = as.data.table(predict(self$model, newdata = newdata))
      names(prediction) = "prediction"
      return(prediction)
    }

  )
)



# Make Predictor
makePredictor = function(model, data, target) {
  if ("LearnerRegr" %in% class(model)) {
    return(PredictorMLR3$new(model, data, target))
  } else if ("randomForest" %in% class(model)) {
    return(PredictorRandomForest$new(model, data, target))
  }
}
