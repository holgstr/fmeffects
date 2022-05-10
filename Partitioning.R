# Abstract Predictor Class
Partitioning = R6Class("Partitioning",
  public = list(
    
    initialize = function(...) {
      stop(paste(class(self)[1], "is an abstract class that cannot be initialized."))
    },
    
    object = NULL,
    method = NULL,
    value = NULL,
    tree = NULL
    
  ),
  private = list(
    
    initializeSubclass = function(object, method, value) {
      
      # Check if object is of class 'FME'
      assertClass(object, classes = "FME")
      
      # Check if method is sensible
      assertChoice(method, choices = c("partitions", "max.cov"))
      
      # Check if value is sensible and within range
      if (method == "partitions") {
        assertIntegerish(value, lower = 2, upper = 10, len = 1)
      } else {
        assertNumeric(value, lower = sqrt(.Machine$double.eps))
      }
      
      self$object = object
      self$method = method
      self$value = value
      
    },
    
    getX = function(data, feature.names) {
      return(data[, ..feature.names])
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