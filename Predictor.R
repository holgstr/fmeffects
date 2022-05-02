Predictor = R6Class("Predictor",
  public = list(
    
    # Abstract Class
    initialize = function(...) {
      stop(paste(class(self)[1], "is an abstract class that cannot be initialized."))
    },
    
    model = NULL,
    X = NULL,
    feature.names = NULL,
    feature.types = NULL
    
  ),
  private = list(
    
    initializeSubclass = function(model, data, y) {
      
      # Check if data is data.frame
      assert_data_frame(data, all.missing = FALSE)
      
      # Transform data into data.table if necessary
      if (test_data_table(data)) {
        data = as.data.table(data)
      }
      
      self$model = model
      self$feature.names = private$get.feature.names(data, y)
      self$X = private$get.X(data, self$feature.names)
      self$feature.types = private$get.feature.types(self$X, self$feature.names)
      
    },
    
    get.X = function(data, feature.names) {
      return(data[, ..feature.names])
    },
    
    get.feature.names = function(data, y) {
      return(setdiff(names(data), y))
    },
    
    get.feature.types = function(X, feature.names) {
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
    
    initialize = function(model, data, y) {
      private$initializeSubclass(model, data, y)
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
                          
    initialize = function(model, data, y) {
      private$initializeSubclass(model, data, y)
    },
                          
    predict = function(newdata) {
      prediction = as.data.table(predict(self$model, newdata = newdata))
      names(prediction) = "prediction"
      return(prediction)
    }
  )                      
)
