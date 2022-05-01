Predictortest <- R6Class("Predictortest",
  public = list(
    initialize = function(model, data, y) {
      
      # Check if data is data.frame
      assert_data_frame(data, all.missing = FALSE)
      
      # Transform data into data.table if neccesary
      if (test_data_table(data)) {
        data = as.data.table(data)
      }
      
      self$model = model
      self$feature.names = private$get.feature.names(data, y)
      self$X = private$get.X(data, self$feature.names)
      self$feature.types = private$get.feature.types(self$X, self$feature.names)
      
    },
    
    # Predict function with current support for mlr3 models
    predict = function(newdata) {
      return(as.data.table(self$model$predict_newdata(newdata))[,3])
    },
    
    model = NULL,
    X = NULL,
    feature.names = NULL,
    feature.types = NULL
  ),
  private = list(
    
    get.X = function(data, feature.names) {
      return(data[, ..feature.names])
    },
    
    get.feature.names = function(data, y) {
      return(setdiff(names(data), y))
    },
    
    get.feature.types = function(X, feature.names) {
      feature.types <- c("integer" = "numerical",
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
