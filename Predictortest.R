Predictortest <- R6Class("Predictortest",
  public = list(
    initialize = function(model, data, y) {
    
      self$model = model
      self$feature.names = private$get.feature.names(data, y)
      self$X = private$get.X(data, self$feature.names)
      self$feature.types = private$get.feature.types(self$X)
      
    },
    
    predict = function(newdata) {
      # function that calls self$model and predicts on new observations
      as.data.table(self$model$predict_newdata(Boston[1:6,]))[,3]
    },
    
    model = NULL,
    X = NULL,
    feature.names = NULL,
    feature.types = NULL
  ),
  private = list(
    
    get.X = function(data, feature.names) {
      #a$data$X
      data[feature.names]
    },
    
    get.feature.names = function(data, y) {
      # function that gets feature names
      #a$data$feature.names
      setdiff(names(data), y)
    },
    
    get.feature.types = function(X) {
      # function that gets feature types
      #a$data$feature.types
      4
    }
  
  )
)