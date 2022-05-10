# Abstract Predictor Class
Partitioning = R6Class("Partitioning",
  public = list(
    
    initialize = function(...) {
      stop(paste(class(self)[1], "is an abstract class that cannot be initialized."))
    },
    
    compute = function() {
      # Create data for partitioning algorithm
      data = data.table::copy(object$predictor$X[object$results$obs.id,])
      data.table::set(data, j = "fme", value = object$results$fme)
      # Grow deep tree depending on subclass and method
      self$tree = private$tryGrowing(data, self$method, self$value)
      # Prune deep tree
      self$tree = Pruner$new(self$tree, self$method, self$value)$prune()
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
    
    tryGrowing = function(data, method, value) {
      
      tree = private$growTree(data)
      if (method == "partitions") {
        if (length(nodeids(tree, terminal = TRUE)) < value) {
          stop(paste(class(self)[1], "seems unable to grow a tree with at least", value, method))
        }
      } else {
        df = as.data.table(data_party(tree))
        terminal.nodes = nodeids(tree, terminal = TRUE)
        # function for the cov of a terminal node in a data_party data.table
        covTerminal = function(data, id) {
          setkey(data, "(fitted)")
          fme = unlist(data[.(id)][,1])
          cov.terminal = sd(fme) / (abs(mean(fme)))
          return(cov.terminal)
        }
        max.cov = max(sapply(terminal.nodes, FUN = function(x) max(covTerminal(df, x))))
        if (max.cov > value) {
          stop(paste(class(self)[1], "seems unable to grow a tree with a", method, "of", value))
        }
      }
      return(tree)
    }
    
  )
)


# Predictor for regression models of the 'mlr3' package
PartitioningCtree = R6Class("PartitioningCtree",
  inherit = Partitioning,
  public = list(
    
    initialize = function(object, method, value) {
      private$initializeSubclass(object, method, value)
    }
  ),
  private = list(
    
    growTree = function(data) {
      tree = ctree(fme ~ ., data = data, control = ctree_control(alpha = 0.99, minprob = 0.04))
      return(tree)
    }
    
  )
)
