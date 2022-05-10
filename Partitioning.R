# Abstract Partioning Class
Partitioning = R6Class("Partitioning",
  public = list(
    
    initialize = function(...) {
      stop(paste(class(self)[1], "is an abstract class that cannot be initialized."))
    },
    
    compute = function() {
      # Create data for partitioning algorithm
      data = data.table::copy(self$object$predictor$X[self$object$results$obs.id,])
      data.table::set(data, j = "fme", value = self$object$results$fme)
      if (self$method == "partitions") {
        self$tree = private$partConstant(data, self$value)
      } else {
        self$tree = private$partMaxCov(data, self$value)
      }
      invisible(self)
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
    
    partConstant = function(data, value) {
      
      tree = private$growTree(data)
      partitions = length(nodeids(tree, terminal = TRUE))
      if (partitions < value) {
        stop(paste(class(self)[1], "was unable to find a tree with at least", value, "terminal nodes"))
      }
      for (i in seq_len(partitions - value)) {
        tree = Pruner$new(tree)$prune()
      }
      return(tree)
      
    },
    
    partMaxCov = function(data, value) {
      
      # Function for the max.cov of all terminal nodes in a party tree
      covTree = function(tree) {
        df = as.data.table(data_party(tree))
        terminal.nodes = nodeids(tree, terminal = TRUE)
        covMax = function(data, id) {
          setkey(data, "(fitted)")
          fme = unlist(data[.(id)][,1])
          cov.max = sd(fme) / (abs(mean(fme)))
          return(cov.max)
        }
        max.cov = max(sapply(terminal.nodes, FUN = function(x) max(covMax(df, x))))
        return(max.cov)
      }
      
      tree = private$growTree(data)
      partitions = length(nodeids(tree, terminal = TRUE))
      
      # Find best tree among all trees created by iterative pruning
      best.tree = tree
      best.cov = covTree(tree)
      
      while (partitions > 2) {
        tree = Pruner$new(tree)$prune()
        cov.tree = covTree(tree)
        if (cov.tree < value) {
          best.tree = tree
          best.cov = cov.tree
        }
        partitions = length(nodeids(tree, terminal = TRUE))
      }
      
      # Check if best tree is admissible
      if (length(nodeids(best.tree, terminal = TRUE)) > 1 & best.cov <= value) {
        return(best.tree)
      } else {
        stop(paste(class(self)[1], "was unable to find a tree with a max.cov of", value))
      }
      
    }
    
  )
)


# Partioning for Ctree from the 'partykit' package
PartitioningCtree = R6Class("PartitioningCtree",
                            
  inherit = Partitioning,
  
  public = list(
    
    initialize = function(object, method, value) {
      private$initializeSubclass(object, method, value)
    }
    
  ),
  private = list(
    
    growTree = function(data) {
      tree = ctree(fme ~ .,
                   data = data,
                   control = ctree_control(alpha = 0.35,
                                           minbucket = nrow(data)*0.02))
      return(tree)
    }
    
  )
)


PartitioningRpart = R6Class("PartitioningRpart",
 inherit = Partitioning,
 public = list(
   
   initialize = function(object, method, value) {
     private$initializeSubclass(object, method, value)
   }
   
 ),
 private = list(
   
   growTree = function(data) {
     tree = as.party(rpart(fme ~ .,
                           data = data,
                           control = rpart.control(minbucket = round(nrow(data)*0.04),
                                                   cp= 0.001)))
     return(tree)
   }
   
 )
)




