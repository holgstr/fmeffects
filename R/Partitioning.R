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
      if (is.null(self$tree.control)) {
        tree = private$growTree(data)
      } else {
        tree = private$growTree(data, self$tree.control)
      }
      if (self$method == "partitions") {
        self$tree = private$partConstant(data, self$value, tree)
      } else {
        self$tree = private$partMaxCov(data, self$value, tree)
      }
      self$results = private$getResults(self$tree, self$object)
      invisible(self)
    },

    plot = function() {
      if ("nlm" %in% names(self$object$results)) {
        PartitioningPlot$new(self$tree, self$object)$plot
      } else {
        PartitioningPlot$new(self$tree, self$object, has.nlm = FALSE)$plot
      }
    },

    object = NULL,
    method = NULL,
    value = NULL,
    results = NULL,
    tree = NULL,
    tree.control = NULL

  ),
  private = list(

    initializeSubclass = function(object, method, value, tree.control) {

      # Check if object is of class 'FME'
      assertClass(object, classes = "FME")

      # Check if method is sensible
      assertChoice(method, choices = c("partitions", "max.cov"))

      # Check if value is sensible and within range
      if (method == "partitions") {
        assertIntegerish(value, lower = 2, upper = 8, len = 1)
      } else {
        assertNumeric(value, lower = sqrt(.Machine$double.eps))
      }

      self$object = object
      self$method = method
      self$value = value
      self$tree.control = tree.control

    },

    partConstant = function(data, value, tree) {

      partitions = length(nodeids(tree, terminal = TRUE))
      if (partitions < value) {
        stop(paste(class(self)[1], "was unable to find a tree with at least", value, "terminal nodes"))
      }
      for (i in seq_len(partitions - value)) {
        tree = Pruner$new(tree)$prune()
      }
      return(tree)

    },

    partMaxCov = function(data, value, tree) {

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
        max.cov = max(sapply(terminal.nodes, FUN = function(x) max(covMax(df, x))), na.rm = TRUE)
        return(max.cov)
      }

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

    },

    getResults = function(tree, object) {
      nodes = nodeids(tree)
      nodeResults = function(tree, object, node.id) {
        terminal.nodes = nodeids(tree, from = node.id, terminal = TRUE)
        is.terminal = node.id %in% terminal.nodes
        data = as.data.table(data_party(tree))
        if (!("nlm" %in% names(object$results))) {
          setkey(data, "(fitted)")
          data = data[.(terminal.nodes),]
          res = list("n" = nrow(data),
                     "cAME" = mean(data$fme),
                     "CoV(FME)" = sd(data$fme) / (abs(mean(data$fme))),
                     "is.terminal.node" = is.terminal)
        } else {
          data.table::set(data, j = "nlm", value = object$results$nlm)
          setkey(data, "(fitted)")
          data = data[.(terminal.nodes),]
          res = list("n" = nrow(data),
                     "cAME" = mean(data$fme),
                     "CoV(FME)" = sd(data$fme) / (abs(mean(data$fme))),
                     "cANLM" = mean(data$nlm),
                     "CoV(NLM)" = sd(data$nlm) / (abs(mean(data$nlm))),
                     "is.terminal.node" = is.terminal)
        }
        res
      }
      lapply(nodes, FUN = function(x) nodeResults(tree, object, x))
    }

  )
)


# Partioning for Ctree from the 'partykit' package
PartitioningCtree = R6Class("PartitioningCtree",

  inherit = Partitioning,

  public = list(

    initialize = function(object, method, value, tree.control = NULL) {
      private$initializeSubclass(object, method, value, tree.control)
    }

  ),
  private = list(

    growTree = function(data, tree.control = ctree_control(alpha = 0.35,
                                                           minbucket = nrow(data)*0.02)) {
      tree = ctree(fme ~ .,
                   data = data,
                   control = tree.control)
      return(tree)
    }

  )
)


# Partioning for Rpart from the 'rpart' package
PartitioningRpart = R6Class("PartitioningRpart",
 inherit = Partitioning,
 public = list(

   initialize = function(object, method, value, tree.control = NULL) {
     private$initializeSubclass(object, method, value, tree.control)
   }

 ),
 private = list(

   growTree = function(data, tree.control = rpart.control(minbucket = round(nrow(data)*0.04),
                                                          cp= 0.001)) {
     tree = as.party(rpart(fme ~ .,
                           data = data,
                           control = tree.control))
     return(tree)
   }

 )
)


# User-friendly function
came = function(effects, number.partitions = NULL, max.cov = Inf, rp.method = "ctree", tree.control = NULL) {
  assertChoice(rp.method, choices = c("ctree", "rpart"))
  makePartitioner = function(rp.method, ...) {
    if (rp.method == "ctree") {
      part = PartitioningCtree$new(...)
    } else if (rp.method == "rpart") {
      part = PartitioningRpart$new(...)
    }
  }
  if (is.infinite(max.cov) & !is.null(number.partitions)) {
    part = makePartitioner(rp.method, effects, "partitions", number.partitions, tree.control)$compute()
  } else if (!is.infinite(max.cov) & is.null(number.partitions)){
    part = makePartitioner(rp.method, effects, "max.cov", max.cov, tree.control)$compute()
  } else {
    stop(paste("Must supply either 'number.partitions' or 'max.cov', not both."))
  }
  return(part)
}
