#' @title R6 Class representing a partitioning
#'
#' @description This is the abstract superclass for partitioning objects like [PartitioningCtree] and [PartitioningRpart].
#' A Partitioning contains information about feature subspaces with conditional average marginal effects (cAME) computed for `FME` objects.
#' @export
Partitioning = R6Class("Partitioning",
  public = list(

    #' @description Create a Partitioning object
    #' @param ...
    #' Partitioning cannot be initialized, only its subclasses
    initialize = function(...) {
      stop(paste(class(self)[1], "is an abstract class that cannot be initialized."))
    },

    #' @description
    #' Computes the partitioning, i.e., feature subspaces with more homogeneous FMEs, for an `FME` object.
    #' @return An `Partitioning` object with results.
    #' @examples
    #' # Compute results for an arbitrary partitioning:
    #' subspaces$compute()
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

    #' @description
    #' Plots results, i.e., a decision tree and summary statistics of the feature subspaces, for an `Partitioning` object after `$compute()` has been run.
    #' @examples
    #' # Plot an arbitrary partitioning:
    #' subspaces$plot()
    plot = function() {
      if ("nlm" %in% names(self$object$results)) {
        PartitioningPlot$new(self$tree, self$object)$plot
      } else {
        PartitioningPlot$new(self$tree, self$object, has.nlm = FALSE)$plot
      }
    },

    #' @field object an `FME` object with results computed
    object = NULL,
    #' @field method the method for finding feature subspaces
    method = NULL,
    #' @field value the value of `method`
    value = NULL,
    #' @field results descriptive statistics of the resulting feature subspaces
    results = NULL,
    #' @field tree the tree representing the partitioning, a `party` object
    tree = NULL,
    #' @field tree.control control parameters for the RP algorithm
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
                     "CoV(fME)" = sd(data$fme) / (abs(mean(data$fme))),
                     "is.terminal.node" = is.terminal)
        } else {
          data.table::set(data, j = "nlm", value = object$results$nlm)
          setkey(data, "(fitted)")
          data = data[.(terminal.nodes),]
          res = list("n" = nrow(data),
                     "cAME" = mean(data$fme),
                     "CoV(fME)" = sd(data$fme) / (abs(mean(data$fme))),
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


# User-friendly function

#' @title Computes a partitioning for an `FME`
#'
#' @description This is a wrapper function that creates the correct subclass of `Partitioning`.
#' It computes feature subspaces for semi-global interpretations of FMEs via recursive partitioning (RP).
#' @param effects an `FME` object with FMEs computed.
#' @param number.partitions the exact number of partitions required.
#' Either `number.partitions` or `max.cov` can be specified.
#' @param max.cov the maximum coefficient of variation required in each partition.
#' Among multiple partitionings with this criterion identified, the one with lowest number of partitions is selected.
#' Either `number.partitions` or `max.cov` can be specified.
#' @param rp.method one of `"ctree"` or `"rpart"`. The RP algorithm used for growing the decision tree.
#' @param tree.control control parameters for the RP algorithm. One of `"ctree.control(...)"` or `"rpart.control(...)"`.
#' #' @return `Partitioning` object with identified feature subspaces.
#' @references
#' Scholbeck, C. A., Casalicchio, G., Molnar, C., Bischl, B., & Heumann, C. (2022). Marginal Effects for Non-Linear Prediction Functions.
#' @examples
#' # Train a model and compute FMEs:
#' data("Boston", package = "MASS")
#' forest = randomForest(medv ~ ., data = Boston)
#' effects = fme(model = forest, data = Boston, target = "medv", feature = "rm",
#'               step.size = 1, ep.method = "envelope", )
#'
#' # Find a partitioning with exactly 3 subspaces:
#' subspaces = came(effects, number.partitions = 3)
#'
#' # Find a partitioning with a maximum coefficient of variation of 4, use `rpart`:
#' subspaces = came(effects, max.cov = 4, rp.method = "rpart")
#'
#' # Analyze results:
#' summary(subspaces)
#' plot(subspaces)
#'
#' # Extract results:
#' subspaces$results
#' subspaces$tree
#'
#' @export
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
    stop(paste("Must supply either 'number.partitions' or 'max.cov'."))
  }
  return(part)
}
