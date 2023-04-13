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
        self$tree = private$partMaxSD(data, self$value, tree)
      }
      self$results = private$getResults(self$tree, self$object)
      self$computed = TRUE
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
    tree.control = NULL,
    #' @field computed logical specifying if compute() has been run
    computed = FALSE

  ),
  private = list(

    initializeSubclass = function(object, method, value, tree.control) {

      # Check if object is of class 'FME'
      assertClass(object, classes = "FME")

      # Check if object has FMEs computed
      assertTRUE(object$computed)

      # Check if method is sensible
      assertChoice(method, choices = c("partitions", "max.sd"))

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

    partMaxSD = function(data, value, tree) {

      # Function for the max.sd of all terminal nodes in a party tree
      sdTree = function(tree) {
        df = as.data.table(data_party(tree))
        terminal.nodes = nodeids(tree, terminal = TRUE)
        sdMax = function(data, id) {
          setkey(data, "(fitted)")
          fme = unlist(data[.(id)][,1])
          sd.max = sd(fme)
          return(sd.max)
        }
        max.sd = max(sapply(terminal.nodes, FUN = function(x) max(sdMax(df, x))), na.rm = TRUE)
        return(max.sd)
      }

      partitions = length(nodeids(tree, terminal = TRUE))

      # Find best tree among all trees created by iterative pruning
      best.tree = tree
      best.sd = sdTree(tree)

      while (partitions > 2) {
        tree = Pruner$new(tree)$prune()
        sd.tree = sdTree(tree)
        if (sd.tree < value) {
          best.tree = tree
          best.sd = sd.tree
        }
        partitions = length(nodeids(tree, terminal = TRUE))
      }

      # Check if best tree is admissible
      if (length(nodeids(best.tree, terminal = TRUE)) > 1 & best.sd <= value) {
        return(best.tree)
      } else {
        stop(paste(class(self)[1], "was unable to find a tree with a maximum SD of", value))
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
                     "SD(fME)" = sd(data$fme),
                     "is.terminal.node" = is.terminal)
        } else {
          data.table::set(data, j = "nlm", value = object$results$nlm)
          setkey(data, "(fitted)")
          data = data[.(terminal.nodes),]
          res = list("n" = nrow(data),
                     "cAME" = mean(data$fme),
                     "SD(fME)" = sd(data$fme),
                     "cANLM" = mean(data$nlm),
                     "SD(NLM)" = sd(data$nlm),
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
#' @param effects An `FME` object with FMEs computed.
#' @param number.partitions The exact number of partitions required.
#' Either `number.partitions` or `max.sd` can be specified.
#' @param max.sd The maximum standard deviation required in each partition.
#' Among multiple partitionings with this criterion identified, the one with lowest number of partitions is selected.
#' Either `number.partitions` or `max.sd` can be specified.
#' @param rp.method One of `"ctree"` or `"rpart"`. The RP algorithm used for growing the decision tree. Defaults to `"ctree"`.
#' @param tree.control Control parameters for the RP algorithm. One of `"ctree.control(...)"` or `"rpart.control(...)"`.
#' #' @return `Partitioning` Object with identified feature subspaces.
#' @references
#' Scholbeck, C. A., Casalicchio, G., Molnar, C., Bischl, B., & Heumann, C. (2022). Marginal Effects for Non-Linear Prediction Functions.
#' @examples
#' # Train a model and compute FMEs:
#' library(mlr3verse)
#' data(bikes, package = "fme")
#' forest = lrn("regr.ranger")$train(as_task_regr(x = bikes, id = "bikes", target = "count"))
#' effects = fme(model = forest, data = bikes, target = "count", feature = "temp",
#'               step.size = 1, ep.method = "envelope")
#'
#' # Find a partitioning with exactly 3 subspaces:
#' subspaces = came(effects, number.partitions = 3)
#'
#' # Find a partitioning with a maximum standard deviation of 4, use `rpart`:
#' subspaces = came(effects, max.sd = 4, rp.method = "rpart")
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
came = function(effects, number.partitions = NULL, max.sd = Inf, rp.method = "ctree", tree.control = NULL) {
  assertChoice(rp.method, choices = c("ctree", "rpart"))
  makePartitioner = function(rp.method, ...) {
    if (rp.method == "ctree") {
      part = PartitioningCtree$new(...)
    } else if (rp.method == "rpart") {
      part = PartitioningRpart$new(...)
    }
  }
  if (is.infinite(max.sd) & !is.null(number.partitions)) {
    part = makePartitioner(rp.method, effects, "partitions", number.partitions, tree.control)$compute()
  } else if (!is.infinite(max.sd) & is.null(number.partitions)){
    part = makePartitioner(rp.method, effects, "max.sd", max.sd, tree.control)$compute()
  } else {
    stop(paste("Must supply either 'number.partitions' or 'max.sd'."))
  }
  return(part)
}
