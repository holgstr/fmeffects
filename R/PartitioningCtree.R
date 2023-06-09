#' @title PartitioningCtree
#'
#' @include Partitioning.R
#'
#' @description
#' This task specializes [Partitioning] for the `ctree` algorithm for recursive partitioning.
#'
#' It is recommended to use [came()] for construction of Partitioning objects.
#' @export
PartitioningCtree = R6::R6Class("PartitioningCtree",

  inherit = Partitioning,

  public = list(

    #' @description
    #' Create a new PartitioningCtree object.
    #' @param object an `FME` object with results computed.
    #' @param method the method for finding feature subspaces.
    #' @param value the value of `method`.
    #' @param tree.control control parameters for the RP algorithm.
    initialize = function(object, method, value, tree.control = NULL) {
      private$initializeSubclass(object, method, value, tree.control)
    }

  ),
  private = list(

    growTree = function(data, tree.control = partykit::ctree_control(alpha = 0.35,
                                                           minbucket = nrow(data)*0.02)) {
      tree = partykit::ctree(fme ~ .,
                   data = data,
                   control = tree.control)
      return(tree)
    }

  )
)
