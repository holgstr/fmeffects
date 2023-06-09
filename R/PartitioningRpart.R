#' @title PartitioningRpart
#'
#' @include Partitioning.R
#'
#' @description
#' This task specializes [Partitioning] for the `rpart` algorithm for recursive partitioning.
#'
#' It is recommended to use [came()] for construction of Partitioning objects.
#' @export
PartitioningRpart = R6::R6Class("PartitioningRpart",

 inherit = Partitioning,

 public = list(

   #' @description
   #' Create a new PartitioningRpart object.
   #' @param object An `FME` object with results computed.
   #' @param method The method for finding feature subspaces.
   #' @param value The value of `method`.
   #' @param tree.control Control parameters for the RP algorithm.
   initialize = function(object, method, value, tree.control = NULL) {
     private$initializeSubclass(object, method, value, tree.control)
   }

 ),
 private = list(

   growTree = function(data, tree.control = rpart.control(minbucket = round(nrow(data)*0.04),
                                                          cp= 0.001)) {
     tree = partykit::as.party(rpart(fme ~ .,
                           data = data,
                           control = tree.control))
     return(tree)
   }

 )
)
