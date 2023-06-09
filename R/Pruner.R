Pruner = R6::R6Class("Pruner",
  public = list(

    initialize = function(tree) {
      self$tree = tree
    },

    prune = function() {

      tree = self$tree

      # function for the SD of the union of a given pair of nodes in a partykit::data_party data.table
      sdParent = function(data, ids) {
        data.table::setkeyv(data, names(data)[2])
        fme = unlist(data[.(ids)][,1])
        sd.parent = sd(fme)
        return(sd.parent)
      }
      # all terminal nodes
      terminal.nodes = partykit::nodeids(tree, terminal = TRUE)
      # all pairs of terminal nodes
      pairs = combn(terminal.nodes, 2)
      # ids of subsequent terminal nodes
      subseq.ids = which(combn(terminal.nodes, 2)[1, ] - combn(terminal.nodes, 2)[2, ] == -1)
      # all pairs of subsequent terminal nodes
      subseq = as.matrix(pairs[, subseq.ids])
      # ids of pairs of subsequent terminal nodes that are candidates for pruning
      cand.ids = which(!(subseq[1,] - 1) %in% terminal.nodes)
      # candidates for pruning
      cand = as.matrix(subseq[,cand.ids])
      # SD of candidates for pruning
      colnames = c("fme", "(fitted)")
      data = as.data.table(partykit::data_party(tree))[, ..colnames]
      cand.sd = apply(cand, 2, FUN = function(x) sdParent(data, x))
      # id of node to be pruned (parent node of candidate pair with lowest SD)
      prune.id = cand[,which.min(cand.sd)][1] - 1
      # prune tree
      tree = partykit::nodeprune(tree, ids = prune.id)
      return(tree)
    },

    tree = NULL
  )
)
