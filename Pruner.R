Pruner = R6Class("Pruner",
  public = list(
    
    initialize = function(tree) {
      self$tree = tree
    },
    
    prune = function() {
      
      tree = self$tree
      
      # function for the cov of the union of a given pair of nodes in a data_party data.table
      covParent = function(data, ids) {
        setkeyv(data, names(data)[2])
        fme = unlist(data[.(ids)][,1])
        cov.parent = sd(fme) / (abs(mean(fme)))
        return(cov.parent)
      }
      # all terminal nodes
      terminal.nodes = nodeids(tree, terminal = TRUE)
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
      # cov of candidates for pruning
      colnames = c("fme", "(fitted)")
      data = as.data.table(data_party(tree))[, ..colnames]
      cand.cov = apply(cand, 2, FUN = function(x) covParent(data, x))
      # id of node to be pruned (parent node of candidate pair with lowest cov)
      prune.id = cand[,which.min(cand.cov)][1] - 1
      # prune tree
      tree = nodeprune(tree, ids = prune.id)
      return(tree)
    },
    
    tree = NULL
  )
)
