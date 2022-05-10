Pruner = R6Class("Pruner",
  public = list(
    
    initialize = function(tree, method, value) {
      self$tree = tree
      self$method = method
      self$value = value
    },
    
    prune = function() {
      
      tree = self$tree
      method = self$method
      value = self$value
      
      # function for the cov of the union of a given pair of nodes in a data_party data.table
      covData = function(data, ids) {
        setkeyv(data, names(data)[2])
        fme = unlist(data[.(ids)][,1])
        cov.data = sd(fme) / (abs(mean(fme)))
        return(cov.data)
      }
      
      # function that checks if there is an admissible candidate for method "max.cov"
      checkCandidates = function(cand.cov, value) {
        return(min(cand.cov) > value)
      }
      
      # test expression for the while loop:
      testExp = function() {
        if (method == "partitions") {
          result = length(terminal.nodes) > value + 1
        } else {
          result = TRUE
        }
        if (length(terminal.nodes) == 3) {
          result = FALSE
        }
        return(result)
      }
      
      terminal.nodes = nodeids(tree, terminal = TRUE)
      
      while (testExp()) {
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
        cand.cov = apply(cand, 2, FUN = function(x) covData(data, x))
        # check admissibility for method = "max.cov"
        if (method == "max.cov") {
          if (checkCandidates(cand.cov, value)) {
            break
          }
        }
        # id of node to be pruned (parent node of candidate pair with lowest cov)
        prune.id = cand[,which.min(cand.cov)][1] - 1
        # prune tree
        tree = nodeprune(tree, ids = prune.id)
      }
      return(tree)
    },
    
    tree = NULL,
    method = NULL,
    value = NULL
  )
)
