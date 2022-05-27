summary.Partitioning = function(partitioning) {
  ans = length(nodeids(partitioning$tree, terminal = TRUE))
  class(ans) = "summary.Partitioning"
  ans
}
summary.Partitioning(c)
summary(c)


