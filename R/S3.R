summary.Partitioning = function(partitioning) {
  ans = partitioning$results
  ans
}

print.Partitioning = function(partitioning) {
  cat("\n")
  cat(class(partitioning)[1])
  cat(" of an FME object\n\n")
  cat("Method:  ")
  cat(partitioning$method)
  cat(" = ")
  cat(partitioning$value)
  cat("\n\n")
  print(do.call(rbind.data.frame, f$results))
}

plot.Partitioning = function(partitioning) {
  partitioning$plot()
}
