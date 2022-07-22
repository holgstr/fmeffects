#' Prints summary of FME partitioning.
#'
#' @export
summary.Partitioning = function(partitioning) {
  print(partitioning)
}

#' Prints an FME partitioning.
#'
#' @export
print.Partitioning = function(partitioning) {
  cat("\n")
  cat(class(partitioning)[1])
  cat(" of an FME object\n\n")
  cat("Method:  ")
  cat(partitioning$method)
  cat(" = ")
  cat(partitioning$value)
  cat("\n\n")
  res = do.call(rbind.data.frame, partitioning$results)
  names(res)[grep("CoV", names(res))[1]] = c("CoV(fME)")
  if (length(grep("CoV", names(res))) == 2) {
    names(res)[grep("CoV", names(res))[2]] = c("CoV(NLM)")
  }
  res
}

#' Plots an FME partitioning.
#'
#' @export
plot.Partitioning = function(partitioning) {
  partitioning$plot()
}
