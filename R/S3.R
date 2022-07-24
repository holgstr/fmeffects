### FME


#' Prints summary of an FME object.
#' @param fme object of class `FME`.
#' @export
summary.FME = function(fme) {
  cat("\n")
  cat("Forward Marginal Effects Object\n\n")
  cat(paste0("Step type:\n", "  ", fme$step.type, "\n\n"))
  if (fme$step.type == "numerical") {
    cat(paste0("Features & step lengths:\n"))
  } else {
    cat(paste0("Feature & reference category:\n"))
  }
  for (i in seq_len(length(fme$feature))) {
    cat(paste0("  ", fme$feature[i], ", ", fme$step.size[i], "\n"))
  }
  cat(paste0("\nExtrapolation point detection:\n", "  ", fme$ep.method))
  # empty object
  if (fme$computed == FALSE) {
    cat("\n\n<<<This is an FME object without results>>>\n")
    cat("<<<Call $compute() to compute FMEs>>>\n\n")
  } else {
    cat(paste0(", EPs: ", length(fme$extrapolation.ids), " of ",
               nrow(fme$results) + length(fme$extrapolation.ids), " obs. (",
               round(length(fme$extrapolation.ids)/nrow(fme$predictor$X)*100), " %)\n\n"))
    cat(paste0("Average Marginal Effect (AME):\n  ", round(fme$ame, 4)))
    if ("nlm" %in% names(fme$results)) {
      cat(paste0("\n\nAverage Non-Linearity Measure (ANLM):\n  ", round(fme$anlm, 2),
                 "  (≤0 implies non-linearity, 1 implies linearity)"))
    }
  }
}

#' Prints an FME object.
#' @param fme object of class `FME`.
#' @export
print.FME = function(fme) {
  cat("\n")
  cat("Forward Marginal Effects Object\n\n")
  cat(paste0("Features & step lengths:\n"))
  for (i in seq_len(length(fme$feature))) {
    cat(paste0("  ", fme$feature[i], ", ", fme$step.size[i], "\n"))
  }
  # empty object
  if (fme$computed == FALSE) {
    cat("\n<This is an FME object without results>\n\n")
  } else {
    # non-empty object
    cat(paste0("\nAverage Marginal Effect (AME):\n  ", round(fme$ame, 4)))
    if ("nlm" %in% names(fme$results)) {
      cat(paste0("\n\nAverage Non-Linearity Measure (ANLM):\n  ", round(fme$anlm, 2),
                 "  (≤0 implies non-linearity, 1 implies linearity)"))
    }
  }
}

#' Plots an FME object.
#' @param fme object of class `FME`.
#' @param with.nlm plots NLMs alongside FMEs, defaults to `FALSE`.
#' @export
plot.FME = function(fme, with.nlm = FALSE) {
  fme$plot(with.nlm)
}


### PARTITIONING


#' Prints summary of an FME Partitioning.
#' @param partitioning object of class `Partitioning`.
#' @export
summary.Partitioning = function(partitioning) {
  cat("\n")
  cat(class(partitioning)[1])
  cat(" of an FME object\n\n")
  cat("Method:  ")
  cat(partitioning$method)
  cat(" = ")
  cat(partitioning$value)
  cat("\n\n")
  # empty object
  if (partitioning$computed == FALSE) {
    cat("<<<This is an Partitioning object without results>>>\n")
    cat("<<<Call $compute() to compute>>>\n\n")
  } else {
    # non-empty object
    res = do.call(rbind.data.frame, partitioning$results)
    names(res)[grep("CoV", names(res))[1]] = c("CoV(fME)")
    if (length(grep("CoV", names(res))) == 2) {
      names(res)[grep("CoV", names(res))[2]] = c("CoV(NLM)")
    }
    print(res[which(res$is.terminal.node == TRUE), -(ncol(res))], row.names = FALSE)
    cat("---\n")
    cat("cANLM:  ≤0 implies non-linearity, 1 implies linearity\n\n")
    cat(paste0("AME (Global): ", round(partitioning$object$ame, 4)))
    if ("nlm" %in% names(partitioning$object$results)) {
      cat(paste0("\nANLM (Global): ", round(partitioning$object$anlm, 2)))
    }
    cat("\n\n")
  }
}

#' Prints an FME Partitioning.
#' @param partitioning object of class `Partitioning`.
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
  # empty object
  if (partitioning$computed == FALSE) {
    cat("\n<This is a Partitioning object without results>\n\n")
  } else {
    # non-empty object
    res = do.call(rbind.data.frame, partitioning$results)
    names(res)[grep("CoV", names(res))[1]] = c("CoV(fME)")
    if (length(grep("CoV", names(res))) == 2) {
      names(res)[grep("CoV", names(res))[2]] = c("CoV(NLM)")
    }
    print(res[which(res$is.terminal.node == TRUE), -(ncol(res))], row.names = FALSE)
    cat("\n")
  }
}

#' Plots an FME Partitioning.
#' @param partitioning object of class `Partitioning`.
#' @export
plot.Partitioning = function(partitioning) {
  partitioning$plot()
}
