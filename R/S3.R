### FME


#' Prints summary of an ForwardMarginalEffect object.
#' @param object object of class `ForwardMarginalEffect`.
#' @param ... additional arguments affecting the summary produced.
#' @export
summary.ForwardMarginalEffect = function(object, ...) {
  fme = object
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
    cat("\n\n<<<This is an ForwardMarginalEffect object without results>>>\n")
    cat("<<<Call $compute() to compute FMEs>>>\n\n")
  } else {
    cat(paste0(", EPs: ", length(fme$extrapolation.ids), " of ",
               nrow(fme$results) + length(fme$extrapolation.ids), " obs. (",
               round(length(fme$extrapolation.ids)/nrow(fme$predictor$X)*100), " %)\n\n"))
    cat(paste0("Average Marginal Effect (AME):\n  ", round(fme$ame, 4)))
    if ("nlm" %in% names(fme$results)) {
      cat(paste0("\n\nAverage Non-Linearity Measure (ANLM):\n  ", round(fme$anlm, 2),
                 "  (\u2264 0 implies non-linearity, 1 implies linearity)"))
    }
  }
}

#' Prints an ForwardMarginalEffect object.
#' @param x object of class `ForwardMarginalEffect`.
#' @param ... additional arguments affecting the summary produced.
#' @export
print.ForwardMarginalEffect = function(x, ...) {
  fme = x
  cat("\n")
  cat("Forward Marginal Effects Object\n\n")
  cat(paste0("Features & step lengths:\n"))
  for (i in seq_len(length(fme$feature))) {
    cat(paste0("  ", fme$feature[i], ", ", fme$step.size[i], "\n"))
  }
  # empty object
  if (fme$computed == FALSE) {
    cat("\n<This is an ForwardMarginalEffect object without results>\n\n")
  } else {
    # non-empty object
    cat(paste0("\nAverage Marginal Effect (AME):\n  ", round(fme$ame, 4)))
    if ("nlm" %in% names(fme$results)) {
      cat(paste0("\n\nAverage Non-Linearity Measure (ANLM):\n  ", round(fme$anlm, 2),
                 "  (\u2264 0 implies non-linearity, 1 implies linearity)"))
    }
  }
}

#' Plots an ForwardMarginalEffect object.
#' @param x object of class `ForwardMarginalEffect`.
#'   See the method `$plot()` in [fmeffects::ForwardMarginalEffect()] for details.
#' @param ... additional arguments affecting the summary produced.
#' @export
plot.ForwardMarginalEffect = function(x, ...) {
  x$plot(...)
}


### PARTITIONING


#' Prints summary of an FME Partitioning.
#' @param object object of class `Partitioning`.
#' @param ... additional arguments affecting the summary produced.
#' @export
summary.Partitioning = function(object, ...) {
  partitioning = object
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
    names(res)[grep("SD", names(res))[1]] = c("SD(fME)")
    if (length(grep("SD", names(res))) == 2) {
      names(res)[grep("SD", names(res))[2]] = c("SD(NLM)")
    }

    res = res[c(1,which(res$is.terminal.node == TRUE)), -(ncol(res))]
    res$" " = c("*", rep("", nrow(res) - 1))
    print(res, row.names = FALSE)
    cat("---\n")
    cat("* root node (non-partitioned)")
    if ("nlm" %in% names(partitioning$object$results)) {
      cat("\ncANLM: \u2264 0 implies non-linearity, 1 implies linearity")
    }
    cat(paste0("\n\nAME (Global): ", round(partitioning$object$ame, 4)))
    if ("nlm" %in% names(partitioning$object$results)) {
      cat(paste0("\nANLM (Global): ", round(partitioning$object$anlm, 2)))
    }
    cat("\n\n")
  }
}

#' Prints an FME Partitioning.
#' @param x object of class `Partitioning`.
#' @param ... additional arguments affecting the summary produced.
#' @export
print.Partitioning = function(x, ...) {
  partitioning = x
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
    names(res)[grep("SD", names(res))[1]] = c("SD(fME)")
    if (length(grep("SD", names(res))) == 2) {
      names(res)[grep("SD", names(res))[2]] = c("SD(NLM)")
    }
    print(res[which(res$is.terminal.node == TRUE), -(ncol(res))], row.names = FALSE)
    cat("\n")
  }
}

#' Plots an FME Partitioning.
#' @param x object of class `Partitioning`.
#' @param ... additional arguments affecting the summary produced.
#' @export
plot.Partitioning = function(x, ...) {
  x$plot()
}


### AME


#' Prints summary of an AverageMarginalEffects object.
#' @param object object of class `AverageMarginalEffects`.
#' @param ... additional arguments affecting the summary produced.
#' @export
summary.AverageMarginalEffects = function(object, ...) {
  cat("\n")
  cat("Model Summary Using Average Marginal Effects:\n\n")
  print(object$results)
}
