
# Abstract FMEPlot Class
FMEPlot = R6::R6Class("FMEPlot",
  public = list(

    initialize = function(...) {
      stop(paste(class(self)[1], "is an abstract class that cannot be initialized."))
    },

    feature = NULL,
    step.size = NULL,
    df = NULL

  ),
  private = list(

    initializeSubclass = function(results, data, feature, step.size) {

      if (checkmate::test_true(length(unique(results$fme)) == 1)) {
        cli::cli_abort(paste("Cannot plot effects if they all have the same value."))
      }

      self$feature = feature
      self$step.size = step.size

      results = data.table::copy(results)
      add = data[, .SD, .SDcols = self$feature]
      add = add[i = results$obs.id]
      self$df = cbind(results, add)

    }
  )
)


# FMEPlot for Higher-Order Numerical Steps
FMEPlotHigherOrder = R6::R6Class(
  "FMEPlotHigherOrder",
  inherit = FMEPlot,

  public = list(

    initialize = function(results, data, feature, step.size) {
      private$initializeSubclass(results, data, feature, step.size)
    },

  plot = function(with.nlm = FALSE) {
        df = as.data.frame(self$df)
        countmax = max(hist(df$fme,
                            breaks = seq(min(df$fme),
                                         max(df$fme),
                                         l=min(round(nrow(df))*0.4, 20)+1),
                            plot = FALSE)$counts)
        pfme <- ggplot2::ggplot(df) +
          ggplot2::geom_histogram(lwd = 0.3,
                                  linetype = "solid",
                                  colour = "black",
                                  fill = "gray",
                                  show.legend = FALSE,
                                  mapping = ggplot2::aes(x = fme, y = ggplot2::after_stat(count)),
                                  bins = min(round(nrow(df))*0.4, 20),
                                  na.rm = TRUE) +
          ggplot2::geom_vline(lwd = 1.2, mapping = ggplot2::aes(xintercept = mean(fme))) +
          ggplot2::annotate(geom = "label", x = mean(df$fme), y = countmax*0.9, label = paste0('AME: ', round(mean(df$fme), 4)), fill = 'white') +
          ggplot2::xlab(
            paste0("FME (",
                   paste(
                     paste(
                       self$feature, "=", self$step.size), collapse = " | ")
                   ,")")) +
          ggplot2::ylab("") +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
                         axis.title = ggplot2::element_text(size = 12),
                         axis.text.x = ggplot2::element_text(colour = "black", size = 10),
                         axis.text.y = ggplot2::element_text(colour = "black", size = 10))

        if (with.nlm == FALSE) {
          pfme
        } else if ("nlm" %in% names(df)) {
          df$nlm = sapply(df$nlm, FUN = function(x) {max(x, 0)})
          pnlm <-  ggplot2::ggplot(df) +
            ggplot2::geom_histogram(lwd = 0.3,
                                    linetype = "solid",
                                    colour = "black",
                                    fill = "gray",
                                    show.legend = FALSE,
                                    mapping = ggplot2::aes(x = nlm, y = ggplot2::after_stat(count)),
                                    bins = min(round(nrow(df))*0.4, 20),
                                    na.rm = TRUE) +
            ggplot2::geom_vline(lwd = 1.2, mapping = ggplot2::aes(xintercept = mean(nlm))) +
            ggplot2::annotate(geom = "label", x = mean(df$nlm), y = countmax*0.9, label = paste0('ANLM: ', round(mean(df$nlm), 4)), fill = 'white') +
            ggplot2::xlab(
              paste0("NLM (",
                     paste(
                       paste(
                         self$feature, "=", self$step.size), collapse = " | ")
                     ,")")) +
            ggplot2::ylab("") +
            ggplot2::theme_bw() +
            ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
                           axis.title = ggplot2::element_text(size = 12),
                           axis.text.x = ggplot2::element_text(colour = "black", size = 10),
                           axis.text.y = ggplot2::element_text(colour = "black", size = 10))
          suppressWarnings(cowplot::plot_grid(pfme, pnlm, ncol = 2, rel_widths = c(0.5, 0.5)))
        } else {
          stop("Only possible to plot NLM for FME objects with NLM computed.")
        }
  }
  )
)

# FMEPlot for Bivariate Numerical Steps
FMEPlotBivariate = R6::R6Class(
  "FMEPlotBivariate",

  inherit = FMEPlot,

  public = list(

    initialize = function(results, data, feature, step.size) {
      private$initializeSubclass(results, data, feature, step.size)
    },

    plot = function(with.nlm = FALSE, bins = 40, binwidth = NULL) {
      checkmate::assertNumber(bins, null.ok = TRUE)
      checkmate::assertNumeric(binwidth, len = 2, null.ok = TRUE)
      df = as.data.frame(self$df)
      x1 = df[,which(self$feature[1] == names(df))]
      range.x1 = diff(range(x1, na.rm = TRUE))
      min.x1 = min(x1, na.rm = TRUE)
      x2 = df[,which(self$feature[2] == names(df))]
      range.x2 = diff(range(x2, na.rm = TRUE))
      min.x2 = min(x2, na.rm = TRUE)
      step.size <- sign(self$step.size) * pmin(abs(self$step.size), c(range.x1, range.x2))

      pfme <- ggplot2::ggplot(df, ggplot2::aes(x = x1, y = x2)) +
        ggplot2::stat_summary_hex(ggplot2::aes(z = fme), fun = mean, bins = bins, binwidth = binwidth) +
        ggplot2::scale_fill_gradient2(
          name = "FME",
          low = "#D55E00", mid = "#ECF4F9", high = "#0072B2",
          midpoint = 0,
          breaks = function(x) {pretty(x, n = 5)}
        ) +
        ggplot2::xlim(min.x1 - 0.06 * range.x1, NA) +
        ggplot2::ylim(min.x2 - 0.06 * range.x2, NA) +
        ggplot2::geom_rug(length = ggplot2::unit(0.015, "npc")) +
        ggplot2::xlab(self$feature[1]) +
        ggplot2::ylab(self$feature[2]) +
        ggplot2::theme_bw() +
        ggplot2::annotate("segment", x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * step.size[1]),
                          xend = (0.5 * min.x1 + 0.5 * max(x1) + 0.5 * step.size[1]),
                          y = min.x2 - 0.06 * range.x2,
                          yend = min.x2 - 0.06 * range.x2,
                          colour = 'black', size = 1,
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                          lineend = "round", linejoin = "mitre") +
        ggplot2::annotate("segment", y = (0.5 * min(x2) + 0.5 * max(x2) - 0.5 * step.size[2]),
                          yend = (0.5 * min(x2) + 0.5 * max(x2) + 0.5 * step.size[2]),
                          x = min.x1 - 0.06 * range.x1,
                          xend = min.x1 - 0.06 * range.x1,
                          colour = 'black', size = 1,
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                          lineend = "round", linejoin = "mitre") +
        ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
                       axis.title = ggplot2::element_text(size = 12),
                       axis.text   = ggplot2::element_text(colour = "black", size = 10),
                       legend.title = ggplot2::element_text(color = "black", size = 12),
                       legend.text = ggplot2::element_text(color = "black", size = 10))

      if (with.nlm == FALSE) {
        pfme
      } else if ("nlm" %in% names(df)) {
        df$nlm = sapply(df$nlm, FUN = function(x) {max(x, 0)})
        pnlm <- ggplot2::ggplot(df, ggplot2::aes(x = x1, y = x2)) +
          ggplot2::stat_summary_hex(ggplot2::aes(z = nlm), fun = mean, bins = bins, binwidth = binwidth) +
          ggplot2::scale_fill_gradient(
            name = "NLM",
            low = "white", high = "#0072B2",
            breaks = c(0, 0.5, 1),
            limits = c(0, 1)
          ) +
          ggplot2::xlim(min.x1 - 0.06 * range.x1, NA) +
          ggplot2::ylim(min.x2 - 0.06 * range.x2, NA) +
          ggplot2::geom_rug(length = ggplot2::unit(0.015, "npc")) +
          ggplot2::xlab(self$feature[1]) +
          ggplot2::ylab(self$feature[2]) +
          ggplot2::theme_bw() +
          ggplot2::annotate("segment", x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * step.size[1]),
                            xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * step.size[1]),
                            y = min(x2)-0.03*range.x2,
                            yend = min(x2)-0.03*range.x2,
                            colour = 'black', size = 1,
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                            lineend = "round", linejoin = "mitre") +
          ggplot2::annotate("segment", y = (0.5 * min(x2) + 0.5 * max(x2) - 0.5 * step.size[2]),
                            yend = (0.5 * min(x2) + 0.5 * max(x2) + 0.5 * step.size[2]),
                            x = min(x1)-0.03*range.x1,
                            xend = min(x1)-0.03*range.x1,
                            colour = 'black', size = 1,
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                            lineend = "round", linejoin = "mitre") +
          ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
                         axis.title = ggplot2::element_text(size = 12),
                         axis.text   = ggplot2::element_text(colour = "black", size = 10),
                         legend.title = ggplot2::element_text(color = "black", size = 12),
                         legend.text = ggplot2::element_text(color = "black", size = 10))
        suppressWarnings(cowplot::plot_grid(pfme, pnlm, ncol = 2, rel_widths = c(0.5, 0.5)))
      } else {
        stop("Only possible to plot NLM for FME objects with NLM computed.")
      }
    }
  )
)

# FMEPlot for Univariate Numerical Steps
FMEPlotUnivariate = R6::R6Class(
  "FMEPlotUnivariate",

  inherit = FMEPlot,

  public = list(

    initialize = function(results, data, feature, step.size) {
      private$initializeSubclass(results, data, feature, step.size)
    },

    plot = function(with.nlm = FALSE, bins = 40, binwidth = NULL) {
      checkmate::assertNumber(bins, null.ok = TRUE)
      checkmate::assertNumeric(binwidth, len = 2, null.ok = TRUE)
      df = as.data.frame(self$df)
      names(df)[which(names(df) == self$feature)] = "x1"
      range.x1 = diff(range(df$x1, na.rm = TRUE))
      min.x1 = min(df$x1, na.rm = TRUE)
      max.x1 = max(df$x1, na.rm = TRUE)
      range.fme = diff(range(df$fme, na.rm = TRUE))
      min.fme = min(df$fme, na.rm = TRUE)

      pfme = ggplot2::ggplot(df, ggplot2::aes(x = x1, y = fme)) +
        ggplot2::stat_summary_hex(ggplot2::aes(z = fme), fun = function(x) {length(x)}, bins = bins, binwidth = binwidth) +
        ggplot2::xlim(NA, max.x1 + 0.15 * range.x1) +
        ggplot2::ylim(min.fme - 0.1 * range.fme, NA) +
        ggplot2::scale_fill_gradient(
          name = "Count",
          low = "gray87", high = "black",
          breaks = function(x) {unique(round(pretty(x, n = 3)))}
        ) +
        ggplot2::geom_rug(sides = "b", length = ggplot2::unit(0.015, "npc")) +
        ggplot2::geom_smooth(ggplot2::aes(x = x1, y = fme), se = FALSE, fullrange = FALSE, linetype = "dashed", linewidth = 0.7, color = "black") +
        ggplot2::annotate("segment",
                          x = 0.5 * min(df$x1) + 0.5 * max(df$x1) - 0.5 * self$step.size[1],
                          xend = 0.5 * min(df$x1) + 0.5 * max(df$x1) + 0.5 * self$step.size[1],
                          y = min.fme - 0.06 * range.fme,
                          yend = min.fme - 0.06 * range.fme,
                          colour = 'black', size = 1,
                          arrow = grid::arrow(length = grid::unit(0.2, "cm")),
                          lineend = "round", linejoin = "mitre") +
        ggplot2::geom_hline(lwd = 1.2, mapping = ggplot2::aes(yintercept = mean(fme, na.rm = TRUE))) +
        ggplot2::annotate(geom = "label", x = max.x1 + 0.1 * range.x1, y = mean(df$fme, na.rm = TRUE), label = "AME", size = 3, fill = 'white') +
        ggplot2::xlab(self$feature[1]) +
        ggplot2::ylab("FME") +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
                       axis.title = ggplot2::element_text(size = 12),
                       axis.text.x = ggplot2::element_text(colour = "black", size = 10),
                       axis.text.y = ggplot2::element_text(colour = "black", size = 10),
                       legend.title = ggplot2::element_text(color = "black", size = 12),
                       legend.text = ggplot2::element_text(color = "black", size = 10))

      if (with.nlm == FALSE) {
        pfme
      } else if ("nlm" %in% names(df)) {
        meannlm = mean(df$nlm, na.rm = TRUE)
        df$nlm = sapply(df$nlm, FUN = function(x) {max(x, 0, na.rm = TRUE)})
        range.nlm = diff(range(df$nlm, na.rm = TRUE))
        min.nlm = min(df$nlm, na.rm = TRUE)
        pnlm = ggplot2::ggplot(df, ggplot2::aes(x = x1, y = nlm)) +
          ggplot2::stat_summary_hex(ggplot2::aes(z = nlm), fun = function(x) {length(x)}, bins = bins, binwidth = binwidth) +
          ggplot2::xlim(NA, max.x1 + 0.18 * range.x1) +
          ggplot2::ylim(-0.1, NA) +
          ggplot2::scale_fill_gradient(
            name = "Count",
            low = "gray87", high = "black",
            breaks = function(x) {unique(round(pretty(x, n = 3)))}
          ) +
          ggplot2::geom_rug(sides = "b", length = ggplot2::unit(0.015, "npc")) +
          ggplot2::annotate("segment",
                            x = 0.5 * min(df$x1) + 0.5 * max(df$x1) - 0.5 * self$step.size[1],
                            xend = 0.5 * min(df$x1) + 0.5 * max(df$x1) + 0.5 * self$step.size[1],
                            y = min.nlm - 0.06 * range.nlm,
                            yend = min.nlm - 0.06 * range.nlm,
                            colour = 'black', size = 1,
                            arrow = grid::arrow(length = grid::unit(0.2, "cm")),
                            lineend = "round", linejoin = "mitre") +
          ggplot2::geom_hline(lwd = 1.2, mapping = ggplot2::aes(yintercept = meannlm)) +
          ggplot2::annotate(geom = "label", x = max(df$x1) + 0.09 * range.x1, y = meannlm, label = "ANLM", size = 3, fill = 'white') +
          ggplot2::xlab(self$feature[1]) +
          ggplot2::ylab("NLM") +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
                         axis.title = ggplot2::element_text(size = 12),
                         axis.text.x = ggplot2::element_text(colour = "black", size = 10),
                         axis.text.y = ggplot2::element_text(colour = "black", size = 10),
                         legend.title = ggplot2::element_text(color = "black", size = 12),
                         legend.text = ggplot2::element_text(color = "black", size = 10))
        cowplot::plot_grid(pfme, pnlm, ncol = 2, rel_widths = c(0.5, 0.5))
      } else {
        stop("Only possible to plot NLM for FME objects with NLM computed.")
      }
    }
  )
)

# FMEPlot for Categorical Steps
FMEPlotCategorical = R6::R6Class(
  "FMEPlotCategorical",

  inherit = FMEPlot,

  public = list(

    initialize = function(results, data, feature, step.size) {
      private$initializeSubclass(results, data, feature, step.size)
    },

    plot = function(with.nlm = FALSE) {
      if (with.nlm == FALSE) {
        df = as.data.frame(self$df)
        countmax = max(hist(df$fme,
                            breaks = seq(min(df$fme),
                                         max(df$fme),
                                         l=min(round(nrow(df))*0.4, 20)+1),
                            plot = FALSE)$counts)
        ggplot2::ggplot(df) +
          ggplot2::geom_histogram(lwd = 0.3,
                                  linetype = "solid",
                                  colour = "black",
                                  fill = "gray",
                                  show.legend = FALSE,
                                  mapping = ggplot2::aes(x = fme, y = ggplot2::after_stat(count)),
                                  bins = min(round(nrow(df))*0.4, 20),
                                  na.rm = TRUE) +
          ggplot2::geom_vline(lwd = 1.2, mapping = ggplot2::aes(xintercept = mean(fme))) +
          ggplot2::annotate(geom = "label", x = mean(df$fme), y = countmax*0.9, label = paste0('AME: ', round(mean(df$fme), 4)), fill = 'white') +
          ggplot2::xlab(paste0("FME (category: ", self$step.size, ", feature: ", self$feature, ")")) +
          ggplot2::ylab("") +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
                         axis.title = ggplot2::element_text(size = 12),
                         axis.text.x = ggplot2::element_text(colour = "black", size = 10),
                         axis.text.y = ggplot2::element_text(colour = "black", size = 10))
      } else {
        stop("Cannot plot NLM because NLM can only be computed for numerical features.")
      }
    }
  )
)
