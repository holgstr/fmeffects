
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

      # Check if results is a data.table with a minimum of one observation
      checkmate::assertDataTable(results, min.rows = 1)

      self$feature = feature
      self$step.size = step.size

      results = data.table::copy(results)
      add = data[, .SD, .SDcols = self$feature]
      add = add[i = results$obs.id]
      self$df = cbind(results, add)

    }
  )
)


# FMEPlot for Bivariate Numerical Steps
FMEPlotBivariate = R6::R6Class("FMEPlotBivariate",

  inherit = FMEPlot,

  public = list(

    initialize = function(results, data, feature, step.size) {
      private$initializeSubclass(results, data, feature, step.size)
    },

    plot = function(with.nlm = FALSE, jitter) {
      checkmate::assertNumeric(jitter, len = 2)
      df = as.data.frame(self$df)
      x1 = df[,which(self$feature[1] == names(df))]
      range.x1 = diff(range(x1))
      x2 = df[,which(self$feature[2] == names(df))]
      range.x2 = diff(range(x2))

      pfme = ggplot2::ggplot(df) +
        ggplot2::geom_jitter(ggplot2::aes(x = x1, y = x2, fill = fme),
                   size = 2.8,
                   shape = 21,
                   alpha = 0.6,
                   #position = "identity") +
                   width = jitter[1],
                   height = jitter[2]) +
        ggplot2::scale_fill_viridis_c(ggplot2::guide_legend("FME")) +
        ggplot2::geom_segment(ggplot2::aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * self$step.size[1]),
                         xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * self$step.size[1]),
                         y = min(x2)-0.03*range.x2,
                         yend = min(x2)-0.03*range.x2),
                     colour = 'black', size = 1,
                     arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
                     lineend = "round", linejoin = "mitre") +
        ggplot2::geom_segment(ggplot2::aes(y = (0.5 * min(x2) + 0.5 * max(x2) - 0.5 * self$step.size[2]),
                         yend = (0.5 * min(x2) + 0.5 * max(x2) + 0.5 * self$step.size[2]),
                         x = min(x1)-0.03*range.x1,
                         xend = min(x1)-0.03*range.x1),
                     colour = 'black', size = 1,
                     arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
                     lineend = "round", linejoin = "mitre") +
        ggplot2::xlab(self$feature[1]) +
        ggplot2::ylab(self$feature[2]) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
              axis.title = ggplot2::element_text(size = 12),
              axis.text.x   = ggplot2::element_text(colour = "black", size = 10),
              axis.text.y   = ggplot2::element_text(colour = "black", size = 10),
              legend.title = ggplot2::element_text(color = "black", size = 12),
              legend.text = ggplot2::element_text(color = "black", size = 10))

      if (with.nlm == FALSE) {
        pfme
      } else if ("nlm" %in% names(df)) {
        df$nlm = sapply(df$nlm, FUN = function(x) {max(x, 0)})
        pnlm = ggplot2::ggplot(df) +
          ggplot2::geom_jitter(ggplot2::aes(x = x1, y = x2, fill = nlm),
                     size = 2.8,
                     shape = 21,
                     alpha = 0.6,
                     width = jitter[1],
                     height = jitter[2]) +
          ggplot2::scale_fill_viridis_c(ggplot2::guide_legend("NLM"),
                               breaks=c(0.98, 0.5, 0.01),
                               labels = c("1.0", "0.5", "\u2264 0")) +
          ggplot2::geom_segment(ggplot2::aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * self$step.size[1]),
                           xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * self$step.size[1]),
                           y = min(x2)-0.03*range.x2, yend = min(x2)-0.03*range.x2),
                       colour = 'black', size = 1, arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
                       lineend = "round", linejoin = "mitre") +
          ggplot2::geom_segment(ggplot2::aes(y = (0.5 * min(x2) + 0.5 * max(x2) - 0.5 * self$step.size[2]),
                           yend = (0.5 * min(x2) + 0.5 * max(x2) + 0.5 * self$step.size[2]),
                           x = min(x1)-0.03*range.x1, xend = min(x1)-0.03*range.x1),
                       colour = 'black', size = 1,
                       arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
                       lineend = "round", linejoin = "mitre") +
          ggplot2::xlab(self$feature[1]) +
          ggplot2::ylab(self$feature[2]) +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
                axis.title = ggplot2::element_text(size = 12),
                axis.text.x   = ggplot2::element_text(colour = "black", size = 10),
                axis.text.y   = ggplot2::element_text(colour = "black", size = 10),
                legend.title = ggplot2::element_text(color = "black", size = 12),
                legend.text = ggplot2::element_text(color = "black", size = 10))
        suppressWarnings(plot_grid(pfme, pnlm, ncol = 2, rel_widths = c(0.5, 0.5)))
      } else {
        stop("Only possible to plot NLM for FME objects with NLM computed.")
      }
    }
  )
)

# FMEPlot for Univariate Numerical Steps
FMEPlotUnivariate = R6::R6Class("FMEPlotUnivariate",

  inherit = FMEPlot,

  public = list(

    initialize = function(results, data, feature, step.size) {
      private$initializeSubclass(results, data, feature, step.size)
    },

    plot = function(with.nlm = FALSE, jitter) {
      checkmate::assertNumeric(jitter, len = 2)
      df = as.data.frame(self$df)
      names(df)[which(names(df) == self$feature)] = "x1"
      range.x1 = diff(range(df$x1))
      min.x1 = min(df$x1)
      max.x1 = max(df$x1)
      range.fme = diff(range(df$fme))

      pfme = ggplot2::ggplot(df) +
        ggplot2::geom_jitter(ggplot2::aes(x = x1, y = fme),
                   colour = "black",
                   fill= "#1E9B8AFF",
                   size = 2.8,
                   shape = 21,
                   alpha = 0.5,
                   width = jitter[1],
                   height = jitter[2]) +
        ggplot2::geom_smooth(ggplot2::aes(x = x1, y = fme), se = TRUE, method = "gam", fullrange = TRUE, linetype = "solid", linewidth = 0.7, color = "black") +
        ggplot2::geom_segment(ggplot2::aes(x = (0.5 * min.x1 + 0.5 * max.x1 - 0.5 * self$step.size[1]),
                         xend = (0.5 * min.x1 + 0.5 * max.x1 + 0.5 * self$step.size[1]),
                         y = min(fme)-0.03*range.fme,
                         yend = min(fme)-0.03*range.fme),
                     colour = 'black', size = 1,
                     arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
                     lineend = "round", linejoin = "mitre") +
        ggplot2::geom_hline(lwd = 1.2, mapping = ggplot2::aes(yintercept = mean(fme))) +
        ggplot2::geom_label(x = max(df$x1) - 0.2 * range.x1, y = mean(df$fme), label = paste0('AME: ', round(mean(df$fme), 4)), fill = 'white') +
        ggplot2::xlab(self$feature[1]) +
        ggplot2::ylab("FME") +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
              axis.title = ggplot2::element_text(size = 12),
              axis.text.x   = ggplot2::element_text(colour = "black", size = 10),
              axis.text.y   = ggplot2::element_text(colour = "black", size = 10),
              legend.title = ggplot2::element_text(color = "black", size = 12),
              legend.text = ggplot2::element_text(color = "black", size = 10))

      if (with.nlm == FALSE) {
        pfme
      } else if ("nlm" %in% names(df)) {
        meannlm = mean(df$nlm, na.rm = TRUE)
        df$nlm = sapply(df$nlm, FUN = function(x) {max(x, 0, na.rm = TRUE)})
        range.nlm = diff(range(df$nlm, na.rm = FALSE))
        pnlm = ggplot2::ggplot(df) +
          ggplot2::geom_jitter(ggplot2::aes(x = x1, y = nlm, fill = cut(nlm, c(-Inf, 0.0001, Inf))),
                     colour = "black",
                     size = 2.8,
                     shape = 21,
                     alpha = 0.5,
                     width = jitter[1],
                     height = jitter[2],
                     show.legend = FALSE,
                     na.rm = FALSE) +
          ggplot2::geom_segment(ggplot2::aes(x = (0.5 * min.x1 + 0.5 * max.x1 - 0.5 * self$step.size[1]),
                           xend = (0.5 * min.x1 + 0.5 * max.x1 + 0.5 * self$step.size[1]),
                           y = min(nlm, na.rm = FALSE)-0.03*range.nlm,
                           yend = min(nlm, na.rm = FALSE)-0.03*range.nlm),
                       colour = 'black', size = 1,
                       arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
                       lineend = "round", linejoin = "mitre",
                       na.rm = FALSE) +
          ggplot2::geom_hline(lwd = 1.2, mapping = ggplot2::aes(yintercept = meannlm)) +
          ggplot2::geom_label(x = max.x1 - 0.2 * range.x1, y = meannlm, label = paste0('ANLM: ', round(meannlm, 2)), fill = 'white') +
          ggplot2::xlab(self$feature) +
          ggplot2::ylab("NLM") +
          ggplot2::theme_bw() +
          ggplot2::scale_fill_manual(values = c("aliceblue", "#1E9B8AFF")) +
          scale_y_continuous(breaks=seq(0, 1.0, 0.25),
                             labels=c("\u2264 0", as.character(seq(0.25, 1, 0.25)))) +
          ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
                axis.title = ggplot2::element_text(size = 12),
                axis.text.x   = ggplot2::element_text(colour = "black", size = 10),
                axis.text.y   = ggplot2::element_text(colour = "black", size = 10))
        plot_grid(pfme, pnlm, ncol = 2, rel_widths = c(0.5, 0.5))
      } else {
        stop("Only possible to plot NLM for FME objects with NLM computed.")
      }
    }
  )
)

# FMEPlot for Categorical Steps
FMEPlotCategorical = R6::R6Class("FMEPlotCategorical",

  inherit = FMEPlot,

  public = list(

    initialize = function(results, data, feature, step.size) {
      private$initializeSubclass(results, data, feature, step.size)
    },

    plot = function(with.nlm = FALSE, jitter) {
      checkmate::assertNumeric(jitter, len = 2)
      if (!(jitter[1] == 0 & jitter[2] == 0)) {
        stop("You supplied an invalid argument (jitter). Jittering is not possible for categorical steps.")
      }
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
                         fill = "#1E9B8AFF",
                         show.legend = FALSE,
                         mapping = ggplot2::aes(x = fme, y = ggplot2::after_stat(count)),
                         bins = min(round(nrow(df))*0.4, 20),
                         na.rm = TRUE) +
          ggplot2::geom_density(mapping = ggplot2::aes(x = fme, y = ..scaled..*countmax), adjust = 1.5) +
          ggplot2::geom_vline(lwd = 1.2, mapping = ggplot2::aes(xintercept = mean(fme))) +
          ggplot2::geom_label(x = mean(df$fme), y = countmax*0.9, label = paste0('AME: ', round(mean(df$fme), 4)), fill = 'white') +
          ggplot2::xlab(paste0("FME (category: ", self$step.size, ", feature: ", self$feature, ")")) +
          ggplot2::ylab("") +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
                axis.title = ggplot2::element_text(size = 12),
                axis.text.x   = ggplot2::element_text(colour = "black", size = 10),
                axis.text.y   = ggplot2::element_text(colour = "black", size = 10))
      } else {
        stop("Cannot plot NLM because NLM can only be computed for numerical features.")
      }
    }
  )
)
