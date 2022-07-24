# Abstract FMEPlot Class
FMEPlot = R6Class("FMEPlot",
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
      assertDataTable(results, min.rows = 1)

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
FMEPlotBivariate = R6Class("FMEPlotBivariate",

  inherit = FMEPlot,

  public = list(

    initialize = function(results, data, feature, step.size) {
      private$initializeSubclass(results, data, feature, step.size)
    },

    plot = function(with.nlm = FALSE) {
      df = as.data.frame(self$df)
      x1 = df[,which(self$feature[1] == names(df))]
      range.x1 = diff(range(x1))
      x2 = df[,which(self$feature[2] == names(df))]
      range.x2 = diff(range(x2))

      pfme = ggplot(df) +
        geom_point(aes(x = x1, y = x2, fill = fme),
                   size = 3.8,
                   shape = 21,
                   alpha = 0.7) +
        scale_fill_viridis_c(guide_legend("fME")) +
        geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * self$step.size[1]),
                         xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * self$step.size[1]),
                         y = min(x2)-0.03*range.x2,
                         yend = min(x2)-0.03*range.x2),
                     colour = 'black', size = 1,
                     arrow = arrow(length = unit(0.5, "cm")),
                     lineend = "round", linejoin = "mitre") +
        geom_segment(aes(y = (0.5 * min(x2) + 0.5 * max(x2) - 0.5 * self$step.size[2]),
                         yend = (0.5 * min(x2) + 0.5 * max(x2) + 0.5 * self$step.size[2]),
                         x = min(x1)-0.03*range.x1,
                         xend = min(x1)-0.03*range.x1),
                     colour = 'black', size = 1,
                     arrow = arrow(length = unit(0.5, "cm")),
                     lineend = "round", linejoin = "mitre") +
        xlab(self$feature[1]) +
        ylab(self$feature[2]) +
        theme_bw() +
        theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7),
              axis.title = element_text(size = 12),
              axis.text.x   = element_text(colour = "black", size = 10),
              axis.text.y   = element_text(colour = "black", size = 10),
              legend.title = element_text(color = "black", size = 12),
              legend.text = element_text(color = "black", size = 10))

      if (with.nlm == FALSE) {
        pfme
      } else if ("nlm" %in% names(df)) {
        df$nlm = sapply(df$nlm, FUN = function(x) {max(x, 0)})
        pnlm = ggplot(df) +
          geom_point(aes(x = x1, y = x2, fill = nlm),
                     size = 3.8,
                     shape = 21,
                     alpha = 0.7) +
          scale_fill_viridis_c(guide_legend("NLM"),
                               breaks=c(0.98, 0.5, 0.01),
                               labels = c("1.0", "0.5", "\u2264 0")) +
          geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * self$step.size[1]),
                           xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * self$step.size[1]),
                           y = min(x2)-0.03*range.x2, yend = min(x2)-0.03*range.x2),
                       colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm")),
                       lineend = "round", linejoin = "mitre") +
          geom_segment(aes(y = (0.5 * min(x2) + 0.5 * max(x2) - 0.5 * self$step.size[2]),
                           yend = (0.5 * min(x2) + 0.5 * max(x2) + 0.5 * self$step.size[2]),
                           x = min(x1)-0.03*range.x1, xend = min(x1)-0.03*range.x1),
                       colour = 'black', size = 1,
                       arrow = arrow(length = unit(0.5, "cm")),
                       lineend = "round", linejoin = "mitre") +
          xlab(self$feature[1]) +
          ylab(self$feature[2]) +
          theme_bw() +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7),
                axis.title = element_text(size = 12),
                axis.text.x   = element_text(colour = "black", size = 10),
                axis.text.y   = element_text(colour = "black", size = 10),
                legend.title = element_text(color = "black", size = 12),
                legend.text = element_text(color = "black", size = 10))
        suppressWarnings(plot_grid(pfme, pnlm, ncol = 2, rel_widths = c(0.5, 0.5)))
      } else {
        stop("Only possible to plot NLM for FME objects with NLM computed.")
      }
    }
  )
)

# FMEPlot for Univariate Numerical Steps
FMEPlotUnivariate = R6Class("FMEPlotUnivariate",

  inherit = FMEPlot,

  public = list(

    initialize = function(results, data, feature, step.size) {
      private$initializeSubclass(results, data, feature, step.size)
    },

    plot = function(with.nlm = FALSE) {
      df = as.data.frame(self$df)
      x1 = df[,which(self$feature == names(df))]
      range.x1 = diff(range(x1))
      range.fme = diff(range(df$fme))

      pfme = ggplot(df) +
        geom_point(aes(x = x1, y = fme),
                   colour = "black",
                   fill= "#1E9B8AFF",
                   size = 3.8,
                   shape = 21,
                   alpha = 0.55) +
        geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * self$step.size[1]),
                         xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * self$step.size[1]),
                         y = min(fme)-0.03*range.fme,
                         yend = min(fme)-0.03*range.fme),
                     colour = 'black', size = 1,
                     arrow = arrow(length = unit(0.5, "cm")),
                     lineend = "round", linejoin = "mitre") +
        geom_hline(lwd = 1.2, mapping = aes(yintercept = mean(fme))) +
        geom_label(x = max(x1) - 0.05 * range.x1, y = mean(df$fme), label = 'AME', fill = 'white') +
        xlab(self$feature[1]) +
        ylab("fME") +
        theme_bw() +
        theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7),
              axis.title = element_text(size = 12),
              axis.text.x   = element_text(colour = "black", size = 10),
              axis.text.y   = element_text(colour = "black", size = 10),
              legend.title = element_text(color = "black", size = 12),
              legend.text = element_text(color = "black", size = 10))

      if (with.nlm == FALSE) {
        pfme
      } else if ("nlm" %in% names(df)) {
        meannlm = mean(df$nlm, na.rm = TRUE)
        df$nlm = sapply(df$nlm, FUN = function(x) {max(x, 0, na.rm = TRUE)})
        range.nlm = diff(range(df$nlm, na.rm = FALSE))
        pnlm = ggplot(df) +
          geom_point(aes(x = x1, y = nlm, fill = cut(nlm, c(-Inf, 0.0001, Inf))),
                     colour = "black",
                     size = 3.8,
                     shape = 21,
                     alpha = 0.55,
                     show.legend = FALSE,
                     na.rm = FALSE) +
          geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * self$step.size[1]),
                           xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * self$step.size[1]),
                           y = min(nlm, na.rm = FALSE)-0.03*range.nlm,
                           yend = min(nlm, na.rm = FALSE)-0.03*range.nlm),
                       colour = 'black', size = 1,
                       arrow = arrow(length = unit(0.5, "cm")),
                       lineend = "round", linejoin = "mitre",
                       na.rm = FALSE) +
          geom_hline(lwd = 1.2, mapping = aes(yintercept = meannlm)) +
          geom_label(x = max(x1) - 0.05 * range.x1, y = meannlm, label = 'ANLM', fill = 'white') +
          xlab(self$feature) +
          ylab("NLM") +
          theme_bw() +
          scale_fill_manual(values = c("aliceblue", "#1E9B8AFF")) +
          scale_y_continuous(breaks=seq(0, 1.0, 0.25),
                             labels=c("\u2264 0", as.character(seq(0.25, 1, 0.25)))) +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7),
                axis.title = element_text(size = 12),
                axis.text.x   = element_text(colour = "black", size = 10),
                axis.text.y   = element_text(colour = "black", size = 10))
        plot_grid(pfme, pnlm, ncol = 2, rel_widths = c(0.5, 0.5))
      } else {
        stop("Only possible to plot NLM for FME objects with NLM computed.")
      }
    }
  )
)

# FMEPlot for Categorical Steps
FMEPlotCategorical = R6Class("FMEPlotCategorical",

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
        ggplot(df) +
          geom_histogram(lwd = 0.3,
                         linetype = "solid",
                         colour = "black",
                         fill = "#1E9B8AFF",
                         show.legend = FALSE,
                         mapping = aes(x = fme, y = ..count..),
                         bins = min(round(nrow(df))*0.4, 20),
                         na.rm = TRUE) +
          geom_density(mapping = aes(x = fme, y = ..scaled..*countmax), adjust = 1.5) +
          geom_vline(lwd = 1.2, mapping = aes(xintercept = mean(fme))) +
          geom_label(x = mean(df$fme), y = countmax*0.9, label = 'AME', fill = 'white') +
          xlab(paste0("fME (category: ", self$step.size, ", feature: ", self$feature, ")")) +
          ylab("") +
          theme_bw() +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7),
                axis.title = element_text(size = 12),
                axis.text.x   = element_text(colour = "black", size = 10),
                axis.text.y   = element_text(colour = "black", size = 10))
      } else {
        stop("Cannot plot NLM because NLM can only be computed for numerical features.")
      }
    }
  )
)
