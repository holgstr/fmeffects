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
      add = data[, .SD, .SDcols = feature]
      add = add[i = results$obs.id]
      results = cbind(results, add)
      self$df = results

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
      x1 = df[,which(feature[1] == names(df))]
      range.x1 = diff(range(x1))
      x2 = df[,which(feature[2] == names(df))]
      range.x2 = diff(range(x2))

      pfme = ggplot(df) +
        geom_point(aes(x = x1, y = x2, fill = fme),
                   size = 3.8,
                   shape = 21,
                   alpha = 0.7) +
        scale_fill_viridis_c(guide_legend("fME")) +
        geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * step.size[1]),
                         xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * step.size[1]),
                         y = min(x2)-0.03*range.x2,
                         yend = min(x2)-0.03*range.x2),
                     colour = 'black', size = 1,
                     arrow = arrow(length = unit(0.5, "cm")),
                     lineend = "round", linejoin = "mitre") +
        geom_segment(aes(y = (0.5 * min(x2) + 0.5 * max(x2) - 0.5 * step.size[2]),
                         yend = (0.5 * min(x2) + 0.5 * max(x2) + 0.5 * step.size[2]),
                         x = min(x1)-0.03*range.x1,
                         xend = min(x1)-0.03*range.x1),
                     colour = 'black', size = 1,
                     arrow = arrow(length = unit(0.5, "cm")),
                     lineend = "round", linejoin = "mitre") +
        xlab(feature[1]) +
        ylab(feature[2]) +
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
          geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * step.size[1]),
                           xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * step.size[1]),
                           y = min(x2)-0.03*range.x2, yend = min(x2)-0.03*range.x2),
                       colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm")),
                       lineend = "round", linejoin = "mitre") +
          geom_segment(aes(y = (0.5 * min(x2) + 0.5 * max(x2) - 0.5 * step.size[2]),
                           yend = (0.5 * min(x2) + 0.5 * max(x2) + 0.5 * step.size[2]),
                           x = min(x1)-0.03*range.x1, xend = min(x1)-0.03*range.x1),
                       colour = 'black', size = 1,
                       arrow = arrow(length = unit(0.5, "cm")),
                       lineend = "round", linejoin = "mitre") +
          xlab(feature[1]) +
          ylab(feature[2]) +
          theme_bw() +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7),
                axis.title = element_text(size = 12),
                axis.text.x   = element_text(colour = "black", size = 10),
                axis.text.y   = element_text(colour = "black", size = 10),
                legend.title = element_text(color = "black", size = 12),
                legend.text = element_text(color = "black", size = 10))
        plot_grid(pfme, pnlm, ncol = 2, rel_widths = c(0.5, 0.5))
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
      x1 = df[,which(feature == names(df))]
      range.fme = diff(range(df$fme))

      pfme = ggplot(df) +
        geom_point(aes(x = x1, y = fme),
                   colour = "black",
                   fill= "cornflowerblue",
                   size = 3.8,
                   shape = 21,
                   alpha = 0.55) +
        geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * step.size[1]),
                         xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * step.size[1]),
                         y = min(fme)-0.03*range.fme,
                         yend = min(fme)-0.03*range.fme),
                     colour = 'black', size = 1,
                     arrow = arrow(length = unit(0.5, "cm")),
                     lineend = "round", linejoin = "mitre") +
        xlab(feature[1]) +
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
        df$nlm = sapply(df$nlm, FUN = function(x) {max(x, 0)})
        range.nlm = diff(range(df$nlm))
        pnlm = ggplot(df) +
          geom_point(aes(x = x1, y = nlm, fill = cut(nlm, c(-Inf, 0.0001, Inf))),
                     colour = "black",
                     size = 3.8,
                     shape = 21,
                     alpha = 0.55,
                     show.legend = FALSE) +
          geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * step.size[1]),
                           xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * step.size[1]),
                           y = min(nlm)-0.03*range.nlm,
                           yend = min(nlm)-0.03*range.nlm),
                       colour = 'black', size = 1,
                       arrow = arrow(length = unit(0.5, "cm")),
                       lineend = "round", linejoin = "mitre") +
          xlab(feature) +
          ylab("NLM") +
          theme_bw() +
          scale_fill_manual(values = c("aliceblue", "cornflowerblue")) +
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
