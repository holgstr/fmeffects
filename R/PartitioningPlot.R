PartitioningPlot = R6::R6Class("PartitioningPlot",
  public = list(

    initialize = function(tree, object, has.nlm = TRUE) {

      # Check if tree is of class 'party'
      checkmate::assertClass(tree, classes = "party")

      # Check if has.nlm is logical of length 1
      checkmate::assertLogical(has.nlm, len = 1)

      self$tree = tree
      self$has.nlm = has.nlm
      self$plot = private$compute(self$tree, object, self$has.nlm)

    },

    tree = NULL,
    has.nlm = NULL,
    plot = NULL
  ),
  private = list(

    compute = function(tree, object, has.nlm) {
      # Create mean fme grouped by terminal node
      dt = as.data.table(partykit::data_party(tree))
      dt[, c := mean(fme), by = `(fitted)`]
      tree$data$fmemean = dt$c

      # Plot for tree without nlm computed
      if (has.nlm == FALSE) {
        p = ggparty::ggparty(tree,
                    terminal_space = 0.5,
                    add_vars = list(cAME =
                                  function(data, node) {
                                    list(round(mean(node$data$fme), 2))
                                  },
                                cAME_sd =
                                  function(data, node) {
                                    list(round(sd(node$data$fme), 2))
                                  }
                   )) +
          ggparty::geom_edge() +
          ggparty::geom_edge_label() +
          ggparty::geom_node_splitvar() +
          ggparty::geom_node_label(ggplot2::aes(label = paste0("n = ", nodesize, "\ncAME = ", cAME, "\nSD(FME) = ", cAME_sd)),
                          fontface = "bold",
                          ids = "terminal",
                          size = 2.5,
                          nudge_y = -0.05) +
          ggparty::geom_node_plot(gglist = list(ggplot2::geom_histogram(lwd = 0.3,
                                                      linetype = "solid",
                                                      colour = "black",
                                                      fill = "#1E9B8AFF",
                                                      show.legend = FALSE,
                                                      mapping = ggplot2::aes(x = fme, y = ggplot2::after_stat(count)),
                                                      bins = 11),
                                       ggplot2::xlab("FME"),
                                       ggplot2::ylab(""),
                                       #geom_density(lwd = 0.4,
                                       #mapping = ggplot2::aes(x = fme, y = ..scaled..)),
                                       ggplot2::geom_vline(lwd = 1.2, mapping = ggplot2::aes(xintercept = fmemean)),
                                       ggplot2::theme_bw(),
                                       ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
                                             axis.title = ggplot2::element_text(size = 10.5),
                                             axis.text.x   = ggplot2::element_text(colour = "black"),
                                             axis.text.y   = ggplot2::element_text(colour = "black"))),
                         height = 0.68,
                         nudge_x = -0.028,
                         nudge_y = -0.14) +
          ggplot2::theme(legend.position = "none")
      } else {
        # Plot for tree with nlm computed

        tree$data$nlm = object$results$nlm
        dt$nlm = object$results$nlm
        dt[, d := mean(nlm, na.rm = TRUE), by = `(fitted)`]
        tree$data$nlmmean = dt$d
        tree$data$nlmplot = tree$data$nlm
        tree$data$nlmplot[tree$data$nlm <= 0.05 ] = 0
        tree$data$iszero = FALSE
        tree$data$iszero[tree$data$nlmplot == 0 ] = TRUE

        p = ggparty::ggparty(tree,
                    terminal_space = 0.6,
                    add_vars = list(cAME =
                                      function(data, node) {
                                        list(round(mean(node$data$fme), 2))
                                      },
                                    cAME_sd =
                                      function(data, node) {
                                        list(round(sd(node$data$fme), 2))
                                      },
                                    cANLM =
                                      function(data, node) {
                                        list(round(mean(node$data$nlm, na.rm = TRUE), 2))
                                      },
                                    cANLM_sd =
                                      function(data, node) {
                                        list(round(sd(node$data$nlm, na.rm = TRUE), 2))
                                      }
                    )) +
          ggparty::geom_edge() +
          ggparty::geom_edge_label() +
          ggparty::geom_node_splitvar() +
          ggparty::geom_node_label(ggplot2::aes(label = paste0("n = ", nodesize, "\ncAME = ", cAME, "\nSD(FME) = ", cAME_sd, "\ncANLM = ", cANLM, "\nSD(NLM) = ", cANLM_sd)),
                          fontface = "bold",
                          ids = "terminal",
                          size = 2.5,
                          nudge_y = -0.07) +
          ggparty::geom_node_plot(gglist = list(ggplot2::geom_histogram(lwd = 0.3,
                                                      linetype = "solid",
                                                      colour = "black",
                                                      fill = "#1E9B8AFF",
                                                      show.legend = FALSE,
                                                      mapping = ggplot2::aes(x = fme, y = ggplot2::after_stat(count)),
                                                      bins = 11),
                                       ggplot2::xlab("FME"),
                                       ggplot2::ylab(""),
                                       #geom_density(lwd = 0.4,
                                       #mapping = ggplot2::aes(x = fme, y = ..scaled..)),
                                       ggplot2::geom_vline(lwd = 1.2, mapping = ggplot2::aes(xintercept = fmemean)),
                                       ggplot2::theme_bw(),
                                       ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
                                             axis.title = ggplot2::element_text(size = 10.5),
                                             axis.text.x   = ggplot2::element_text(colour = "black"),
                                             axis.text.y   = ggplot2::element_text(colour = "black"))),
                         height = 0.43,
                         nudge_x = -0.028,
                         nudge_y = -0.16) +
          ggparty::geom_node_plot(gglist = list(ggplot2::geom_histogram(lwd = 0.3,
                                                      linetype = "solid",
                                                      colour = "black",
                                                      show.legend = FALSE,
                                                      mapping = ggplot2::aes(x = nlmplot, y = ggplot2::after_stat(count), fill = iszero),
                                                      binwidth = 0.1,
                                                      na.rm = TRUE),
                                       ggplot2::xlab("NLM"),
                                       ggplot2::ylab(""),
                                       #geom_density(lwd = 0.4,
                                       #show.legend = FALSE,
                                       #mapping = ggplot2::aes(x = nlm, y = ..scaled..)),
                                       ggplot2::geom_vline(lwd = 1.2, mapping = ggplot2::aes(xintercept = nlmmean)),
                                       ggplot2::theme_bw(),
                                       ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.7),
                                             axis.title = ggplot2::element_text(size = 10.5),
                                             axis.text.x   = ggplot2::element_text(colour = "black"),
                                             axis.text.y   = ggplot2::element_text(colour = "black")),
                                       ggplot2::scale_fill_manual(values = c("#1E9B8AFF", "aliceblue")),
                                       ggplot2::scale_x_continuous(breaks=seq(0, 1, 0.5),
                                                          labels=c("\u2264 0", as.character(seq(0.5, 1, 0.5))))),
                         height = 0.43,
                         nudge_x = -0.028,
                         nudge_y = -0.43) +
          ggplot2::theme(legend.position = "none")
      }
      p
    }
  )
)
