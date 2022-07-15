PartitioningPlot = R6Class("PartitioningPlot",
  public = list(

    initialize = function(tree, object, has.nlm = TRUE) {

      # Check if tree is of class 'party'
      assertClass(tree, classes = "party")

      # Check if has.nlm is logical of length 1
      assertLogical(has.nlm, len = 1)

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
      dt = as.data.table(data_party(tree))
      dt[, c := mean(fme), by = `(fitted)`]
      tree$data$fmemean = dt$c

      # Plot for tree without nlm computed
      if (has.nlm == FALSE) {
        p = ggparty(tree,
                    terminal_space = 0.5,
                    add_vars = list(cAME =
                                  function(data, node) {
                                    list(round(mean(node$data$fme), 2))
                                  },
                                cAME_cov =
                                  function(data, node) {
                                    list(round(sd(node$data$fme) / (abs(mean(node$data$fme))), 2))
                                  }
                   )) +
          geom_edge() +
          geom_edge_label() +
          geom_node_splitvar() +
          geom_node_label(aes(label = paste0("n = ", nodesize, "\ncAME = ", cAME, "\nCoV(fME) = ", cAME_cov)),
                          fontface = "bold",
                          ids = "terminal",
                          size = 2.5,
                          nudge_y = -0.05) +
          geom_node_plot(gglist = list(geom_histogram(lwd = 0.3,
                                                      linetype = "solid",
                                                      colour = "black",
                                                      fill = "cornflowerblue",
                                                      show.legend = FALSE,
                                                      mapping = aes(x = fme, y = ..count..),
                                                      bins = 11),
                                       xlab("fME"),
                                       ylab(""),
                                       #geom_density(lwd = 0.4,
                                       #mapping = aes(x = fme, y = ..scaled..)),
                                       geom_vline(lwd = 1.2, mapping = aes(xintercept = fmemean)),
                                       theme_bw(),
                                       theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7),
                                             axis.title = element_text(size = 10.5),
                                             axis.text.x   = element_text(colour = "black"),
                                             axis.text.y   = element_text(colour = "black"))),
                         height = 0.68,
                         nudge_x = -0.028,
                         nudge_y = -0.14) +
          theme(legend.position = "none")
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

        p = ggparty(tree,
                    terminal_space = 0.6,
                    add_vars = list(cAME =
                                      function(data, node) {
                                        list(round(mean(node$data$fme), 2))
                                      },
                                    cAME_cov =
                                      function(data, node) {
                                        list(round(sd(node$data$fme) / (abs(mean(node$data$fme))), 2))
                                      },
                                    cANLM =
                                      function(data, node) {
                                        list(round(mean(node$data$nlm, na.rm = TRUE), 2))
                                      },
                                    cANLM_cov =
                                      function(data, node) {
                                        list(round(sd(node$data$nlm, na.rm = TRUE) / (abs(mean(node$data$nlm, na.rm = TRUE))), 2))
                                      }
                    )) +
          geom_edge() +
          geom_edge_label() +
          geom_node_splitvar() +
          geom_node_label(aes(label = paste0("n = ", nodesize, "\ncAME = ", cAME, "\nCoV(fME) = ", cAME_cov, "\ncANLM = ", cANLM, "\nCoV(NLM) = ", cANLM_cov)),
                          fontface = "bold",
                          ids = "terminal",
                          size = 2.5,
                          nudge_y = -0.07) +
          geom_node_plot(gglist = list(geom_histogram(lwd = 0.3,
                                                      linetype = "solid",
                                                      colour = "black",
                                                      fill = "cornflowerblue",
                                                      show.legend = FALSE,
                                                      mapping = aes(x = fme, y = ..count..),
                                                      bins = 11),
                                       xlab("fME"),
                                       ylab(""),
                                       #geom_density(lwd = 0.4,
                                       #mapping = aes(x = fme, y = ..scaled..)),
                                       geom_vline(lwd = 1.2, mapping = aes(xintercept = fmemean)),
                                       theme_bw(),
                                       theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7),
                                             axis.title = element_text(size = 10.5),
                                             axis.text.x   = element_text(colour = "black"),
                                             axis.text.y   = element_text(colour = "black"))),
                         height = 0.43,
                         nudge_x = -0.028,
                         nudge_y = -0.16) +
          geom_node_plot(gglist = list(geom_histogram(lwd = 0.3,
                                                      linetype = "solid",
                                                      colour = "black",
                                                      show.legend = FALSE,
                                                      mapping = aes(x = nlmplot, y = ..count.., fill = iszero),
                                                      binwidth = 0.1,
                                                      na.rm = TRUE),
                                       xlab("NLM"),
                                       ylab(""),
                                       #geom_density(lwd = 0.4,
                                       #show.legend = FALSE,
                                       #mapping = aes(x = nlm, y = ..scaled..)),
                                       geom_vline(lwd = 1.2, mapping = aes(xintercept = nlmmean)),
                                       theme_bw(),
                                       theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7),
                                             axis.title = element_text(size = 10.5),
                                             axis.text.x   = element_text(colour = "black"),
                                             axis.text.y   = element_text(colour = "black")),
                                       scale_fill_manual(values = c("cornflowerblue", "aliceblue")),
                                       scale_x_continuous(breaks=seq(0, 1, 0.5),
                                                          labels=c("\u2264 0", as.character(seq(0.5, 1, 0.5))))),
                         height = 0.43,
                         nudge_x = -0.028,
                         nudge_y = -0.43) +
          theme(legend.position = "none")
      }
      p
    }
  )
)
