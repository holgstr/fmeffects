
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# **`fme`**: Forward Marginal Effects <img src="man/figures/logo.png" align="right" alt="" width="120" />

The `fme` package is designed to compute, aggregate and visualize
[forward marginal effects (FMEs)](https://arxiv.org/abs/2201.08837) for
arbitrary supervised machine learning models. Currently, it supports
regression models such as random forests or gradient-boosted trees.
Also, we are working to extend it to binary classification models.

## Installation

``` r
# Install directly from GitHub:
library(devtools)
install_github("holgstr/fme")
```

## Usage

A introduction can be found
[here](https://holgstr.github.io/fme/articles/fme.html). The main
function to compute FMEs is `fme()`:

``` r
# Run once to configure your package to use pkgdown
effects = fme(model = forest,
              data = bikes,
              target = "count",
              feature = "temp",
              step.size = 1)
plot(effects, jitter = c(0.2, 0))
```

![](man/figures/unnamed-chunk-4-1.png)<!-- -->
