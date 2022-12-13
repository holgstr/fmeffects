
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# **`fme`**: Forward Marginal Effects <img src="man/figures/logo.png" align="right" alt="" width="120" />

The `fme` package implements [forward marginal effects
(FMEs)](https://arxiv.org/abs/2201.08837), a new model-agnostic
interpretation method for feature effects in machine learning models.
FMEs are conceptually simple and intuitive to work with - we explain
[here](https://holgstr.github.io/fme/articles/fme_theory.html) why you
should use them. Currently, `fme` supports binary classification and
regression models included in the
[caret](https://topepo.github.io/caret/available-models.html) and
[mlr3](https://mlr3learners.mlr-org.com/) libraries.

## Installation

``` r
# Install directly from GitHub:
library(devtools)
install_github("holgstr/fme")
```

## Quickstart

An in-depth tutorial to the package can be found
[here](https://holgstr.github.io/fme/articles/fme.html). One advantage
of FMEs is that the way they are interpreted is similar to beta
coefficients in simple linear regression models. Consider the following
example: how does an increase in temperature by 1°C affect bike rentals
(`count`)?

``` r
# Compute and plot FMEs for a model like this:
effects = fme(model = forest,
              data = bikes,
              target = "count",
              feature = "temp",
              step.size = 1)
plot(effects, jitter = c(0.2, 0))
```

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

On average, an increase in temperature by 1°C can be expected to raise
the predicted number of bikes rentals by more than 2.
