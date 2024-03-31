
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/fmeffects)](https://www.r-pkg.org/badges/version-last-release/fmeffects)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/fmeffects)](https://cranlogs.r-pkg.org/badges/grand-total/fmeffects)
[![R-CMD-check](https://github.com/holgstr/fme/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/holgstr/fme/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# **`fmeffects`**: An R Package for Forward Marginal Effects <img src="man/figures/logo.png" align="right" alt="" width="120" />

This package implements [forward marginal effects
(FMEs)](https://arxiv.org/abs/2201.08837), a model-agnostic framework
for interpreting feature effects in machine learning models. FMEs are
the simplest and most intuitive way to interpret feature effects - we
explain
[here](https://holgstr.github.io/fmeffects/articles/fme_theory.html) how
they are computed and why they should be preferred to existing methods.
Currently, `fmeffects` supports 100+ regression and (binary)
classification models:

- All models from the
  [tidymodels](https://www.tidymodels.org/find/parsnip/),
  [mlr3](https://mlr3learners.mlr-org.com/) and
  [caret](https://topepo.github.io/caret/available-models.html)
  libraries.
- Additionally, native support for generalized linear models (`glm`) &
  generalized additive models (`mgcv::gam`).

## Quickstart

See [here](https://holgstr.github.io/fmeffects/articles/fmeffects.html)
for an in-depth tutorial. The big advantage of FMEs is that they are
interpreted similar to beta coefficients in linear regression models.
Consider the following example: how does an increase in temperature
(`temp`) by 1°C affect bike rentals (`count`)?

``` r
# Train a random forest on "bike" data
set.seed(123)
library(fmeffects)
library(mlr3verse)
data(bikes)
task = as_task_regr(x = bikes, id = "bikes", target = "count")
forest = lrn("regr.ranger")$train(task)
```

``` r
# Compute effects for a trained model 'forest':
effects = fme(model = forest,
              data = bikes,
              feature = "temp",
              step.size = 1)
plot(effects, jitter = c(0.2, 0))
```

![](man/figures/unnamed-chunk-3-1.png)<!-- -->

On average, an increase in temperature by 1°C results in an increase in
the predicted number of bike rentals by more than 2. This is called the
average marginal effect (AME).

Let’s compute the AME for every feature of the model:

``` r
# Compute AMEs with default step sizes:
overview = ame(model = forest,
               data = bikes)
summary(overview)
#> 
#> Model Summary Using Average Marginal Effects:
#> 
#>       Feature step.size       AME      SD      0.25     0.75   n
#> 1      season    spring  -29.5627 30.3933  -38.9776    -6.47 548
#> 2      season    summer    0.3712 22.2538   -8.3257  11.5291 543
#> 3      season      fall   13.9269 28.0969   -0.2271  35.7786 539
#> 4      season    winter   14.6231 24.5739    1.2331  25.8998 551
#> 5        year         0 -100.0511 67.9522 -158.5412  -20.643 364
#> 6        year         1   97.9793 61.0461   23.7845 149.0662 363
#> 7       month         1    4.1008 13.1329   -1.2309   7.4386 727
#> 8     holiday     False   -1.7886 21.8287   -9.6724   8.3443  21
#> 9     holiday      True  -13.4861 25.6105  -32.4273   6.4777 706
#> 10    weekday       Sat  -54.0185 48.8713  -85.2344 -16.5142 622
#> 11    weekday       Sun  -82.8857 55.7827 -119.0325 -32.2624 622
#> 12    weekday       Mon   10.1004 27.9977   -8.8229  30.4342 623
#> 13    weekday       Tue   17.1576 24.7033    0.5063  32.5038 625
#> 14    weekday       Wed   20.3346  22.484    1.3541  34.6645 623
#> 15    weekday       Thu   19.5628 23.6865   -0.4163  35.5117 624
#> 16    weekday       Fri    1.3505 35.6711  -25.4026  29.6176 623
#> 17 workingday     False -204.8757 89.7998 -259.5304  -143.91 496
#> 18 workingday      True  162.7476  63.766  121.3106 210.1368 231
#> 19    weather     clear   26.0338 41.5209    3.8218  24.2506 284
#> 20    weather     misty    3.0396 32.1851   -8.7945   1.1693 513
#> 21    weather      rain  -55.5029 52.9214   -93.484  -3.5707 657
#> 22       temp         1     2.341  7.1894   -0.5878   4.3155 727
#> 23   humidity      0.01   -0.2617  2.7596   -0.3505    0.434 727
#> 24  windspeed         1    0.0207  2.1497   -0.1694   0.2686 727
```
