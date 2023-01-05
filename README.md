
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# **`fme`**: Forward Marginal Effects <img src="man/figures/logo.png" align="right" alt="" width="120" />

This package implements [forward marginal effects
(FMEs)](https://arxiv.org/abs/2201.08837), a model-agnostic framework
for interpreting feature effects in machine learning models. FMEs are
the simplest and most intuitive way to interpret feature effects - we
explain [here](https://holgstr.github.io/fme/articles/fme_theory.html)
how they are computed and why they should be preferred to existing
methods. Currently, `fme` supports regression and (binary)
classification models from the
[caret](https://topepo.github.io/caret/available-models.html) and
[mlr3](https://mlr3learners.mlr-org.com/) libraries.

## Installation

``` r
# Install directly from GitHub:
library(devtools)
install_github("holgstr/fme")
```

## Quickstart

See [here](https://holgstr.github.io/fme/articles/fme.html) for an
in-depth tutorial. The big advantage of FMEs is that they are
interpreted similar to beta coefficients in linear regression models.
Consider the following example: how does an increase in temperature
(`temp`) by 1°C affect bike rentals (`count`)?

``` r
# Compute effects for a trained model 'forest':
effects = fme(model = forest,
              data = bikes,
              target = "count",
              feature = "temp",
              step.size = 1)
plot(effects, jitter = c(0.2, 0))
```

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

On average, an increase in temperature by 1°C results in an increase in
the predicted number of bike rentals by more than 2. This is called the
average marginal effect (AME).

Let’s compute the AME for every feature of the model:

``` r
# Compute AMEs with default step sizes:
ame(model = forest,
    data = bikes,
    target = "count")
```

    #> Model Summary Using Average Marginal Effects:
    #>                                                       
    #>  Feature    step.size       AME      0.25     0.75   n
    #>  season     spring     -29.5627  -38.9776    -6.47 548
    #>             summer       0.3712   -8.3257  11.5291 543
    #>             fall        13.9269   -0.2271  35.7786 539
    #>             winter      14.6231    1.2331  25.8998 551
    #>  year       0         -100.0511 -158.5412  -20.643 364
    #>             1           97.9793   23.7845 149.0662 363
    #>  month      1            4.1008   -1.2309   7.4386 727
    #>  holiday    False       -1.7886   -9.6724   8.3443  21
    #>             True       -13.4861  -32.4273   6.4777 706
    #>  weekday    Sat        -54.0185  -85.2344 -16.5142 622
    #>             Sun        -82.8857 -119.0325 -32.2624 622
    #>             Mon         10.1004   -8.8229  30.4342 623
    #>             Tue         17.1576    0.5063  32.5038 625
    #>             Wed         20.3346    1.3541  34.6645 623
    #>             Thu         19.5628   -0.4163  35.5117 624
    #>             Fri          1.3505  -25.4026  29.6176 623
    #>  workingday False     -204.8757 -259.5304  -143.91 496
    #>             True       162.7476  121.3106 210.1368 231
    #>  weather    clear       26.0338    3.8218  24.2506 284
    #>             misty        3.0396   -8.7945   1.1693 513
    #>             rain       -55.5029   -93.484  -3.5707 657
    #>  temp       1             2.341   -0.5878   4.3155 727
    #>  humidity   0.01        -0.2617   -0.3505    0.434 727
    #>  windspeed  1            0.0207   -0.1694   0.2686 727
