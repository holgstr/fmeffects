# fmeffects 0.1.3

-   `fme` and `ame` automatically extract the name of the model's target variable from the model.
-    Added support for `"lm"`-type models, such as `stats::glm` and `mgcv::gam`.
-   `fme` now computes NLMs via parallel processing with `future` and displays a progress bar while doing so.
-    Multivariate effects for more than two features can be computed and visualized.
-    Improved visualizations (especially for larger data sets) via hexagon plots.
-    Better error communication with `cli`.
-    Feature interactions for categorical features are supported.

# fmeffects 0.1.0

-   Created package.
-   Added a `NEWS.md` file to track changes to the package.
