# Perform multimodel inference with a meta-regression model

This function performs multimodel inference to evaluate the importance
of predictors with a meta-analytical meta-regression model.

## Usage

``` r
multimodel.inference(TE, seTE, data, predictors, method='REML', test='knha',
    eval.criterion='AICc', interaction=FALSE, seed=123)
```

## Arguments

- TE:

  The precalculated effect size for each study. Must be supplied as the
  name of the effect size column in the dataset (in quotation marks;
  e.g. `TE = "effectsize"`).

- seTE:

  The precalculated standard error of the effect size for each study.
  Must be supplied as the name of the standard error column in the
  dataset (in quotation marks; e.g. `seTE = "se"`).

- data:

  A `data.frame` containing columns for the effect size, standard error
  and meta-regression predictors of each study/effect.

- predictors:

  A character vector specifying the predictors to be used for multimodel
  inference. Names of the predictors must be identical to the names of
  the columns in the `data.frame` supplied to `data`.

- method:

  Meta-analysis model to use for pooling effect sizes. Use `'FE'` for
  the fixed-effect model. Different random-effect models are available:
  "`DL`", "`HE`", "`SJ`", "`ML`", "`REML`", "`EB`", "`HS`" or "`GENQ`.
  If `'FE'` is used, the `test` argument is automatically set to `'z'`,
  as the Knapp-Hartung method is not meant to be used with fixed-effect
  models. Default is `'REML'`, and it is strongly advised to remain with
  this option to use a standard (mixed-effects) meta-regression model.

- test:

  Method to use to compute test statistics and confidence intervals.
  Default is `'knha'` which uses the Knapp-Hartung (Knapp &
  Hartung, 2003) adjustment method. "Conventional" Wald-type tests and
  CIs are calculated by setting this argument to `'z'`. When
  `method='FE'`, this argument is set to `'z'` automatically as the
  Knapp-Hartung method was not intended to be used with fixed-effect
  models.

- eval.criterion:

  Evaluation criterion to sort the multiple models by. Can be either
  `'AICc'` (default; corrected Akaike's Information Criterion), `'AIC'`
  (Akaike's Information Criterion) or `'BIC'` (Bayesian Information
  Criterion).

- interaction:

  If set to `FALSE` (default), no interactions between predictors are
  considered. Setting this parameter to `TRUE` means that all
  interactions are modeled (interactions will only be modeled if the
  number of provided predictors is 4 or less).

- seed:

  Set a seed for the function. Default seed is `123`.

## Value

Returns four tables and a plot:

- **Final Results (Summary Table)**: Displays the number of fitted
  models, model formula, method to calculate test statistics and
  confidence intervals, interactions, and evaluation criterion used.

- **Best 5 Models**: Displays the top five models in terms of the
  evaluation criterion used. Predictors are displayed as columns of the
  table, and models as rows. A number (weight) or `+` sign (for
  categorical predictors) indicates that a predictor/interaction term
  was used for the model, while empty cells indicate that the predictor
  was omitted in this model. Other metrics such as the `weight`,
  evaluation metric `delta` compared to the best model, log-Likelihood
  (`logLik`) and degrees of freedom are also displayed.

- **Multimodel Inference Coefficients**: Displays the estimated
  coefficients and statistical significance of each regression term in
  the model.

- **Predictor Importance**: Displays the estimated importance of each
  model term. The table is sorted from highest to lowest. A common rule
  of thumb is to consider a predictor as important when its importance
  value is above 0.8.

- **Predictor Importance Plot**: A bar plot for the predictor importance
  data along with a reference line for the 0.8 value often used as a
  crude threshold to characterize a predictor as important.

## Details

Multimodel methods differ from stepwise regression methods as they do
not try to successively build the “best” single (meta-regression) model
explaining most of the variance. Instead, in this procedure, all
possible combinations of a predefined selection of predictors are
modeled, and evaluated using a criterion such as Akaike’s Information
Criterion, which rewards simpler models. This enables a full examination
of all possible models, and how they perform. A common finding using
this procedure is that there are many different kinds of predictor
combinations within a model which lead to a good fit. In multimodel
inference, the estimated coefficients of predictors can then be
synthesized across all possible models to infer how important certain
predictors are overall.

Multimodel Inference can be a useful way to obtain a comprehensive look
on which predictors are more or less important for predicting
differences in effect sizes. Despite avoiding some of the problems of
stepwise regression methods, it should be noted that this method should
still be rather seen as exploratory, and may be used when there is no
prior knowledge on how predictors are related to effect sizes in the
research field under study.

The `multimodel.inference` function calls the
[`rma.uni`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
function internally, results of which are then fed forward to an adapted
version of the [`dredge`](https://rdrr.io/pkg/MuMIn/man/dredge.html)
function internally for multimodel inference. Parts of the computations
in this function are adapted from a vignette by Wolfgang Viechtbauer,
which can be found
[here](http://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti_and_mumin).

## References

Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019). *Doing
Meta-Analysis in R: A Hands-on Guide*. DOI: 10.5281/zenodo.2551803.
[Chapter 9.1](https://doing-meta.guide/smallstudyeffects.html)

Knapp, G., & Hartung, J. (2003). Improved tests for a random effects
meta-regression with a single covariate. *Statistics in Medicine, 22*,
2693–2710.

Viechtbauer, W. (2019). *Model Selection using the glmulti and MuMIn
Packages*.
[Link](http://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti_and_mumin).
Last accessed 01-Aug-2019.

## See also

[`dredge`](https://rdrr.io/pkg/MuMIn/man/dredge.html)

## Author

Mathias Harrer & David Daniel Ebert

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Perform multimodel inference with default settings
data('MVRegressionData')
library(metafor)
mmi = multimodel.inference(TE = 'yi', seTE = 'sei', data = MVRegressionData,
                           predictors = c('pubyear', 'quality',
                                          'reputation', 'continent'))
# Print summary
summary(mmi)

# Plot predictor importance
plot(mmi)

# Example 2: Model Interaction terms, set method to 'DL',
# change evaluation criterion to bic
multimodel.inference(TE = 'yi', seTE = 'sei', data = MVRegressionData,
                     predictors = c('pubyear', 'quality',
                                    'reputation', 'continent'),
                     method='DL', eval.criterion = 'BIC', interaction = TRUE)

# Example 3: Use only categorical predictors
data('ThirdWave')
multimodel.inference(TE = 'TE', seTE = 'seTE', data = ThirdWave,
                     predictors = colnames(ThirdWave)[4:7], interaction = FALSE)} # }
```
