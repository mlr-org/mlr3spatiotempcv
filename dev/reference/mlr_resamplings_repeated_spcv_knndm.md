# (CAST) Repeated K-fold Nearest Neighbour Distance Matching

This function implements the kNNDM algorithm and returns the necessary
indices to perform a k-fold NNDM CV for map validation.

## Details

knndm is a k-fold version of NNDM LOO CV for medium and large datasets.
Brielfy, the algorithm tries to find a k-fold configuration such that
the integral of the absolute differences (Wasserstein W statistic)
between the empirical nearest neighbour distance distribution function
between the test and training data during CV (Gj\*), and the empirical
nearest neighbour distance distribution function between the prediction
and training points (Gij), is minimised. It does so by performing
clustering of the training points' coordinates for different numbers of
clusters that range from k to N (number of observations), merging them
into k final folds, and selecting the configuration with the lowest W.

Using a projected CRS in \`knndm\` has large computational advantages
since fast nearest neighbour search can be done via the \`FNN\` package,
while working with geographic coordinates requires computing the full
spherical distance matrices. As a clustering algorithm, \`kmeans\` can
only be used for projected CRS while \`hierarchical\` can work with both
projected and geographical coordinates, though it requires calculating
the full distance matrix of the training points even for a projected
CRS.

In order to select between clustering algorithms and number of folds
\`k\`, different \`knndm\` configurations can be run and compared, being
the one with a lower W statistic the one that offers a better match. W
statistics between \`knndm\` runs are comparable as long as \`tpoints\`
and \`predpoints\` or \`modeldomain\` stay the same.

Map validation using \`knndm\` should be used using
\`CAST::global_validation\`, i.e. by stacking all out-of-sample
predictions and evaluating them all at once. The reasons behind this
are 1) The resulting folds can be unbalanced and 2) nearest neighbour
functions are constructed and matched using all CV folds simultaneously.

If training data points are very clustered with respect to the
prediction area and the presented \`knndm\` configuration still show
signs of Gj\* \> Gij, there are several things that can be tried. First,
increase the \`maxp\` parameter; this may help to control for strong
clustering (at the cost of having unbalanced folds). Secondly, decrease
the number of final folds \`k\`, which may help to have larger clusters.

The \`modeldomain\` is either a sf polygon that defines the prediction
area, or alternatively a SpatRaster out of which a polygon, transformed
into the CRS of the training points, is defined as the outline of all
non-NA cells. Then, the function takes a regular point sample (amount
defined by \`samplesize\`) from the spatial extent. As an alternative
use \`predpoints\` instead of \`modeldomain\`, if you have already
defined the prediction locations (e.g. raster pixel centroids). When
using either \`modeldomain\` or \`predpoints\`, we advise to plot the
study area polygon and the training/prediction points as a previous step
to ensure they are aligned.

\`knndm\` can also be performed in the feature space by setting
\`space\` to "feature". Euclidean distances or Mahalanobis distances can
be used for distance calculation, but only Euclidean are tested. In this
case, nearest neighbour distances are calculated in n-dimensional
feature space rather than in geographical space. \`tpoints\` and
\`predpoints\` can be data frames or sf objects containing the values of
the features. Note that the names of \`tpoints\` and \`predpoints\` must
be the same. \`predpoints\` can also be missing, if \`modeldomain\` is
of class SpatRaster. In this case, the values of of the SpatRaster will
be extracted to the \`predpoints\`. In the case of any categorical
features, Gower distances will be used to calculate the Nearest
Neighbour distances \[Experimental\]. If categorical features are
present, and \`clustering\` = "kmeans", K-Prototype clustering will be
performed instead.

## Parameters

- `folds` (`integer(1)`)  
  Number of folds.

- `stratify`  
  If `TRUE`, stratify on the target column.

&nbsp;

- `repeats` (`integer(1)`)  
  Number of repeats.

## References

Linnenbrink, J., Mila, C., Ludwig, M., Meyer, H. (2023). “kNNDM: k-fold
Nearest Neighbour Distance Matching Cross-Validation for map accuracy
estimation.” *EGUsphere*, **2023**, 1–16.
[doi:10.5194/egusphere-2023-1308](https://doi.org/10.5194/egusphere-2023-1308)
,
<https://egusphere.copernicus.org/preprints/2023/egusphere-2023-1308/>.

## Super class

[`mlr3::Resampling`](https://mlr3.mlr-org.com/reference/Resampling.html)
-\> `ResamplingRepeatedSpCVKnndm`

## Active bindings

- `iters`:

  `integer(1)`  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingRepeatedSpCVKnndm$new()`](#method-ResamplingRepeatedSpCVKnndm-new)

- [`ResamplingRepeatedSpCVKnndm$folds()`](#method-ResamplingRepeatedSpCVKnndm-folds)

- [`ResamplingRepeatedSpCVKnndm$repeats()`](#method-ResamplingRepeatedSpCVKnndm-repeats)

- [`ResamplingRepeatedSpCVKnndm$instantiate()`](#method-ResamplingRepeatedSpCVKnndm-instantiate)

- [`ResamplingRepeatedSpCVKnndm$clone()`](#method-ResamplingRepeatedSpCVKnndm-clone)

Inherited methods

- [`mlr3::Resampling$format()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-format)
- [`mlr3::Resampling$help()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-help)
- [`mlr3::Resampling$print()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-print)
- [`mlr3::Resampling$test_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-test_set)
- [`mlr3::Resampling$train_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-train_set)

------------------------------------------------------------------------

### Method `new()`

Create a "K-fold Nearest Neighbour Distance Matching" resampling
instance.

#### Usage

    ResamplingRepeatedSpCVKnndm$new(id = "repeated_spcv_knndm")

#### Arguments

- `id`:

  `character(1)`  
  Identifier for the resampling strategy.

------------------------------------------------------------------------

### Method `folds()`

Translates iteration numbers to fold number.

#### Usage

    ResamplingRepeatedSpCVKnndm$folds(iters)

#### Arguments

- `iters`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Iteration number.

------------------------------------------------------------------------

### Method `repeats()`

Translates iteration numbers to repetition number.

#### Usage

    ResamplingRepeatedSpCVKnndm$repeats(iters)

#### Arguments

- `iters`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Iteration number.

------------------------------------------------------------------------

### Method `instantiate()`

Materializes fixed training and test splits for a given task.

#### Usage

    ResamplingRepeatedSpCVKnndm$instantiate(task)

#### Arguments

- `task`:

  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)  
  A task to instantiate.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingRepeatedSpCVKnndm$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (requireNamespace(c("mlr3spatial", "mlr3"))) {
  library(mlr3)
  library(mlr3spatial)
  set.seed(42)
  simarea = list(matrix(c(0, 0, 0, 100, 100, 100, 100, 0, 0, 0), ncol = 2, byrow = TRUE))
  simarea = sf::st_polygon(simarea)
  train_points = sf::st_sample(simarea, 1000, type = "random")
  train_points = sf::st_as_sf(train_points)
  train_points$target = as.factor(sample(c("TRUE", "FALSE"), 1000, replace = TRUE))
  pred_points = sf::st_sample(simarea, 1000, type = "regular")

  task = mlr3spatial::as_task_classif_st(sf::st_as_sf(train_points), "target", positive = "TRUE")

  cv_knndm = rsmp("repeated_spcv_knndm", predpoints = pred_points, repeats = 2)
  cv_knndm$instantiate(task)
  ### Individual sets:
  # cv_knndm$train_set(1)
  # cv_knndm$test_set(1)
  # check that no obs are in both sets
  intersect(cv_knndm$train_set(1), cv_knndm$test_set(1)) # good!

  # Internal storage:
  # cv_knndm$instance # table
}
#> Loading required namespace: mlr3spatial
#> 
#> Attaching package: ‘mlr3spatial’
#> The following objects are masked from ‘package:mlr3spatiotempcv’:
#> 
#>     TaskClassifST, TaskRegrST, as_task_classif_st,
#>     as_task_classif_st.DataBackend, as_task_classif_st.TaskClassifST,
#>     as_task_classif_st.data.frame, as_task_classif_st.sf,
#>     as_task_regr_st, as_task_regr_st.DataBackend,
#>     as_task_regr_st.TaskClassifST, as_task_regr_st.TaskRegrST,
#>     as_task_regr_st.data.frame, as_task_regr_st.sf
#> Warning: Missing CRS in training or prediction points. Assuming projected CRS.
#> Gij <= Gj; a random CV assignment is returned
#> Warning: Missing CRS in training or prediction points. Assuming projected CRS.
#> Gij <= Gj; a random CV assignment is returned
#> integer(0)
```
