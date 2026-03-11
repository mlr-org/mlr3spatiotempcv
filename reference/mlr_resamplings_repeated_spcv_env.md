# (blockCV) Repeated "environmental blocking" resampling

Splits data by clustering in the feature space. See the upstream
implementation at
[`blockCV::cv_cluster()`](https://rdrr.io/pkg/blockCV/man/cv_cluster.html)
and Valavi et al. (2018) for further information.

## Details

Useful when the dataset is supposed to be split on environmental
information which is present in features. The method allows for a
combination of multiple features for clustering.

The input of raster images directly as in
[`blockCV::cv_cluster()`](https://rdrr.io/pkg/blockCV/man/cv_cluster.html)
is not supported. See
[mlr3spatial](https://CRAN.R-project.org/package=mlr3spatial) and its
raster DataBackends for such support in
[mlr3](https://CRAN.R-project.org/package=mlr3).

## Parameters

- `folds` (`integer(1)`)  
  Number of folds.

- `features` ([`character()`](https://rdrr.io/r/base/character.html))  
  The features to use for clustering.

&nbsp;

- `repeats` (`integer(1)`)  
  Number of repeats.

## References

Valavi R, Elith J, Lahoz-Monfort JJ, Guillera-Arroita G (2018).
“blockCV: an R package for generating spatially or environmentally
separated folds for k-fold cross-validation of species distribution
models.” *bioRxiv*. [doi:10.1101/357798](https://doi.org/10.1101/357798)
.

## Super class

[`mlr3::Resampling`](https://mlr3.mlr-org.com/reference/Resampling.html)
-\> `ResamplingRepeatedSpCVEnv`

## Active bindings

- `iters`:

  `integer(1)`  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingRepeatedSpCVEnv$new()`](#method-ResamplingRepeatedSpCVEnv-new)

- [`ResamplingRepeatedSpCVEnv$folds()`](#method-ResamplingRepeatedSpCVEnv-folds)

- [`ResamplingRepeatedSpCVEnv$repeats()`](#method-ResamplingRepeatedSpCVEnv-repeats)

- [`ResamplingRepeatedSpCVEnv$instantiate()`](#method-ResamplingRepeatedSpCVEnv-instantiate)

- [`ResamplingRepeatedSpCVEnv$clone()`](#method-ResamplingRepeatedSpCVEnv-clone)

Inherited methods

- [`mlr3::Resampling$format()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-format)
- [`mlr3::Resampling$help()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-help)
- [`mlr3::Resampling$print()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-print)
- [`mlr3::Resampling$test_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-test_set)
- [`mlr3::Resampling$train_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-train_set)

------------------------------------------------------------------------

### Method `new()`

Create an "Environmental Block" repeated resampling instance.

For a list of available arguments, please see
[blockCV::cv_cluster](https://rdrr.io/pkg/blockCV/man/cv_cluster.html).

#### Usage

    ResamplingRepeatedSpCVEnv$new(id = "repeated_spcv_env")

#### Arguments

- `id`:

  `character(1)`  
  Identifier for the resampling strategy.

------------------------------------------------------------------------

### Method `folds()`

Translates iteration numbers to fold number.

#### Usage

    ResamplingRepeatedSpCVEnv$folds(iters)

#### Arguments

- `iters`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Iteration number.

------------------------------------------------------------------------

### Method `repeats()`

Translates iteration numbers to repetition number.

#### Usage

    ResamplingRepeatedSpCVEnv$repeats(iters)

#### Arguments

- `iters`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Iteration number.

------------------------------------------------------------------------

### Method `instantiate()`

Materializes fixed training and test splits for a given task.

#### Usage

    ResamplingRepeatedSpCVEnv$instantiate(task)

#### Arguments

- `task`:

  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)  
  A task to instantiate.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingRepeatedSpCVEnv$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
  library(mlr3)
  task = tsk("ecuador")

  # Instantiate Resampling
  rrcv = rsmp("repeated_spcv_env", folds = 4, repeats = 2)
  rrcv$instantiate(task)

  # Individual sets:
  rrcv$train_set(1)
  rrcv$test_set(1)
  intersect(rrcv$train_set(1), rrcv$test_set(1))

  # Internal storage:
  rrcv$instance
}
#>       row_id   rep  fold
#>        <int> <int> <int>
#>    1:      1     1     3
#>    2:      2     1     3
#>    3:      3     1     4
#>    4:      4     1     3
#>    5:      5     1     3
#>   ---                   
#> 1498:    747     2     1
#> 1499:    748     2     1
#> 1500:    749     2     1
#> 1501:    750     2     1
#> 1502:    751     2     1
# }
```
