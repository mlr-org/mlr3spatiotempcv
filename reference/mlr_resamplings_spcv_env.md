# (blockCV) "Environmental blocking" resampling

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

## References

Valavi R, Elith J, Lahoz-Monfort JJ, Guillera-Arroita G (2018).
“blockCV: an R package for generating spatially or environmentally
separated folds for k-fold cross-validation of species distribution
models.” *bioRxiv*. [doi:10.1101/357798](https://doi.org/10.1101/357798)
.

## Super class

[`mlr3::Resampling`](https://mlr3.mlr-org.com/reference/Resampling.html)
-\> `ResamplingSpCVEnv`

## Active bindings

- `iters`:

  `integer(1)`  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingSpCVEnv$new()`](#method-ResamplingSpCVEnv-new)

- [`ResamplingSpCVEnv$instantiate()`](#method-ResamplingSpCVEnv-instantiate)

- [`ResamplingSpCVEnv$clone()`](#method-ResamplingSpCVEnv-clone)

Inherited methods

- [`mlr3::Resampling$format()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-format)
- [`mlr3::Resampling$help()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-help)
- [`mlr3::Resampling$print()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-print)
- [`mlr3::Resampling$test_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-test_set)
- [`mlr3::Resampling$train_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-train_set)

------------------------------------------------------------------------

### Method `new()`

Create an "Environmental Block" resampling instance.

For a list of available arguments, please see
[blockCV::cv_cluster](https://rdrr.io/pkg/blockCV/man/cv_cluster.html).

#### Usage

    ResamplingSpCVEnv$new(id = "spcv_env")

#### Arguments

- `id`:

  `character(1)`  
  Identifier for the resampling strategy.

------------------------------------------------------------------------

### Method `instantiate()`

Materializes fixed training and test splits for a given task.

#### Usage

    ResamplingSpCVEnv$instantiate(task)

#### Arguments

- `task`:

  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)  
  A task to instantiate.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingSpCVEnv$clone(deep = FALSE)

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
  rcv = rsmp("spcv_env", folds = 4)
  rcv$instantiate(task)

  # Individual sets:
  rcv$train_set(1)
  rcv$test_set(1)
  intersect(rcv$train_set(1), rcv$test_set(1))

  # Internal storage:
  rcv$instance
}
#> Key: <fold>
#>      row_id  fold
#>       <int> <int>
#>   1:      3     1
#>   2:    135     1
#>   3:    192     1
#>   4:    237     1
#>   5:    335     1
#>  ---             
#> 747:    748     3
#> 748:    749     3
#> 749:    750     3
#> 750:    751     3
#> 751:    682     4
# }
```
