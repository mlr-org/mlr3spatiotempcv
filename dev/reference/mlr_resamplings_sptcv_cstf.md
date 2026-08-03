# (CAST) Spatiotemporal "Leave-location-and-time-out" resampling

Splits data using Leave-Location-Out (LLO), Leave-Time-Out (LTO) and
Leave-Location-and-Time-Out (LLTO) partitioning. See the upstream
implementation at `CreateSpacetimeFolds()` (package
[CAST](https://CRAN.R-project.org/package=CAST)) and Meyer et al. (2018)
for further information.

## Details

LLO predicts on unknown locations i.e. complete locations are left out
in the training sets. The `"space"` role in `Task$col_roles` identifies
spatial units. If `stratify` is `TRUE`, the target distribution is
similar in each fold. This is useful for land cover classification when
the observations are polygons. In this case, LLO with stratification
should be used to hold back complete polygons and have a similar target
distribution in each fold. LTO leaves out complete temporal units which
are identified by the `"time"` role in `Task$col_roles`. LLTO leaves out
spatial and temporal units. See the examples.

## Parameters

- `folds` (`integer(1)`)  
  Number of folds.

- `stratify`  
  If `TRUE`, stratify on the target column.

## References

Meyer H, Reudenbach C, Hengl T, Katurji M, Nauss T (2018). “Improving
performance of spatio-temporal machine learning models using forward
feature selection and target-oriented validation.” *Environmental
Modelling & Software*, **101**, 1–9.
[doi:10.1016/j.envsoft.2017.12.001](https://doi.org/10.1016/j.envsoft.2017.12.001)
.

## Super class

[`mlr3::Resampling`](https://mlr3.mlr-org.com/reference/Resampling.html)
-\> `ResamplingSptCVCstf`

## Active bindings

- `iters`:

  `integer(1)`  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingSptCVCstf$new()`](#method-ResamplingSptCVCstf-new)

- [`ResamplingSptCVCstf$instantiate()`](#method-ResamplingSptCVCstf-instantiate)

- [`ResamplingSptCVCstf$clone()`](#method-ResamplingSptCVCstf-clone)

Inherited methods

- [`mlr3::Resampling$format()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-format)
- [`mlr3::Resampling$help()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-help)
- [`mlr3::Resampling$print()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-print)
- [`mlr3::Resampling$test_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-test_set)
- [`mlr3::Resampling$train_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-train_set)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a "Spacetime Folds" resampling instance.

#### Usage

    ResamplingSptCVCstf$new(id = "sptcv_cstf")

#### Arguments

- `id`:

  `character(1)`  
  Identifier for the resampling strategy.

------------------------------------------------------------------------

### Method `instantiate()`

Materializes fixed training and test splits for a given task.

#### Usage

    ResamplingSptCVCstf$instantiate(task)

#### Arguments

- `task`:

  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)  
  A task to instantiate.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingSptCVCstf$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
library(mlr3)
task = tsk("cookfarm_mlr3")
task$set_col_roles("SOURCEID", roles = "space")
task$set_col_roles("Date", roles = "time")

# Instantiate Resampling
rcv = rsmp("sptcv_cstf", folds = 5)
rcv$instantiate(task)

### Individual sets:
# rcv$train_set(1)
# rcv$test_set(1)
# check that no obs are in both sets
intersect(rcv$train_set(1), rcv$test_set(1)) # good!
#> integer(0)

# Internal storage:
# rcv$instance # table
# }
```
