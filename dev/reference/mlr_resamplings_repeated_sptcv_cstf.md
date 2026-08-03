# (CAST) Repeated spatiotemporal "leave-location-and-time-out" resampling

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

&nbsp;

- `repeats` (`integer(1)`)  
  Number of repeats.

## References

Zhao Y, Karypis G (2002). “Evaluation of Hierarchical Clustering
Algorithms for Document Datasets.” *11th Conference of Information and
Knowledge Management (CIKM)*, 51-524.
[doi:10.1145/584792.584877](https://doi.org/10.1145/584792.584877) .

## Super class

[`mlr3::Resampling`](https://mlr3.mlr-org.com/reference/Resampling.html)
-\> `ResamplingRepeatedSptCVCstf`

## Active bindings

- `iters`:

  `integer(1)`  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingRepeatedSptCVCstf$new()`](#method-ResamplingRepeatedSptCVCstf-new)

- [`ResamplingRepeatedSptCVCstf$folds()`](#method-ResamplingRepeatedSptCVCstf-folds)

- [`ResamplingRepeatedSptCVCstf$repeats()`](#method-ResamplingRepeatedSptCVCstf-repeats)

- [`ResamplingRepeatedSptCVCstf$instantiate()`](#method-ResamplingRepeatedSptCVCstf-instantiate)

- [`ResamplingRepeatedSptCVCstf$clone()`](#method-ResamplingRepeatedSptCVCstf-clone)

Inherited methods

- [`mlr3::Resampling$format()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-format)
- [`mlr3::Resampling$help()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-help)
- [`mlr3::Resampling$print()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-print)
- [`mlr3::Resampling$test_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-test_set)
- [`mlr3::Resampling$train_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-train_set)

------------------------------------------------------------------------

### Method `new()`

Create a "Spacetime Folds" resampling instance.

#### Usage

    ResamplingRepeatedSptCVCstf$new(id = "repeated_sptcv_cstf")

#### Arguments

- `id`:

  `character(1)`  
  Identifier for the resampling strategy.

------------------------------------------------------------------------

### Method `folds()`

Translates iteration numbers to fold number.

#### Usage

    ResamplingRepeatedSptCVCstf$folds(iters)

#### Arguments

- `iters`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Iteration number.

------------------------------------------------------------------------

### Method `repeats()`

Translates iteration numbers to repetition number.

#### Usage

    ResamplingRepeatedSptCVCstf$repeats(iters)

#### Arguments

- `iters`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Iteration number.

------------------------------------------------------------------------

### Method `instantiate()`

Materializes fixed training and test splits for a given task.

#### Usage

    ResamplingRepeatedSptCVCstf$instantiate(task)

#### Arguments

- `task`:

  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)  
  A task to instantiate.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingRepeatedSptCVCstf$clone(deep = FALSE)

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
rcv = rsmp("repeated_sptcv_cstf", folds = 5, repeats = 2)
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
