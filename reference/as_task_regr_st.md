# Convert to a Spatiotemporal Regression Task

Convert object to a
[TaskRegrST](https://mlr3spatiotempcv.mlr-org.com/reference/TaskRegrST.md).

This is a S3 generic, specialized for at least the following objects:

1.  [TaskRegrST](https://mlr3spatiotempcv.mlr-org.com/reference/TaskRegrST.md):
    Ensure the identity.

2.  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) and
    [mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html):
    Provides an alternative to the constructor of
    [TaskRegrST](https://mlr3spatiotempcv.mlr-org.com/reference/TaskRegrST.md).

3.  [sf::sf](https://r-spatial.github.io/sf/reference/sf.html): Extracts
    spatial meta data before construction.

4.  [mlr3::TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html):
    Calls
    [`mlr3::convert_task()`](https://mlr3.mlr-org.com/reference/convert_task.html).

## Usage

``` r
# S3 method for class 'TaskClassifST'
as_task_regr_st(
  x,
  target = NULL,
  drop_original_target = FALSE,
  drop_levels = TRUE,
  ...
)

as_task_regr_st(x, ...)

# S3 method for class 'TaskRegrST'
as_task_regr_st(x, clone = FALSE, ...)

# S3 method for class 'data.frame'
as_task_regr_st(
  x,
  target,
  id = deparse(substitute(x)),
  coordinate_names,
  crs = NA_character_,
  coords_as_features = FALSE,
  label = NA_character_,
  ...
)

# S3 method for class 'DataBackend'
as_task_regr_st(
  x,
  target,
  id = deparse(substitute(x)),
  positive = NULL,
  coordinate_names,
  crs,
  coords_as_features = FALSE,
  label = NA_character_,
  ...
)

# S3 method for class 'sf'
as_task_regr_st(
  x,
  target = NULL,
  id = deparse(substitute(x)),
  coords_as_features = FALSE,
  label = NA_character_,
  ...
)

# S3 method for class 'TaskClassifST'
as_task_regr_st(
  x,
  target = NULL,
  drop_original_target = FALSE,
  drop_levels = TRUE,
  ...
)
```

## Arguments

- x:

  (any)  
  Object to convert.

- target:

  (`character(1)`)  
  Name of the target column.

- drop_original_target:

  (`logical(1)`)  
  If `FALSE` (default), the original target is added as a feature.
  Otherwise the original target is dropped.

- drop_levels:

  (`logical(1)`)  
  If `TRUE` (default), unused levels of the new target variable are
  dropped.

- ...:

  (any)  
  Additional arguments.

- clone:

  (`logical(1)`)  
  If `TRUE`, ensures that the returned object is not the same as the
  input `x`.

- id:

  (`character(1)`)  
  Id for the new task. Defaults to the (deparsed and substituted) name
  of the data argument.

- coordinate_names:

  (`character(1)`)  
  The column names of the coordinates in the data.

- crs:

  (`character(1)`)  
  Coordinate reference system. WKT2 or EPSG string.

- coords_as_features:

  (`logical(1)`)  
  If `TRUE`, coordinates are used as features. This is a shortcut for
  `task$set_col_roles(c("x", "y"), role = "feature")` with the
  assumption that the coordinates in the data are named `"x"` and `"y"`.

- label:

  (`character(1)`)  
  Label for the new instance. Shown in `as.data.table(mlr_tasks)`.

- positive:

  (`character(1)`)  
  Only for binary classification: Name of the positive class. The levels
  of the target columns are reordered accordingly, so that the first
  element of `$class_names` is the positive class, and the second
  element is the negative class.

## Value

[TaskRegrST](https://mlr3spatiotempcv.mlr-org.com/reference/TaskRegrST.md)

## Examples

``` r
# \donttest{
if (mlr3misc::require_namespaces(c("sf"), quietly = TRUE)) {
  library("mlr3")
  data("cookfarm_mlr3", package = "mlr3spatiotempcv")

  # data.frame
  as_task_regr_st(cookfarm_mlr3, target = "PHIHOX",
    coords_as_features = FALSE, crs = 26911,
    coordinate_names = c("x", "y"))

  # sf
  cookfarm_sf = sf::st_as_sf(cookfarm_mlr3, coords = c("x", "y"), crs = 26911)
  as_task_regr_st(cookfarm_sf, target = "PHIHOX")
}
#> 
#> ── <TaskRegrST> (178840x23) ────────────────────────────────────────────────────
#> • Target: PHIHOX
#> • Properties: -
#> • Features (22):
#>   • dbl (18): BLD, LHDICM, Port1C, Port1EC, Port1VW, Port2C, Port2EC, Port2VW,
#>   Port3C, Port3EC, Port3VW, Port4C, Port4EC, Port4VW, Port5C, Port5EC, Port5VW,
#>   UHDICM
#>   • fct (3): HZDUSD, SOURCEID, TAXSUSDA
#>   • chr (1): Date
#> * Coordinates:
#>                X       Y
#>            <num>   <num>
#>      1: 493383.1 5180586
#>      2: 493383.1 5180586
#>      3: 493383.1 5180586
#>      4: 493383.1 5180586
#>      5: 493383.1 5180586
#>     ---                 
#> 178836: 493841.6 5181101
#> 178837: 493841.6 5181101
#> 178838: 493841.6 5181101
#> 178839: 493841.6 5181101
#> 178840: 493841.6 5181101
# }
```
