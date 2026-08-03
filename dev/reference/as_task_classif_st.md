# Convert to a Spatiotemporal Classification Task

Convert an object to a
[TaskClassifST](https://mlr3spatiotempcv.mlr-org.com/dev/reference/TaskClassifST.md).
This is a S3 generic for the following objects:

1.  [TaskClassifST](https://mlr3spatiotempcv.mlr-org.com/dev/reference/TaskClassifST.md):
    Ensure the identity.

2.  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) and
    [mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html):
    Provides an alternative to the constructor of
    [TaskClassifST](https://mlr3spatiotempcv.mlr-org.com/dev/reference/TaskClassifST.md).

3.  [sf::sf](https://r-spatial.github.io/sf/reference/sf.html): Extracts
    spatial meta data before construction.

4.  [mlr3::TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html):
    Calls
    [`mlr3::convert_task()`](https://mlr3.mlr-org.com/reference/convert_task.html).

## Usage

``` r
as_task_classif_st(x, ...)

# S3 method for class 'TaskClassifST'
as_task_classif_st(x, clone = FALSE, ...)

# S3 method for class 'data.frame'
as_task_classif_st(
  x,
  target,
  id = deparse(substitute(x)),
  positive = NULL,
  coordinate_names,
  crs = NA_character_,
  coords_as_features = FALSE,
  label = NA_character_,
  ...
)

# S3 method for class 'DataBackend'
as_task_classif_st(
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
as_task_classif_st(
  x,
  target = NULL,
  id = deparse(substitute(x)),
  positive = NULL,
  coords_as_features = FALSE,
  label = NA_character_,
  ...
)
```

## Arguments

- x:

  (any)  
  Object to convert.

- ...:

  (any)  
  Additional arguments.

- clone:

  (`logical(1)`)  
  If `TRUE`, ensures that the returned object is not the same as the
  input `x`.

- target:

  (`character(1)`)  
  Name of the target column.

- id:

  (`character(1)`)  
  Id for the new task. Defaults to the (deparsed and substituted) name
  of the data argument.

- positive:

  (`character(1)`)  
  Only for binary classification: Name of the positive class. The levels
  of the target columns are reordered accordingly, so that the first
  element of `$class_names` is the positive class, and the second
  element is the negative class.

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

## Value

[TaskClassifST](https://mlr3spatiotempcv.mlr-org.com/dev/reference/TaskClassifST.md).

## Examples

``` r
if (mlr3misc::require_namespaces(c("sf"), quietly = TRUE)) {
  library("mlr3")
  data("ecuador", package = "mlr3spatiotempcv")

  # data.frame
  as_task_classif_st(ecuador, target = "slides", positive = "TRUE",
    coords_as_features = FALSE,
    crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs",
    coordinate_names = c("x", "y"))

  # sf
  ecuador_sf = sf::st_as_sf(ecuador, coords = c("x", "y"), crs = 32717)
  as_task_classif_st(ecuador_sf, target = "slides", positive = "TRUE")
}
#> 
#> ── <TaskClassifST> (751x11) ────────────────────────────────────────────────────
#> • Target: slides
#> • Properties: twoclass
#> • Features (10):
#>   • dbl (10): carea, cslope, dem, distdeforest, distroad, distslidespast,
#>   hcurv, log.carea, slope, vcurv
#> • Target classes: TRUE (positive class, 67%), FALSE (33%)
#> * Coordinates:
#>             X       Y
#>         <num>   <num>
#>   1: 712882.5 9560002
#>   2: 715232.5 9559582
#>   3: 715392.5 9560172
#>   4: 715042.5 9559312
#>   5: 715382.5 9560142
#>  ---                 
#> 747: 714472.5 9558482
#> 748: 713142.5 9560992
#> 749: 713322.5 9560562
#> 750: 715392.5 9557932
#> 751: 713802.5 9560862
```
