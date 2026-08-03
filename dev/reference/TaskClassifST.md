# Create a Spatiotemporal Classification Task

This task specializes
[mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) and
[mlr3::TaskSupervised](https://mlr3.mlr-org.com/reference/TaskSupervised.html)
for spatiotemporal classification problems. The target column is assumed
to be a factor. The `task_type` is set to `"classif"` and
`"spatiotemporal"`.

A spatial example task is available via `tsk("ecuador")`, a
spatiotemporal one via `tsk("cookfarm_mlr3")`.

The coordinate reference system passed during initialization must match
the one which was used during data creation, otherwise offsets of
multiple meters may occur. By default, coordinates are not used as
features. This can be changed by setting `coords_as_features = TRUE`.

## See also

Other Task:
[`TaskRegrST`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/TaskRegrST.md),
[`mlr_tasks_cookfarm_mlr3`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/mlr_tasks_cookfarm_mlr3.md),
[`mlr_tasks_diplodia`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/mlr_tasks_diplodia.md),
[`mlr_tasks_ecuador`](https://mlr3spatiotempcv.mlr-org.com/dev/reference/mlr_tasks_ecuador.md)

## Super classes

[`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.html) -\>
[`mlr3::TaskSupervised`](https://mlr3.mlr-org.com/reference/TaskSupervised.html)
-\>
[`mlr3::TaskClassif`](https://mlr3.mlr-org.com/reference/TaskClassif.html)
-\> `TaskClassifST`

## Active bindings

- `crs`:

  (`character(1)`)  
  Returns coordinate reference system of task.

- `coordinate_names`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Coordinate names.

- `coords_as_features`:

  (`logical(1)`)  
  If `TRUE`, coordinates are used as features. This is a shortcut for
  `task$set_col_roles(c("x", "y"), role = "feature")` with the
  assumption that the coordinates in the data are named `"x"` and `"y"`.

## Methods

### Public methods

- [`TaskClassifST$new()`](#method-TaskClassifST-new)

- [`TaskClassifST$coordinates()`](#method-TaskClassifST-coordinates)

- [`TaskClassifST$print()`](#method-TaskClassifST-print)

- [`TaskClassifST$clone()`](#method-TaskClassifST-clone)

Inherited methods

- [`mlr3::Task$add_strata()`](https://mlr3.mlr-org.com/reference/Task.html#method-add_strata)
- [`mlr3::Task$cbind()`](https://mlr3.mlr-org.com/reference/Task.html#method-cbind)
- [`mlr3::Task$data()`](https://mlr3.mlr-org.com/reference/Task.html#method-data)
- [`mlr3::Task$divide()`](https://mlr3.mlr-org.com/reference/Task.html#method-divide)
- [`mlr3::Task$filter()`](https://mlr3.mlr-org.com/reference/Task.html#method-filter)
- [`mlr3::Task$format()`](https://mlr3.mlr-org.com/reference/Task.html#method-format)
- [`mlr3::Task$formula()`](https://mlr3.mlr-org.com/reference/Task.html#method-formula)
- [`mlr3::Task$head()`](https://mlr3.mlr-org.com/reference/Task.html#method-head)
- [`mlr3::Task$help()`](https://mlr3.mlr-org.com/reference/Task.html#method-help)
- [`mlr3::Task$levels()`](https://mlr3.mlr-org.com/reference/Task.html#method-levels)
- [`mlr3::Task$materialize_view()`](https://mlr3.mlr-org.com/reference/Task.html#method-materialize_view)
- [`mlr3::Task$missings()`](https://mlr3.mlr-org.com/reference/Task.html#method-missings)
- [`mlr3::Task$rbind()`](https://mlr3.mlr-org.com/reference/Task.html#method-rbind)
- [`mlr3::Task$rename()`](https://mlr3.mlr-org.com/reference/Task.html#method-rename)
- [`mlr3::Task$select()`](https://mlr3.mlr-org.com/reference/Task.html#method-select)
- [`mlr3::Task$set_col_roles()`](https://mlr3.mlr-org.com/reference/Task.html#method-set_col_roles)
- [`mlr3::Task$set_levels()`](https://mlr3.mlr-org.com/reference/Task.html#method-set_levels)
- [`mlr3::Task$set_row_roles()`](https://mlr3.mlr-org.com/reference/Task.html#method-set_row_roles)
- [`mlr3::TaskClassif$droplevels()`](https://mlr3.mlr-org.com/reference/TaskClassif.html#method-droplevels)
- [`mlr3::TaskClassif$truth()`](https://mlr3.mlr-org.com/reference/TaskClassif.html#method-truth)

------------------------------------------------------------------------

### Method `new()`

Create a new spatiotemporal resampling Task

#### Usage

    TaskClassifST$new(
      id,
      backend,
      target,
      positive = NULL,
      label = NA_character_,
      coordinate_names,
      crs = NA_character_,
      coords_as_features = FALSE,
      extra_args = list()
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `backend`:

  ([mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html))  
  Either a
  [mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html),
  or any object which is convertible to a
  [mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html)
  with `as_data_backend()`. E.g., am `sf` will be converted to a
  [mlr3::DataBackendDataTable](https://mlr3.mlr-org.com/reference/DataBackendDataTable.html).

- `target`:

  (`character(1)`)  
  Name of the target column.

- `positive`:

  (`character(1)`)  
  Only for binary classification: Name of the positive class. The levels
  of the target columns are reordered accordingly, so that the first
  element of `$class_names` is the positive class, and the second
  element is the negative class.

- `label`:

  (`character(1)`)  
  Label for the new instance. Shown in `as.data.table(mlr_tasks)`.

- `coordinate_names`:

  (`character(1)`)  
  The column names of the coordinates in the data.

- `crs`:

  (`character(1)`)  
  Coordinate reference system. WKT2 or EPSG string.

- `coords_as_features`:

  (`logical(1)`)  
  If `TRUE`, coordinates are used as features. This is a shortcut for
  `task$set_col_roles(c("x", "y"), role = "feature")` with the
  assumption that the coordinates in the data are named `"x"` and `"y"`.

- `extra_args`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Named list of constructor arguments, required for converting task
  types via
  [`mlr3::convert_task()`](https://mlr3.mlr-org.com/reference/convert_task.html).

------------------------------------------------------------------------

### Method `coordinates()`

Returns coordinates of observations.

#### Usage

    TaskClassifST$coordinates(row_ids = NULL)

#### Arguments

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Vector of rows indices as subset of `task$row_ids`.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the task.

#### Usage

    TaskClassifST$print(...)

#### Arguments

- `...`:

  Arguments passed to the `$print()` method of the superclass.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskClassifST$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
  task = as_task_classif_st(ecuador,
    target = "slides",
    positive = "TRUE", coordinate_names = c("x", "y")
  )

  # passing objects of class 'sf' is also supported
  data_sf = sf::st_as_sf(ecuador, coords = c("x", "y"))
  task = as_task_classif_st(data_sf, target = "slides", positive = "TRUE")

  task$task_type
  task$formula()
  task$class_names
  task$positive
  task$negative
  task$coordinates()
  task$coordinate_names
}
#> [1] "X" "Y"
# }
```
