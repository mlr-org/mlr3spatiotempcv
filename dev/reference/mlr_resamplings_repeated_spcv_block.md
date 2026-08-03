# (blockCV) Repeated spatial block resampling

This function creates spatially separated folds based on a distance to
number of row and/or column. It assigns blocks to the training and
testing folds **randomly**, **systematically** or in a **checkerboard
pattern**. The distance (`size`) should be in **metres**, regardless of
the unit of the reference system of the input data (for more information
see the details section). By default, the function creates blocks
according to the extent and shape of the spatial sample data (`x` e.g.
the species occurrence), Alternatively, blocks can be created based on
`r` assuming that the user has considered the landscape for the given
species and case study. Blocks can also be offset so the origin is not
at the outer corner of the rasters. Instead of providing a distance, the
blocks can also be created by specifying a number of rows and/or columns
and divide the study area into vertical or horizontal bins, as presented
in Wenger & Olden (2012) and Bahn & McGill (2012). Finally, the blocks
can be specified by a user-defined spatial polygon layer.

## Details

To maintain consistency, all functions in this package use **meters** as
their unit of measurement. However, when the input map has a geographic
coordinate system (in decimal degrees), the block size is calculated by
dividing the `size` parameter by `deg_to_metre` (which defaults to
111325 meters, the standard distance of one degree of latitude on the
Equator). In reality, this value varies by a factor of the cosine of the
latitude. So, an alternative sensible value could be
`cos(mean(sf::st_bbox(x)[c(2,4)]) * pi/180) * 111325`.

The `offset` can be used to change the spatial position of the blocks.
It can also be used to assess the sensitivity of analysis results to
shifting in the blocking arrangements. These options are available when
`size` is defined. By default the region is located in the middle of the
blocks and by setting the offsets, the blocks will shift.

Roberts et. al. (2017) suggest that blocks should be substantially
bigger than the range of spatial autocorrelation (in model residual) to
obtain realistic error estimates, while a buffer with the size of the
spatial autocorrelation range would result in a good estimation of
error. This is because of the so-called edge effect (O'Sullivan & Unwin,
2014), whereby points located on the edges of the blocks of opposite
sets are not separated spatially. Blocking with a buffering strategy
overcomes this issue (see
[`cv_buffer`](https://rdrr.io/pkg/blockCV/man/cv_buffer.html)).

## mlr3spatiotempcv notes

By default
[`blockCV::cv_spatial()`](https://rdrr.io/pkg/blockCV/man/cv_spatial.html)
does not allow the creation of multiple repetitions. `mlr3spatiotempcv`
adds support for this when using the `size` argument for fold creation.
When supplying a vector of `length(repeats)` for argument `size`, these
different settings will be used to create folds which differ among the
repetitions.

Multiple repetitions are not possible when using the "row & cols"
approach because the created folds will always be the same.

The 'Description' and 'Details' fields are inherited from the respective
upstream function.

For a list of available arguments, please see
[blockCV::cv_spatial](https://rdrr.io/pkg/blockCV/man/cv_spatial.html).

`blockCV` \>= 3.0.0 changed the argument names of the implementation.
For backward compatibility, `mlr3spatiotempcv` is still using the old
ones. Here's a list which shows the mapping between `blockCV` \< 3.0.0
and `blockCV` \>= 3.0.0:

- `range` -\> `size`

- `rasterLayer` -\> `r`

- `speciesData` -\> `points`

- `showBlocks` -\> `plot`

- `cols` and `rows` -\> `rows_cols`

The default of argument `hexagon` is different in `mlr3spatiotempcv`
(`FALSE` instead of `TRUE`) to create square blocks instead of hexagonal
blocks by default.

## Parameters

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
-\> `ResamplingRepeatedSpCVBlock`

## Public fields

- `blocks`:

  `sf | list of sf objects`  
  Polygons (`sf` objects) as returned by blockCV which grouped
  observations into partitions.

## Active bindings

- `iters`:

  `integer(1)`  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingRepeatedSpCVBlock$new()`](#method-ResamplingRepeatedSpCVBlock-new)

- [`ResamplingRepeatedSpCVBlock$folds()`](#method-ResamplingRepeatedSpCVBlock-folds)

- [`ResamplingRepeatedSpCVBlock$repeats()`](#method-ResamplingRepeatedSpCVBlock-repeats)

- [`ResamplingRepeatedSpCVBlock$instantiate()`](#method-ResamplingRepeatedSpCVBlock-instantiate)

- [`ResamplingRepeatedSpCVBlock$clone()`](#method-ResamplingRepeatedSpCVBlock-clone)

Inherited methods

- [`mlr3::Resampling$format()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-format)
- [`mlr3::Resampling$help()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-help)
- [`mlr3::Resampling$print()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-print)
- [`mlr3::Resampling$test_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-test_set)
- [`mlr3::Resampling$train_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-train_set)

------------------------------------------------------------------------

### Method `new()`

Create an "spatial block" repeated resampling instance.

For a list of available arguments, please see
[blockCV::cv_spatial](https://rdrr.io/pkg/blockCV/man/cv_spatial.html).

#### Usage

    ResamplingRepeatedSpCVBlock$new(id = "repeated_spcv_block")

#### Arguments

- `id`:

  `character(1)`  
  Identifier for the resampling strategy.

------------------------------------------------------------------------

### Method `folds()`

Translates iteration numbers to fold number.

#### Usage

    ResamplingRepeatedSpCVBlock$folds(iters)

#### Arguments

- `iters`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Iteration number.

------------------------------------------------------------------------

### Method `repeats()`

Translates iteration numbers to repetition number.

#### Usage

    ResamplingRepeatedSpCVBlock$repeats(iters)

#### Arguments

- `iters`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Iteration number.

------------------------------------------------------------------------

### Method `instantiate()`

Materializes fixed training and test splits for a given task.

#### Usage

    ResamplingRepeatedSpCVBlock$instantiate(task)

#### Arguments

- `task`:

  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)  
  A task to instantiate.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingRepeatedSpCVBlock$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
if (mlr3misc::require_namespaces(c("sf", "blockCV"), quietly = TRUE)) {
  library(mlr3)
  task = tsk("diplodia")

  # Instantiate Resampling
  rrcv = rsmp("repeated_spcv_block",
    folds = 3, repeats = 2,
    range = c(5000L, 10000L))
  rrcv$instantiate(task)

  # Individual sets:
  rrcv$iters
  rrcv$folds(1:6)
  rrcv$repeats(1:6)

  # Individual sets:
  rrcv$train_set(1)
  rrcv$test_set(1)
  intersect(rrcv$train_set(1), rrcv$test_set(1))

  # Internal storage:
  rrcv$instance # table
}
} # }
```
