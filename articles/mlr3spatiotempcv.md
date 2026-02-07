# Getting Started

## Introduction

This package adds resampling methods for the {mlr3} package framework
suited for spatial, temporal and spatiotemporal data. These methods can
help to reduce the influence of autocorrelation on performance estimates
when performing cross-validation. While this article gives a rather
technical introduction to the package, a more applied approach can be
found in the [mlr3book section on “Spatiotemporal
Analysis”](https://mlr3book.mlr-org.com/chapters/chapter13/beyond_regression_and_classification.html#sec-spatiotemporall).

After loading the package via
[`library("mlr3spatiotempcv")`](https://mlr3spatiotempcv.mlr-org.com/),
the spatiotemporal resampling methods and example tasks provided by
{mlr3spatiotempcv} are available to the user **alongside the default
{mlr3} resampling methods and tasks**.

### Creating a spatial Task

To make use of spatial resampling methods, a {mlr3} task that is aware
of its spatial characteristic needs to be created. Two `Task` child
classes exist in {mlr3spatiotempcv} for this purpose:

- `TaskClassifST`
- `TaskRegrST`

To create one of these, you have multiple options:

1.  Use the constructor of the `Task` directly via `$new()` - this only
    works for data.table backends (!)
2.  Use the `as_task_*` converters (e.g. if your data is stored in an
    `sf` object)

We recommend the latter, as the `as_task_*` converters aim to make task
construction easier, e.g., by creating the `DataBackend` (which is
required to create a Task in {mlr3}) automatically and setting the `crs`
and `coordinate_names` fields. Let’s assume your (point) data is stored
in with an `sf` object, which is a common scenario for spatial analysis
in R.

``` r
# create 'sf' object
data_sf = sf::st_as_sf(ecuador, coords = c("x", "y"), crs = 32717)

# create `TaskClassifST` from `sf` object
task = as_task_classif_st(data_sf, id = "ecuador_task", target = "slides", positive = "TRUE")
```

You can also use a plain `data.frame`. In this case, `crs` and
`coordinate_names` need to be passed along explicitly as they cannot be
inferred directly from the `sf` object:

``` r
task = as_task_classif_st(ecuador, id = "ecuador_task", target = "slides",
  positive = "TRUE", coordinate_names = c("x", "y"), crs = 32717)
```

The `*ST` task family prints a subset of the coordinates by default:

``` r
print(task)
#> 
#> ── <TaskClassifST> (751x11) ────────────────────────────────────────────────────
#> • Target: slides
#> • Properties: twoclass
#> • Features (10):
#>   • dbl (10): carea, cslope, dem, distdeforest, distroad, distslidespast,
#>   hcurv, log.carea, slope, vcurv
#> * Coordinates:
#>             x       y
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

All `*ST` tasks can be treated as their super class equivalents
`TaskClassif` or `TaskRegr` in subsequent {mlr3} modeling steps.

## Contributed reflections by {mlr3spatiotempcv}

In {mlr3}, dictionaries are used for overview purposes of available
methods. The following sections show which dictionaries get appended
with new entries when loading {mlr3spatiotempcv}.

## Task Type

- `TaskClassifST`

- `TaskRegrST`

``` r
mlr_reflections$task_types
#> Key: <type>
#>            type          package             task        learner
#>          <char>           <char>           <char>         <char>
#> 1:      classif             mlr3      TaskClassif LearnerClassif
#> 2:   classif_st mlr3spatiotempcv    TaskClassifST LearnerClassif
#> 3:         regr             mlr3         TaskRegr    LearnerRegr
#> 4:      regr_st mlr3spatiotempcv       TaskRegrST    LearnerRegr
#> 5: unsupervised             mlr3 TaskUnsupervised        Learner
#>           prediction       prediction_data        measure
#>               <char>                <char>         <char>
#> 1: PredictionClassif PredictionDataClassif MeasureClassif
#> 2: PredictionClassif PredictionDataClassif MeasureClassif
#> 3:    PredictionRegr    PredictionDataRegr    MeasureRegr
#> 4:    PredictionRegr    PredictionDataRegr    MeasureRegr
#> 5:              <NA>                  <NA>           <NA>
```

## Task Column Roles

- `coordinate`

- `space`

- `time`

``` r
mlr_reflections$task_col_roles
#> $regr
#> [1] "feature"         "target"          "name"            "order"          
#> [5] "stratum"         "group"           "offset"          "weights_learner"
#> [9] "weights_measure"
#> 
#> $classif
#> [1] "feature"         "target"          "name"            "order"          
#> [5] "stratum"         "group"           "offset"          "weights_learner"
#> [9] "weights_measure"
#> 
#> $unsupervised
#> [1] "feature" "name"    "order"  
#> 
#> $classif_st
#>  [1] "feature"         "target"          "name"            "order"          
#>  [5] "stratum"         "group"           "offset"          "weights_learner"
#>  [9] "weights_measure" "coordinate"      "space"           "time"           
#> 
#> $regr_st
#>  [1] "feature"         "target"          "name"            "order"          
#>  [5] "stratum"         "group"           "offset"          "weights_learner"
#>  [9] "weights_measure" "coordinate"      "space"           "time"
```

## Resampling Methods

- `mlr_resampling_spcv_block`

- `mlr_resampling_spcv_buffer`

- `mlr_resampling_spcv_coords`

- `mlr_resampling_spcv_knndm`

- `mlr_resampling_spcv_disc`

- `mlr_resampling_spcv_tiles`

- `mlr_resampling_spcv_env`

- `mlr_resampling_sptcv_cstf`

and their respective repeated versions. See
`as.data.table(mlr_resamplings)` for the full dictionary.

## Examples Tasks

- `tsk("ecuador")` (spatial, classif)

- `tsk("cookfarm_mlr3")` (spatiotemp, regr)

## Upstream Packages and Scientific References

The following table lists all spatiotemporal methods implemented in
{mlr3spatiotempcv} (or {mlr3}), their upstream R package and scientific
references. All methods besides `"spcv_buffer"` also have a
corresponding “repeated” method.

[TABLE]

## References

Brenning, Alexander. 2012. “Spatial cross-validation and bootstrap for
the assessment of prediction rules in remote sensing: The R package
sperrorest.” In *2012 IEEE International Geoscience and Remote Sensing
Symposium*. IEEE. <https://doi.org/10.1109/igarss.2012.6352393>.

Linnenbrink, Jan, Carles Milà, Marvin Ludwig, and Hanna Meyer. 2023.
“kNNDM: K-Fold Nearest Neighbour Distance Matching Cross-Validation for
Map Accuracy Estimation.” *EGUsphere*, July, 1–16.
<https://doi.org/10.5194/egusphere-2023-1308>.

Meyer, Hanna, Christoph Reudenbach, Tomislav Hengl, Marwan Katurji, and
Thomas Nauss. 2018. “Improving Performance of Spatio-Temporal Machine
Learning Models Using Forward Feature Selection and Target-Oriented
Validation.” *Environmental Modelling & Software* 101 (March): 1–9.
<https://doi.org/10.1016/j.envsoft.2017.12.001>.

Valavi, Roozbeh, Jane Elith, Jose J. Lahoz-Monfort, and Gurutzeta
Guillera-Arroita. 2018. “blockCV: an R package for generating spatially
or environmentally separated folds for k-fold cross-validation of
species distribution models.” *bioRxiv*, June.
<https://doi.org/10.1101/357798>.
