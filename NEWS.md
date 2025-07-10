<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# mlr3spatiotempcv 2.3.3

## compatibility

- Mlr3 1.0.0 (#242).

## Bug fixes

- Use of custom `train_color` and `test_color` in `autoplot()` (#241, #243).

## Chore

- Remove tic.R.

## Continuous integration

- Remove tic badge.

## Uncategorized

- Bump version to 2.3.1.9000.


# mlr3spatiotempcv 2.3.2

- mlr3 compatibility
- JSS paper inclusion
- Roxygen fixes


# mlr3spatiotempcv 2.3.1

- Add support for new {paradox} version (#234)
- Rename `ppoints` param in {CAST} functions to `predpoints` to adhere with the upstream package renaming (#237)
- Update {vdiffr} tests with latest {ggplot2} version


# mlr3spatiotempcv 2.3.0

## Features

- Allow changing point size in all `autoplot()` functions (#231).

## Misc

- Cleanup dicts during unload (#233)
- Pass ellipsis to all `autoplot()` sub-functions.
- Fix mlr3book references
- Compile "vis" vignette also on macOS


# mlr3spatiotempcv 2.2.0

## Features

- Add "knndm" method from package {CAST} (#229)
- Add `label_size` to "spcv_block" to make label size configurable (#227)
- Add `show_omitted` for "spcv_buffer" method (#228)

# mlr3spatiotempcv 2.1.0

- Add support for {blockCV} v3 and bump requirement to >= 3.1.2 (#222)
- Replace {raster} dependency in favor of {terra}
- Remove CLUTO algorithm and method due to CLUTO being non-downloadable anymore (#224)


# mlr3spatiotempcv 2.0.3

- add `label` support for built-in tasks
- adhere to CRAN "noSuggests" policy


# mlr3spatiotempcv 2.0.2

- Add error message when trying to create a `TaskClassifST` or `TaskRegrST` from an `sf` object
- Synchronize `TaskClassifST` or `TaskRegrST` with {mlr3spatial}
- Add support for `mlr_reflections` changes in {mlr3} > 0.13.4
- Adjust "Getting Started" vignette to recent API changes
- `autoplot.ResamplingSptCVCstf()`: Add missing support for argument `axis_label_fontsize` for x and y axes


# mlr3spatiotempcv 2.0.1

## Bugfixes

- `autoplot.ResamplingSptCVCstf`: when multiple folds are requested, the subplots are now returned again (before, the return was empty)
- `autoplot.ResamplingSptCVCstf`: the legend item for the "omitted" observations now displays the correct color and label again


# mlr3spatiotempcv 2.0.0

## Breaking

- Rename task `cookfarm` to `cookfarm_mlr3`.
  This was done to distinguish the `cookfarm` task implementation in {mlr3} better from the original `cookfarm` dataset.
  `cookfarm_mlr3` also now comes with all rows of the upstream `cookfarm` task and not with a random subset as before.
- Rewrite `mlr_resampling_spctcv_cstf` implementation.
  **The method will produce different fold results compared to {mlr3spatiotempcv} <= 1.0.1**.
  This is because of a change/fix in the sampling behavior: before, an (unwanted) stratified sampling was done on `time` and `space` variables.
  While this matched the upstream implementation in {CAST}, this did not match with the actual theoretical underpinning described in the literature.
- {mlr3} API adaptations: `TaskClassifST$new()` and `TaskRegrST$new()` only accepts `DataBackendDataTable`.
  Support for `sf` objects has moved to `as_task_*_st()` which accepts `sf` objects directly and creates the DataBackend behind the scenes.

## Features

- Add support for `DataBackendRaster` (@be-marc, #191).
- `mlr_resampling_spctcv_cstf`: a log message returns the column roles from the Task which are used for partitioning
- The help pages for all methods now describe the methods manually rather than importing the upstream documentation of the respective method.
- `Task*ST` classes now print column roles `space` and `time` (if set) (#198)
- `autoplot()` gains `plot_time_var` argument for 3D visualizations of `mlr_resamplings_sptcv_cstf` resamplings with only 'space' used for partitioning (#197)
- Vignette updates

## Bugfixes

- All {mlr3spatiotempcv} methods now comply with the {mlr3} man file declaration logic.

## Misc

- Escape all examples and tests for non-installed packages.
- The `cookfarm_mlr3` task now sets column roles "space" and "time" for variables `SOURCEID` and `Date`, respectively.
- Harden CLUTO tests (#182)
- Large update for the "spatiotemporal" section in the [mlr3book](https://mlr3book.mlr-org.com/)


# mlr3spatiotempcv 1.0.1

- Fixed a issue which caused coordinates to appear in the feature set when a data.frame was supplied (#166, @be-marc)
- Add `autoplot()` support for `"groups"` column role in `rsmp("cv")`


# mlr3spatiotempcv 1.0.0

## Breaking

- `autoplot()`: removed argument `crs`.
  The CRS is now inferred from the supplied Task.
  Setting a different CRS than the task might lead to spurious issues and the initial idea of changing the CRS for plotting to have proper axes labeling does not apply (anymore) (#144)

## Features

- Added `autoplot()` support for `ResamplingCustomCV` (#140)

## Bug fixes

- `"spcv_block"`: Assert error if folds > 2 when `selection = "checkerboard"` (#150)
- Fixed row duplication when creating `TaskRegrST` tasks from `sf` objects (#152)

## Miscellaneous

- Upgrade tests to {vdiffr} 1.0.0
- Add {rgdal} to suggests and required it in `"spcv_block"` since it is required in {blockCV} >= 2.1.4 and {sf} >= 1.0


# mlr3spatiotempcv 0.4.0

## Features

- Support clustering coords only for `"sptcv_cluto"`
- Add `as_task_*` S3 generics: `as_task_classif_st.data.frame()`, `as_task_classif_st.DataBackend()`, `as_task_classif_st.sf()`, `as_task_regr_st.data.frame()`, `as_task_regr_st.DataBackend()`, `as_task_regr_st.sf()`, `as_task_classif.TaskClassifST()`, `as_task_regr.TaskRegrST()` (#99)
- Add `"spcv_tiles"` and `"repeated_spcv_tiles"` (#121)
- Add `"spcv_disc"` (#115)

## Bug Fixes

- Fixed train set issues for `sptcv_cstf()` with space and time var (#135)
- Fixed `$folds()` active binding returning wrong fold number (#120)
- Add missing `man` IDs (#122)

## Misc

- Add example 2D spatial plots to spatiotemp-viz vignette
- Add {caret} to Suggests
- "Cstf" methods: remove arguments in favor of param set to align with other methods (#122)
- Inherit documentation from upstream functions (#117)
- Vignette: Update and categorize table listing all implemented methods

# mlr3spatiotempcv 0.3.0

## New Features

- `autoplot.ResamplingSptCVCstf()`: add 2D plotting method (#106)
- `autoplot.ResamplingSptCVCstf()`: add arguments `show_omitted` and `static_image` (#100)
- `autoplot()` (all methods): allow adjusting point size via `...` (#98)

## Maintenance

- Remove {GSIF} package due to CRAN archival and host the `cookfarm` dataset standalone
- Use `Cstf` method for spatiotemporal viz vignette
- Fix help page content of `ResamplingRepeatedSptCVCstf` (beforehand the Cluto method was referenced accidentally)
- Fix segfault in `autoplot.ResamplingSpcvBlock` example when rendering pkgdown site (unclear why this happens when `show_labels = TRUE`)
- Update `autoplot()` examples and related documentation
- Remove duplicate resources in Tasks "see also" fields
- Skip a test on Solaris and macOS 3.6
- Optimize "Spatiotemporal Visualization" vignette


# mlr3spatiotempcv 0.2.1

- Add support for `rasterLayer` argument in `blockCV::spatialBlock()` (#94)
- Ensure that `blockCV::spatialBlock()` functions actually returns the same result when invoked via {mlr3spatiotempcv} (#93).
  Among other issues, `blockCV::spatialBlock(selection = "checkerboard")` was ignored.
- Get coordinates names from {sf} objects dynamically.
  Before some functions would have errored if the coordinate names were not named "x" and "y".


# mlr3spatiotempcv 0.2.0

- Add support for {sf} objects for Task*ST creation (#90)
- "Getting Started" vignette: add example how to create a spatial task


# mlr3spatiotempcv 0.1.1

- CRAN-related changes
- Support ordered factors in TaskClassifST creation (#84)


# mlr3spatiotempcv 0.1.0

- Initial CRAN release
