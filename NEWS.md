<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# mlr3spatiotempcv 2.0.1

## Bug fixes 

- use default extra_args (#166).



## Features 

- support Task with DataBackendVector (#173).



## Chore 

- unify testthat.R



## lifecycle 

- stable



## tests 

- lower lgr log level in tests

- use data without NA for cluto

- escape all examples for non-installed pkgs



## article 

- readd function names in header titles



## paper 

- various style fixes



## testthat 

- test in parallel



## travis 

- no branch builds

- require spatial stack



## GHA 

- update tic templates

- install spatial libs also on macOS release

- Fix linux block



## pkgdown 

- use mlr3pkgdowntemplate



## Uncategorized 

- fix return of plotly subplots in sptcv_cstf autoplot()

- fix "omitted" color label sptcv_cstf plots and don't return plot invisibly

- Add `plot_time_var` argument to `autoplot()` for 3D plotting (#197)

- feat: Print column roles `space`, `time` or `plot` in Task (#198)

- chore: remove whitespace

- chore: rename task cookfarm to `cookfarm_mlr3`

- feat: Add support for `DataBackendRaster` (@33069354+be-marc, #191)

- cstf: print info log message which column roles are used for partitioning

- cookfarm task: include NA observations and set column roles "space" and "time" by default

- add support for mlr3 man file notation for resampling methods

- update help pages (@33069354+be-marc, #190)

- fix typo (@33069354+be-marc, #190)

- turn off codecov for cluto (@33069354+be-marc, #190)

- rewrite CAST implementation (@66853113+pre-commit-ci[bot], #174)

- Use full cookfarm dataset (#181)

- use full cookfarm dataset

- clean preprocess script

- update vdiffr snapshots

- remove custom levels for time+space plot

- Robustify CLUTO test setup (#182)

- add cluto method

- optimize "choosing a method"

- use pdf_book reference mechanism

- use cache

- adjust figure placements

- restructure chapters

- write CSTF methods

- SptCV methods: move arguments from `instantiate()` to constructor

- replace {cowplot} by {patchwork} and make visualization of multiple folds more flexible and eye appealing

- return descriptive error message if a non-spatiotemporal task was provided (#74)

- Adapt to mlr3 "extra_args" approach for tasks arguments

- add pkgdown reference grouping, (fixes #67)

- use underscores instead of hyphens for resampling method naming

- Optimize `autoplot()` and add `plot()`

- update registering of column roles: append instead of overwrite

- Add `ResamplingSptCVCluto` (#53)

- add/update unit tests, especially for `SpCVEnv`

- `SpCVBuffer`: Enables spDataType and addBG parameter

- `SpCVBuffer`: Adds parameter tests

- `SpCVBuffer`: Fixes train and test set storage

- `SpCVBuffer`: Add 6x6 point grid tasks (two-class, multi-class and continuous response)

- `SpCVBuffer`: Add unit tests for `ResamplingSpCVBuffer` with 6x6 point grid tasks

- `SpCVBuffer`: Update `autoplot` unit test `vdiffr` images

- `SpCVEnv`: Remove setting of `cols` and `rows`

- `SpCVEnv`: Add param `features` to parameter set

- "Getting Started" vignette (#34)

- `autoplot()`: support CV and RepeatedCV (#37)

- Add ResamplingRepeatedSpCVBlock (#35)

- Document defaults of param `folds`

- add class `RepeatedSpCVCoords`

- skip vdiffr tests on CI

- add a "Getting Started" vignette

- use .bib file for references

- save ecuador data without rownamesto avoid warning

- rewrite all classes to roxygen2 R6 notation

- restructure zzz.R following mlr3proba approach

- resave ecuador data without rownames

- use `train_set()` and `test_set()` from mlr3::Resampling

- fix `iters()` of spcv-buffer

- test package on Circle CI

- update README

- skip vdiffr tests on CI

- ...

- ...

- ...

- stringr::str_detect -> grepl

- require_namespaces for suggested pkgs

- stringr::str_detect -> grepl

- fix Rd

- revalidate plots in tests

- simplify

- upd spcv-block, update plots

- add --run-donttest

- try resolving merge conflict

- add TaskRegrST

- upd TaskClassifST with upstream TaskClassif changes

- fix example

- remove example for now

- make spcv tests generic

- suggest stringr

- fix typo

- test grouping and stratification combinations

- add spcv-block specific test

- remove unused .combine function

- test setter and getter, test cloning

- fix test and train getter tests

- rename

- move buffer tests into own file

- optimize error message

- add task diplodia

- add spcv-env test file

- <- to =

- fix spcv-env tst

- need assignment operator once

- typo

- format roxygen

- upd README badges

- add test for TaskClassifST

- formot roxygen

- minor notes

- last coverage %


# mlr3spatiotempcv 2.0.0.9002

- fix return of plotly subplots in in `autoplot()` for `sptcv_cstf` when multiple folds are selected

# mlr3spatiotempcv 2.0.0.9001

## Bugfixes

- fix display of the "omitted" color label in `autoplot()` for `sptcv_cstf`


# mlr3spatiotempcv 2.0.0

## Breaking

- Rename task `cookfarm` to `cookfarm_mlr3`.
  This was done to distinguish the `cookfarm` task implementation in {mlr3} better from the original `cookfarm` dataset.
  `cookfarm_mlr3` also now comes with all rows of the upstream `cookfarm` task and not with a random subset as before.
- Rewrite `mlr_resampling_spctcv_cstf` implementation.
  **The method will produce different fold results compared to {mlr3spatiotempcv} <= 1.0.1**.
  This is because of a change/fix in the sampling behavior: before, an (unwanted) stratified sampling was done on `time` and `space` variables.
  While this matched the upstream implementation in {CAST}, this did not match with the actual theoretical underpinning described in the literature.

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
