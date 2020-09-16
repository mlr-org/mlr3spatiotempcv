# mlr3spatiotempcv 0.0.0.9009

- set `self$task_nrow` during construction in all resampling methods (#75)


# mlr3spatiotempcv 0.0.0.9008

- Adapt to mlr3 "extra_args" approach for tasks arguments
- Use {cli} for warnings (#71)
- Enable visual tests via {vdiffr} on CI (#72)
- Store SpCVBuffer more efficiently (#70)


# mlr3spatiotempcv 0.0.0.9007

- add pkgdown reference grouping, (fixes #67)
- use underscores instead of hyphens for resampling method naming
- Optimize `autoplot()` and add `plot()`
- update registering of column roles: append instead of overwrite


# mlr3spatiotempcv 0.0.0.9006

- Add `ResamplingSptCVCluto` (#53)


# mlr3spatiotempcv 0.0.0.9005

* add/update unit tests, especially for `SpCVEnv`
* `SpCVBuffer`: Enables spDataType and addBG parameter
* `SpCVBuffer`: Adds parameter tests
* `SpCVBuffer`: Fixes train and test set storage
* `SpCVBuffer`: Add 6x6 point grid tasks (two-class, multi-class and continuous response)
* `SpCVBuffer`: Add unit tests for `ResamplingSpCVBuffer` with 6x6 point grid tasks
* `SpCVBuffer`: Update `autoplot` unit test `vdiffr` images
* `SpCVEnv`: Remove setting of `cols` and `rows`
* `SpCVEnv`: Add param `features` to parameter set
* "Getting Started" vignette (#34)
* `autoplot()`: support CV and RepeatedCV (#37)
* Add "cookfarm" example regression task (spatiotemp)


# mlr3spatiotempcv 0.0.0.9004

* Add ResamplingRepeatedSpCVBlock (#35)
* Add ResamplingRepeatedSpCVEnv (#32)
* Remove `stratify` argument (#33)
* Document defaults of param `folds`
* Use mlr sugar notation in more places


# mlr3spatiotempcv 0.0.0.9003

* add class `RepeatedSpCVCoords` (#30)
* skip `vdiffr` tests on CI
* add a "Getting Started" vignette
* use .bib file for references
* save `ecuador` data without rownames to avoid warning


# mlr3spatiotempcv 0.0.0.9002

* rewrite all classes to roxygen2 R6 notation
* restructure zzz.R following mlr3proba approach
* use `train_set()` and `test_set()` from mlr3::Resampling
* fix `iters()` of spcv-buffer
* test package on Circle CI
* update README


# mlr3spatiotempcv 0.0.0.9001

* add NEWS.md




