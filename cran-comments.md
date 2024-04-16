mlr3spatiotempcv 2.3.1

## Cran Repository Policy

- [ ] Reviewed CRP last edited 2024-04-04.

See changes at https://github.com/eddelbuettel/crp/compare/master@%7B2023-08-15%7D...master@%7B2024-04-04%7D

## R CMD check results

- [x] Checked locally, R 4.3.3
- [ ] Checked on CI system, R 4.3.3
- [ ] Checked on win-builder, R devel

Check the boxes above after successful execution and remove this line. Then run `fledge::release()`.

## Current CRAN check results

- [x] Checked on 2024-04-16, problems found: https://cran.r-project.org/web/checks/check_results_mlr3spatiotempcv.html
- [ ] ERROR: r-devel-linux-x86_64-debian-clang, r-devel-linux-x86_64-debian-gcc, r-patched-linux-x86_64, r-release-linux-x86_64
     Running examples in ‘mlr3spatiotempcv-Ex.R’ failed
     The error most likely occurred in:
     
     > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
     > ### Name: mlr_resamplings_repeated_spcv_knndm
     > ### Title: (CAST) Repeated K-fold Nearest Neighbour Distance Matching
     > ### Aliases: mlr_resamplings_repeated_spcv_knndm
     > ###   ResamplingRepeatedSpCVKnndm
     > 
     > ### ** Examples
     > 
     > library(mlr3)
     > library(mlr3spatial)
     
     Attaching package: ‘mlr3spatial’
     
     The following objects are masked from ‘package:mlr3spatiotempcv’:
     
     TaskClassifST, TaskRegrST, as_task_classif_st,
     as_task_classif_st.DataBackend, as_task_classif_st.TaskClassifST,
     as_task_classif_st.data.frame, as_task_classif_st.sf,
     as_task_regr_st, as_task_regr_st.DataBackend,
     as_task_regr_st.TaskClassifST, as_task_regr_st.TaskRegrST,
     as_task_regr_st.data.frame, as_task_regr_st.sf
     
     > set.seed(42)
     > simarea = list(matrix(c(0, 0, 0, 100, 100, 100, 100, 0, 0, 0), ncol = 2, byrow = TRUE))
     > simarea = sf::st_polygon(simarea)
     > train_points = sf::st_sample(simarea, 1000, type = "random")
     > train_points = sf::st_as_sf(train_points)
     > train_points$target = as.factor(sample(c("TRUE", "FALSE"), 1000, replace = TRUE))
     > pred_points = sf::st_sample(simarea, 1000, type = "regular")
     > 
     > task = mlr3spatial::as_task_classif_st(sf::st_as_sf(train_points), "target", positive = "TRUE")
     > 
     > cv_knndm = rsmp("repeated_spcv_knndm", ppoints = pred_points, repeats = 2)
     > cv_knndm$instantiate(task)
     Error in CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain,  : 
     unused argument (ppoints = self$param_set$values$ppoints)
     Calls: <Anonymous> ... .__ResamplingRepeatedSpCVKnndm__.sample -> map -> lapply -> FUN
     Execution halted
- [ ] ERROR: r-devel-linux-x86_64-debian-clang
     Running ‘testthat.R’ [118s/81s]
     Running the tests in ‘tests/testthat.R’ failed.
     Complete output:
     > if (requireNamespace("testthat", quietly = TRUE)) {
     +   library("checkmate")
     +   library("testthat")
     +   library("mlr3spatiotempcv")
     +   test_check("mlr3spatiotempcv")
     + }
     Loading required package: mlr3
     Starting 2 test processes
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     
     ══ Skipped tests (23) ══════════════════════════════════════════════════════════
     • On CRAN (18): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-autoplot_buffer.R:19:3'
     • On Linux (2): 'test-2-autoplot.R:8:3', 'test-2-autoplot.R:54:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-2-autoplot.R:331:3'): plot() works for 'spcv_knndm' ────────────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-2-autoplot.R:331:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:21:3'): folds can be printed ─────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:21:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:31:3'): reps can be printed ──────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:31:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:41:3'): resampling iterations equals folds * repeats ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:41:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingSpCVKnndm.R:10:3'): mlr3spatiotempcv indices are the same as CAST knndm: modeldomain ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:10:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     ── Error ('test-ResamplingSpCVKnndm.R:38:3'): mlr3spatiotempcv indices are the same as CAST knndm: ppoints ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:38:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     Deleting unused snapshots:
     • 1-autoplot/autoplot-show-blocks-true-show-labels-true.svg
     • 1-autoplot/custom-cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1-2.svg
     • 1-autoplot/custom-cv-fold-1-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1.svg
     • 1-autoplot/custom-cv-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2-groups-col-role.svg
     • 1-autoplot/cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2.svg
     • 1-autoplot/cv-fold-1-groups-col-role.svg
     • 1-autoplot/cv-fold-1-sample-fold-n.svg
     • 1-autoplot/cv-fold-1.svg
     • 1-autoplot/cv-sample-fold-n.svg
     • 1-autoplot/repcv-fold-1-2-rep-1.svg
     • 1-autoplot/repcv-fold-1-2-rep-2.svg
     • 1-autoplot/repcv-fold-1-rep-1.svg
     • 1-autoplot/repcv-fold-1-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-sample-fold-n.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvenv-fold-1-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-rep-2.svg
     • 1-autoplot/spcvblock-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1-2.svg
     • 1-autoplot/spcvblock-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1.svg
     • 1-autoplot/spcvblock-sample-fold-n.svg
     • 1-autoplot/spcvcoords-fold-1-2.svg
     • 1-autoplot/spcvcoords-fold-1.svg
     • 1-autoplot/spcvenv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1-2.svg
     • 1-autoplot/spcvenv-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1.svg
     • 1-autoplot/spcvenv-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2.svg
     • 2-autoplot/repspcvtiles-fold-1-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1.svg
     • 2-autoplot/repspcvtiles-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-show-omitted.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-1.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-1-sample-fold-n.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-2.svg
     • 2-autoplot/spcvdisc-fold-1-2.svg
     • 2-autoplot/spcvdisc-fold-1.svg
     • 2-autoplot/spcvdisc-show-omitted.svg
     • 2-autoplot/spcvknndm-all-test-sets.svg
     • 2-autoplot/spcvknndm-fold-1-2.svg
     • 2-autoplot/spcvknndm-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-space-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-sample-fold-n.svg
     • autoplot_buffer/spcvbuffer-fold-1-2.svg
     Error: Test failures
     Execution halted
- [ ] ERROR: r-devel-linux-x86_64-debian-gcc
     Running ‘testthat.R’ [99s/92s]
     Running the tests in ‘tests/testthat.R’ failed.
     Complete output:
     > if (requireNamespace("testthat", quietly = TRUE)) {
     +   library("checkmate")
     +   library("testthat")
     +   library("mlr3spatiotempcv")
     +   test_check("mlr3spatiotempcv")
     + }
     Loading required package: mlr3
     Starting 2 test processes
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     
     ══ Skipped tests (23) ══════════════════════════════════════════════════════════
     • On CRAN (18): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-autoplot_buffer.R:19:3'
     • On Linux (2): 'test-2-autoplot.R:8:3', 'test-2-autoplot.R:54:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:21:3'): folds can be printed ─────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:21:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:31:3'): reps can be printed ──────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:31:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:41:3'): resampling iterations equals folds * repeats ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:41:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-2-autoplot.R:331:3'): plot() works for 'spcv_knndm' ────────────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-2-autoplot.R:331:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingSpCVKnndm.R:10:3'): mlr3spatiotempcv indices are the same as CAST knndm: modeldomain ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:10:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     ── Error ('test-ResamplingSpCVKnndm.R:38:3'): mlr3spatiotempcv indices are the same as CAST knndm: ppoints ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:38:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     Deleting unused snapshots:
     • 1-autoplot/autoplot-show-blocks-true-show-labels-true.svg
     • 1-autoplot/custom-cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1-2.svg
     • 1-autoplot/custom-cv-fold-1-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1.svg
     • 1-autoplot/custom-cv-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2-groups-col-role.svg
     • 1-autoplot/cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2.svg
     • 1-autoplot/cv-fold-1-groups-col-role.svg
     • 1-autoplot/cv-fold-1-sample-fold-n.svg
     • 1-autoplot/cv-fold-1.svg
     • 1-autoplot/cv-sample-fold-n.svg
     • 1-autoplot/repcv-fold-1-2-rep-1.svg
     • 1-autoplot/repcv-fold-1-2-rep-2.svg
     • 1-autoplot/repcv-fold-1-rep-1.svg
     • 1-autoplot/repcv-fold-1-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-sample-fold-n.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvenv-fold-1-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-rep-2.svg
     • 1-autoplot/spcvblock-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1-2.svg
     • 1-autoplot/spcvblock-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1.svg
     • 1-autoplot/spcvblock-sample-fold-n.svg
     • 1-autoplot/spcvcoords-fold-1-2.svg
     • 1-autoplot/spcvcoords-fold-1.svg
     • 1-autoplot/spcvenv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1-2.svg
     • 1-autoplot/spcvenv-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1.svg
     • 1-autoplot/spcvenv-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2.svg
     • 2-autoplot/repspcvtiles-fold-1-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1.svg
     • 2-autoplot/repspcvtiles-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-show-omitted.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-1.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-1-sample-fold-n.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-2.svg
     • 2-autoplot/spcvdisc-fold-1-2.svg
     • 2-autoplot/spcvdisc-fold-1.svg
     • 2-autoplot/spcvdisc-show-omitted.svg
     • 2-autoplot/spcvknndm-all-test-sets.svg
     • 2-autoplot/spcvknndm-fold-1-2.svg
     • 2-autoplot/spcvknndm-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-space-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-sample-fold-n.svg
     • autoplot_buffer/spcvbuffer-fold-1-2.svg
     Error: Test failures
     Execution halted
- [ ] ERROR: r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc, r-prerel-windows-x86_64, r-release-windows-x86_64, r-oldrel-windows-x86_64
     Running examples in ‘mlr3spatiotempcv-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: mlr_resamplings_repeated_spcv_knndm
     > ### Title: (CAST) Repeated K-fold Nearest Neighbour Distance Matching
     > ### Aliases: mlr_resamplings_repeated_spcv_knndm
     > ###   ResamplingRepeatedSpCVKnndm
     > 
     > ### ** Examples
     > 
     > library(mlr3)
     > library(mlr3spatial)
     
     Attaching package: ‘mlr3spatial’
     
     The following objects are masked from ‘package:mlr3spatiotempcv’:
     
     TaskClassifST, TaskRegrST, as_task_classif_st,
     as_task_classif_st.DataBackend, as_task_classif_st.TaskClassifST,
     as_task_classif_st.data.frame, as_task_classif_st.sf,
     as_task_regr_st, as_task_regr_st.DataBackend,
     as_task_regr_st.TaskClassifST, as_task_regr_st.TaskRegrST,
     as_task_regr_st.data.frame, as_task_regr_st.sf
     
     > set.seed(42)
     > simarea = list(matrix(c(0, 0, 0, 100, 100, 100, 100, 0, 0, 0), ncol = 2, byrow = TRUE))
     > simarea = sf::st_polygon(simarea)
     > train_points = sf::st_sample(simarea, 1000, type = "random")
     > train_points = sf::st_as_sf(train_points)
     > train_points$target = as.factor(sample(c("TRUE", "FALSE"), 1000, replace = TRUE))
     > pred_points = sf::st_sample(simarea, 1000, type = "regular")
     > 
     > task = mlr3spatial::as_task_classif_st(sf::st_as_sf(train_points), "target", positive = "TRUE")
     > 
     > cv_knndm = rsmp("repeated_spcv_knndm", ppoints = pred_points, repeats = 2)
     > cv_knndm$instantiate(task)
     Error in CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain,  : 
     unused argument (ppoints = self$param_set$values$ppoints)
     Calls: <Anonymous> ... .__ResamplingRepeatedSpCVKnndm__.sample -> map -> lapply -> FUN
     Execution halted
- [ ] ERROR: r-devel-linux-x86_64-fedora-clang
     Running ‘testthat.R’ [154s/76s]
     Running the tests in ‘tests/testthat.R’ failed.
     Complete output:
     > if (requireNamespace("testthat", quietly = TRUE)) {
     +   library("checkmate")
     +   library("testthat")
     +   library("mlr3spatiotempcv")
     +   test_check("mlr3spatiotempcv")
     + }
     Loading required package: mlr3
     Starting 2 test processes
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     
     ══ Skipped tests (23) ══════════════════════════════════════════════════════════
     • On CRAN (18): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-autoplot_buffer.R:19:3'
     • On Linux (2): 'test-2-autoplot.R:8:3', 'test-2-autoplot.R:54:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-2-autoplot.R:331:3'): plot() works for 'spcv_knndm' ────────────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-2-autoplot.R:331:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:21:3'): folds can be printed ─────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:21:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:31:3'): reps can be printed ──────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:31:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:41:3'): resampling iterations equals folds * repeats ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:41:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingSpCVKnndm.R:10:3'): mlr3spatiotempcv indices are the same as CAST knndm: modeldomain ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:10:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     ── Error ('test-ResamplingSpCVKnndm.R:38:3'): mlr3spatiotempcv indices are the same as CAST knndm: ppoints ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:38:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     Deleting unused snapshots:
     • 1-autoplot/autoplot-show-blocks-true-show-labels-true.svg
     • 1-autoplot/custom-cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1-2.svg
     • 1-autoplot/custom-cv-fold-1-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1.svg
     • 1-autoplot/custom-cv-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2-groups-col-role.svg
     • 1-autoplot/cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2.svg
     • 1-autoplot/cv-fold-1-groups-col-role.svg
     • 1-autoplot/cv-fold-1-sample-fold-n.svg
     • 1-autoplot/cv-fold-1.svg
     • 1-autoplot/cv-sample-fold-n.svg
     • 1-autoplot/repcv-fold-1-2-rep-1.svg
     • 1-autoplot/repcv-fold-1-2-rep-2.svg
     • 1-autoplot/repcv-fold-1-rep-1.svg
     • 1-autoplot/repcv-fold-1-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-sample-fold-n.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvenv-fold-1-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-rep-2.svg
     • 1-autoplot/spcvblock-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1-2.svg
     • 1-autoplot/spcvblock-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1.svg
     • 1-autoplot/spcvblock-sample-fold-n.svg
     • 1-autoplot/spcvcoords-fold-1-2.svg
     • 1-autoplot/spcvcoords-fold-1.svg
     • 1-autoplot/spcvenv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1-2.svg
     • 1-autoplot/spcvenv-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1.svg
     • 1-autoplot/spcvenv-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2.svg
     • 2-autoplot/repspcvtiles-fold-1-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1.svg
     • 2-autoplot/repspcvtiles-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-show-omitted.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-1.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-1-sample-fold-n.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-2.svg
     • 2-autoplot/spcvdisc-fold-1-2.svg
     • 2-autoplot/spcvdisc-fold-1.svg
     • 2-autoplot/spcvdisc-show-omitted.svg
     • 2-autoplot/spcvknndm-all-test-sets.svg
     • 2-autoplot/spcvknndm-fold-1-2.svg
     • 2-autoplot/spcvknndm-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-space-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-sample-fold-n.svg
     • autoplot_buffer/spcvbuffer-fold-1-2.svg
     Error: Test failures
     Execution halted
- [ ] ERROR: r-devel-linux-x86_64-fedora-gcc
     Running ‘testthat.R’ [142s/87s]
     Running the tests in ‘tests/testthat.R’ failed.
     Complete output:
     > if (requireNamespace("testthat", quietly = TRUE)) {
     +   library("checkmate")
     +   library("testthat")
     +   library("mlr3spatiotempcv")
     +   test_check("mlr3spatiotempcv")
     + }
     Loading required package: mlr3
     Starting 2 test processes
     [ FAIL 6 | WARN 0 | SKIP 22 | PASS 1202 ]
     
     ══ Skipped tests (22) ══════════════════════════════════════════════════════════
     • On CRAN (19): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-autoplot_buffer.R:19:3',
     'test-mlr3pipelines-graph-integration.R:5:3'
     • On Linux (2): 'test-2-autoplot.R:8:3', 'test-2-autoplot.R:54:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-2-autoplot.R:331:3'): plot() works for 'spcv_knndm' ────────────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-2-autoplot.R:331:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:21:3'): folds can be printed ─────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:21:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:31:3'): reps can be printed ──────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:31:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:41:3'): resampling iterations equals folds * repeats ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:41:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingSpCVKnndm.R:10:3'): mlr3spatiotempcv indices are the same as CAST knndm: modeldomain ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:10:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     ── Error ('test-ResamplingSpCVKnndm.R:38:3'): mlr3spatiotempcv indices are the same as CAST knndm: ppoints ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:38:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     
     [ FAIL 6 | WARN 0 | SKIP 22 | PASS 1202 ]
     Deleting unused snapshots:
     • 1-autoplot/autoplot-show-blocks-true-show-labels-true.svg
     • 1-autoplot/custom-cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1-2.svg
     • 1-autoplot/custom-cv-fold-1-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1.svg
     • 1-autoplot/custom-cv-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2-groups-col-role.svg
     • 1-autoplot/cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2.svg
     • 1-autoplot/cv-fold-1-groups-col-role.svg
     • 1-autoplot/cv-fold-1-sample-fold-n.svg
     • 1-autoplot/cv-fold-1.svg
     • 1-autoplot/cv-sample-fold-n.svg
     • 1-autoplot/repcv-fold-1-2-rep-1.svg
     • 1-autoplot/repcv-fold-1-2-rep-2.svg
     • 1-autoplot/repcv-fold-1-rep-1.svg
     • 1-autoplot/repcv-fold-1-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-sample-fold-n.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvenv-fold-1-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-rep-2.svg
     • 1-autoplot/spcvblock-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1-2.svg
     • 1-autoplot/spcvblock-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1.svg
     • 1-autoplot/spcvblock-sample-fold-n.svg
     • 1-autoplot/spcvcoords-fold-1-2.svg
     • 1-autoplot/spcvcoords-fold-1.svg
     • 1-autoplot/spcvenv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1-2.svg
     • 1-autoplot/spcvenv-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1.svg
     • 1-autoplot/spcvenv-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2.svg
     • 2-autoplot/repspcvtiles-fold-1-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1.svg
     • 2-autoplot/repspcvtiles-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-show-omitted.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-1.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-1-sample-fold-n.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-2.svg
     • 2-autoplot/spcvdisc-fold-1-2.svg
     • 2-autoplot/spcvdisc-fold-1.svg
     • 2-autoplot/spcvdisc-show-omitted.svg
     • 2-autoplot/spcvknndm-all-test-sets.svg
     • 2-autoplot/spcvknndm-fold-1-2.svg
     • 2-autoplot/spcvknndm-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-space-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-sample-fold-n.svg
     • autoplot_buffer/spcvbuffer-fold-1-2.svg
     Error: Test failures
     Execution halted
- [ ] ERROR: r-prerel-windows-x86_64
     Running 'testthat.R' [46s]
     Running the tests in 'tests/testthat.R' failed.
     Complete output:
     > if (requireNamespace("testthat", quietly = TRUE)) {
     +   library("checkmate")
     +   library("testthat")
     +   library("mlr3spatiotempcv")
     +   test_check("mlr3spatiotempcv")
     + }
     Loading required package: mlr3
     Starting 2 test processes
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     
     ══ Skipped tests (23) ══════════════════════════════════════════════════════════
     • On CRAN (18): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-autoplot_buffer.R:19:3'
     • On Windows (2): 'test-2-autoplot.R:9:3', 'test-2-autoplot.R:55:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:21:3'): folds can be printed ─────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:21:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:31:3'): reps can be printed ──────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:31:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:41:3'): resampling iterations equals folds * repeats ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:41:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingSpCVKnndm.R:10:3'): mlr3spatiotempcv indices are the same as CAST knndm: modeldomain ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:10:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     ── Error ('test-ResamplingSpCVKnndm.R:38:3'): mlr3spatiotempcv indices are the same as CAST knndm: ppoints ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:38:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     ── Error ('test-2-autoplot.R:331:3'): plot() works for 'spcv_knndm' ────────────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-2-autoplot.R:331:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     Deleting unused snapshots:
     • 1-autoplot/autoplot-show-blocks-true-show-labels-true.svg
     • 1-autoplot/custom-cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1-2.svg
     • 1-autoplot/custom-cv-fold-1-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1.svg
     • 1-autoplot/custom-cv-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2-groups-col-role.svg
     • 1-autoplot/cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2.svg
     • 1-autoplot/cv-fold-1-groups-col-role.svg
     • 1-autoplot/cv-fold-1-sample-fold-n.svg
     • 1-autoplot/cv-fold-1.svg
     • 1-autoplot/cv-sample-fold-n.svg
     • 1-autoplot/repcv-fold-1-2-rep-1.svg
     • 1-autoplot/repcv-fold-1-2-rep-2.svg
     • 1-autoplot/repcv-fold-1-rep-1.svg
     • 1-autoplot/repcv-fold-1-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-sample-fold-n.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvenv-fold-1-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-rep-2.svg
     • 1-autoplot/spcvblock-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1-2.svg
     • 1-autoplot/spcvblock-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1.svg
     • 1-autoplot/spcvblock-sample-fold-n.svg
     • 1-autoplot/spcvcoords-fold-1-2.svg
     • 1-autoplot/spcvcoords-fold-1.svg
     • 1-autoplot/spcvenv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1-2.svg
     • 1-autoplot/spcvenv-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1.svg
     • 1-autoplot/spcvenv-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2.svg
     • 2-autoplot/repspcvtiles-fold-1-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1.svg
     • 2-autoplot/repspcvtiles-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-show-omitted.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-1.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-1-sample-fold-n.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-2.svg
     • 2-autoplot/spcvdisc-fold-1-2.svg
     • 2-autoplot/spcvdisc-fold-1.svg
     • 2-autoplot/spcvdisc-show-omitted.svg
     • 2-autoplot/spcvknndm-all-test-sets.svg
     • 2-autoplot/spcvknndm-fold-1-2.svg
     • 2-autoplot/spcvknndm-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-space-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-sample-fold-n.svg
     • autoplot_buffer/spcvbuffer-fold-1-2.svg
     Error: Test failures
     Execution halted
- [ ] ERROR: r-patched-linux-x86_64
     Running ‘testthat.R’ [124s/87s]
     Running the tests in ‘tests/testthat.R’ failed.
     Complete output:
     > if (requireNamespace("testthat", quietly = TRUE)) {
     +   library("checkmate")
     +   library("testthat")
     +   library("mlr3spatiotempcv")
     +   test_check("mlr3spatiotempcv")
     + }
     Loading required package: mlr3
     Starting 2 test processes
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     
     ══ Skipped tests (23) ══════════════════════════════════════════════════════════
     • On CRAN (18): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-autoplot_buffer.R:19:3'
     • On Linux (2): 'test-2-autoplot.R:8:3', 'test-2-autoplot.R:54:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:21:3'): folds can be printed ─────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:21:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:31:3'): reps can be printed ──────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:31:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:41:3'): resampling iterations equals folds * repeats ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:41:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-2-autoplot.R:331:3'): plot() works for 'spcv_knndm' ────────────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-2-autoplot.R:331:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingSpCVKnndm.R:10:3'): mlr3spatiotempcv indices are the same as CAST knndm: modeldomain ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:10:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     ── Error ('test-ResamplingSpCVKnndm.R:38:3'): mlr3spatiotempcv indices are the same as CAST knndm: ppoints ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:38:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     Deleting unused snapshots:
     • 1-autoplot/autoplot-show-blocks-true-show-labels-true.svg
     • 1-autoplot/custom-cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1-2.svg
     • 1-autoplot/custom-cv-fold-1-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1.svg
     • 1-autoplot/custom-cv-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2-groups-col-role.svg
     • 1-autoplot/cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2.svg
     • 1-autoplot/cv-fold-1-groups-col-role.svg
     • 1-autoplot/cv-fold-1-sample-fold-n.svg
     • 1-autoplot/cv-fold-1.svg
     • 1-autoplot/cv-sample-fold-n.svg
     • 1-autoplot/repcv-fold-1-2-rep-1.svg
     • 1-autoplot/repcv-fold-1-2-rep-2.svg
     • 1-autoplot/repcv-fold-1-rep-1.svg
     • 1-autoplot/repcv-fold-1-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-sample-fold-n.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvenv-fold-1-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-rep-2.svg
     • 1-autoplot/spcvblock-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1-2.svg
     • 1-autoplot/spcvblock-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1.svg
     • 1-autoplot/spcvblock-sample-fold-n.svg
     • 1-autoplot/spcvcoords-fold-1-2.svg
     • 1-autoplot/spcvcoords-fold-1.svg
     • 1-autoplot/spcvenv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1-2.svg
     • 1-autoplot/spcvenv-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1.svg
     • 1-autoplot/spcvenv-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2.svg
     • 2-autoplot/repspcvtiles-fold-1-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1.svg
     • 2-autoplot/repspcvtiles-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-show-omitted.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-1.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-1-sample-fold-n.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-2.svg
     • 2-autoplot/spcvdisc-fold-1-2.svg
     • 2-autoplot/spcvdisc-fold-1.svg
     • 2-autoplot/spcvdisc-show-omitted.svg
     • 2-autoplot/spcvknndm-all-test-sets.svg
     • 2-autoplot/spcvknndm-fold-1-2.svg
     • 2-autoplot/spcvknndm-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-space-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-sample-fold-n.svg
     • autoplot_buffer/spcvbuffer-fold-1-2.svg
     Error: Test failures
     Execution halted
- [ ] ERROR: r-release-linux-x86_64
     Running ‘testthat.R’ [129s/98s]
     Running the tests in ‘tests/testthat.R’ failed.
     Complete output:
     > if (requireNamespace("testthat", quietly = TRUE)) {
     +   library("checkmate")
     +   library("testthat")
     +   library("mlr3spatiotempcv")
     +   test_check("mlr3spatiotempcv")
     + }
     Loading required package: mlr3
     Starting 2 test processes
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     
     ══ Skipped tests (23) ══════════════════════════════════════════════════════════
     • On CRAN (18): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-autoplot_buffer.R:19:3'
     • On Linux (2): 'test-2-autoplot.R:8:3', 'test-2-autoplot.R:54:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:21:3'): folds can be printed ─────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:21:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:31:3'): reps can be printed ──────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:31:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:41:3'): resampling iterations equals folds * repeats ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:41:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-2-autoplot.R:331:3'): plot() works for 'spcv_knndm' ────────────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-2-autoplot.R:331:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingSpCVKnndm.R:10:3'): mlr3spatiotempcv indices are the same as CAST knndm: modeldomain ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:10:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     ── Error ('test-ResamplingSpCVKnndm.R:38:3'): mlr3spatiotempcv indices are the same as CAST knndm: ppoints ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:38:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     Deleting unused snapshots:
     • 1-autoplot/autoplot-show-blocks-true-show-labels-true.svg
     • 1-autoplot/custom-cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1-2.svg
     • 1-autoplot/custom-cv-fold-1-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1.svg
     • 1-autoplot/custom-cv-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2-groups-col-role.svg
     • 1-autoplot/cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2.svg
     • 1-autoplot/cv-fold-1-groups-col-role.svg
     • 1-autoplot/cv-fold-1-sample-fold-n.svg
     • 1-autoplot/cv-fold-1.svg
     • 1-autoplot/cv-sample-fold-n.svg
     • 1-autoplot/repcv-fold-1-2-rep-1.svg
     • 1-autoplot/repcv-fold-1-2-rep-2.svg
     • 1-autoplot/repcv-fold-1-rep-1.svg
     • 1-autoplot/repcv-fold-1-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-sample-fold-n.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvenv-fold-1-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-rep-2.svg
     • 1-autoplot/spcvblock-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1-2.svg
     • 1-autoplot/spcvblock-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1.svg
     • 1-autoplot/spcvblock-sample-fold-n.svg
     • 1-autoplot/spcvcoords-fold-1-2.svg
     • 1-autoplot/spcvcoords-fold-1.svg
     • 1-autoplot/spcvenv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1-2.svg
     • 1-autoplot/spcvenv-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1.svg
     • 1-autoplot/spcvenv-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2.svg
     • 2-autoplot/repspcvtiles-fold-1-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1.svg
     • 2-autoplot/repspcvtiles-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-show-omitted.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-1.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-1-sample-fold-n.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-2.svg
     • 2-autoplot/spcvdisc-fold-1-2.svg
     • 2-autoplot/spcvdisc-fold-1.svg
     • 2-autoplot/spcvdisc-show-omitted.svg
     • 2-autoplot/spcvknndm-all-test-sets.svg
     • 2-autoplot/spcvknndm-fold-1-2.svg
     • 2-autoplot/spcvknndm-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-space-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-sample-fold-n.svg
     • autoplot_buffer/spcvbuffer-fold-1-2.svg
     Error: Test failures
     Execution halted
- [ ] ERROR: r-release-windows-x86_64
     Running 'testthat.R' [70s]
     Running the tests in 'tests/testthat.R' failed.
     Complete output:
     > if (requireNamespace("testthat", quietly = TRUE)) {
     +   library("checkmate")
     +   library("testthat")
     +   library("mlr3spatiotempcv")
     +   test_check("mlr3spatiotempcv")
     + }
     Loading required package: mlr3
     Starting 2 test processes
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     
     ══ Skipped tests (23) ══════════════════════════════════════════════════════════
     • On CRAN (18): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-autoplot_buffer.R:19:3'
     • On Windows (2): 'test-2-autoplot.R:9:3', 'test-2-autoplot.R:55:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:21:3'): folds can be printed ─────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:21:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:31:3'): reps can be printed ──────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:31:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:41:3'): resampling iterations equals folds * repeats ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:41:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-2-autoplot.R:331:3'): plot() works for 'spcv_knndm' ────────────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-2-autoplot.R:331:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingSpCVKnndm.R:10:3'): mlr3spatiotempcv indices are the same as CAST knndm: modeldomain ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:10:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     ── Error ('test-ResamplingSpCVKnndm.R:38:3'): mlr3spatiotempcv indices are the same as CAST knndm: ppoints ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:38:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     Deleting unused snapshots:
     • 1-autoplot/autoplot-show-blocks-true-show-labels-true.svg
     • 1-autoplot/custom-cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1-2.svg
     • 1-autoplot/custom-cv-fold-1-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1.svg
     • 1-autoplot/custom-cv-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2-groups-col-role.svg
     • 1-autoplot/cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2.svg
     • 1-autoplot/cv-fold-1-groups-col-role.svg
     • 1-autoplot/cv-fold-1-sample-fold-n.svg
     • 1-autoplot/cv-fold-1.svg
     • 1-autoplot/cv-sample-fold-n.svg
     • 1-autoplot/repcv-fold-1-2-rep-1.svg
     • 1-autoplot/repcv-fold-1-2-rep-2.svg
     • 1-autoplot/repcv-fold-1-rep-1.svg
     • 1-autoplot/repcv-fold-1-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-sample-fold-n.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvenv-fold-1-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-rep-2.svg
     • 1-autoplot/spcvblock-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1-2.svg
     • 1-autoplot/spcvblock-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1.svg
     • 1-autoplot/spcvblock-sample-fold-n.svg
     • 1-autoplot/spcvcoords-fold-1-2.svg
     • 1-autoplot/spcvcoords-fold-1.svg
     • 1-autoplot/spcvenv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1-2.svg
     • 1-autoplot/spcvenv-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1.svg
     • 1-autoplot/spcvenv-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2.svg
     • 2-autoplot/repspcvtiles-fold-1-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1.svg
     • 2-autoplot/repspcvtiles-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-show-omitted.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-1.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-1-sample-fold-n.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-2.svg
     • 2-autoplot/spcvdisc-fold-1-2.svg
     • 2-autoplot/spcvdisc-fold-1.svg
     • 2-autoplot/spcvdisc-show-omitted.svg
     • 2-autoplot/spcvknndm-all-test-sets.svg
     • 2-autoplot/spcvknndm-fold-1-2.svg
     • 2-autoplot/spcvknndm-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-space-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-sample-fold-n.svg
     • autoplot_buffer/spcvbuffer-fold-1-2.svg
     Error: Test failures
     Execution halted
- [ ] ERROR: r-oldrel-windows-x86_64
     Running 'testthat.R' [64s]
     Running the tests in 'tests/testthat.R' failed.
     Complete output:
     > if (requireNamespace("testthat", quietly = TRUE)) {
     +   library("checkmate")
     +   library("testthat")
     +   library("mlr3spatiotempcv")
     +   test_check("mlr3spatiotempcv")
     + }
     Loading required package: mlr3
     Starting 2 test processes
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     
     ══ Skipped tests (23) ══════════════════════════════════════════════════════════
     • On CRAN (18): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-autoplot_buffer.R:19:3'
     • On Windows (2): 'test-2-autoplot.R:9:3', 'test-2-autoplot.R:55:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:21:3'): folds can be printed ─────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:21:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:31:3'): reps can be printed ──────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:31:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingRepeatedSpCVKnndm.R:41:3'): resampling iterations equals folds * repeats ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-ResamplingRepeatedSpCVKnndm.R:41:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-2-autoplot.R:331:3'): plot() works for 'spcv_knndm' ────────────
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsp$instantiate(task))) at test-2-autoplot.R:331:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingRepeatedSpCVKnndm__.sample(...)
     9.         └─mlr3misc::map(...)
     10.           └─base::lapply(.x, .f, ...)
     11.             └─mlr3spatiotempcv (local) FUN(X[[i]], ...)
     ── Error ('test-ResamplingSpCVKnndm.R:10:3'): mlr3spatiotempcv indices are the same as CAST knndm: modeldomain ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:10:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     ── Error ('test-ResamplingSpCVKnndm.R:38:3'): mlr3spatiotempcv indices are the same as CAST knndm: ppoints ──
     Error in `CAST::knndm(tpoints = points, modeldomain = self$param_set$values$modeldomain, 
     ppoints = self$param_set$values$ppoints, k = self$param_set$values$folds, 
     maxp = self$param_set$values$maxp, clustering = self$param_set$values$clustering, 
     linkf = self$param_set$values$linkf, samplesize = self$param_set$values$samplesize, 
     sampling = self$param_set$values$sampling)`: unused argument (ppoints = self$param_set$values$ppoints)
     Backtrace:
     ▆
     1. ├─base::suppressMessages(suppressWarnings(rsmp$instantiate(task))) at test-ResamplingSpCVKnndm.R:38:3
     2. │ └─base::withCallingHandlers(...)
     3. ├─base::suppressWarnings(rsmp$instantiate(task))
     4. │ └─base::withCallingHandlers(...)
     5. └─rsmp$instantiate(task)
     6.   └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__instantiate(...)
     7.     └─private$.sample(task$row_ids, task$coordinates(), task$crs)
     8.       └─mlr3spatiotempcv:::.__ResamplingSpCVKnndm__.sample(...)
     
     [ FAIL 6 | WARN 0 | SKIP 23 | PASS 1197 ]
     Deleting unused snapshots:
     • 1-autoplot/autoplot-show-blocks-true-show-labels-true.svg
     • 1-autoplot/custom-cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1-2.svg
     • 1-autoplot/custom-cv-fold-1-sample-fold-n.svg
     • 1-autoplot/custom-cv-fold-1.svg
     • 1-autoplot/custom-cv-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2-groups-col-role.svg
     • 1-autoplot/cv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/cv-fold-1-2.svg
     • 1-autoplot/cv-fold-1-groups-col-role.svg
     • 1-autoplot/cv-fold-1-sample-fold-n.svg
     • 1-autoplot/cv-fold-1.svg
     • 1-autoplot/cv-sample-fold-n.svg
     • 1-autoplot/repcv-fold-1-2-rep-1.svg
     • 1-autoplot/repcv-fold-1-2-rep-2.svg
     • 1-autoplot/repcv-fold-1-rep-1.svg
     • 1-autoplot/repcv-fold-1-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvblock-fold-1-rep-1.svg
     • 1-autoplot/repspcvblock-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-2-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-1.svg
     • 1-autoplot/repspcvcoords-fold-1-rep-2.svg
     • 1-autoplot/repspcvcoords-fold-1-sample-fold-n.svg
     • 1-autoplot/repspcvcoords-sample-fold-n.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-2-rep-2.svg
     • 1-autoplot/repspcvenv-fold-1-rep-1.svg
     • 1-autoplot/repspcvenv-fold-1-rep-2.svg
     • 1-autoplot/spcvblock-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1-2.svg
     • 1-autoplot/spcvblock-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvblock-fold-1.svg
     • 1-autoplot/spcvblock-sample-fold-n.svg
     • 1-autoplot/spcvcoords-fold-1-2.svg
     • 1-autoplot/spcvcoords-fold-1.svg
     • 1-autoplot/spcvenv-fold-1-2-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1-2.svg
     • 1-autoplot/spcvenv-fold-1-sample-fold-n.svg
     • 1-autoplot/spcvenv-fold-1.svg
     • 1-autoplot/spcvenv-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvdisc-fold-1-rep-2.svg
     • 2-autoplot/repspcvdisc-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvdisc-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-1-sample-n-fold.svg
     • 2-autoplot/repspcvknndm-fold-1-rep-2.svg
     • 2-autoplot/repspcvknndm-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvknndm-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-2-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1-2.svg
     • 2-autoplot/repspcvtiles-fold-1-rep-2.svg
     • 2-autoplot/repspcvtiles-fold-1-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-fold-1.svg
     • 2-autoplot/repspcvtiles-sample-fold-n.svg
     • 2-autoplot/repspcvtiles-show-omitted.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-2d-space-var-fold-1-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-1.svg
     • 2-autoplot/repsptcvcstf-fold-1-2-rep-2.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-1-sample-fold-n.svg
     • 2-autoplot/repsptcvcstf-fold-1-rep-2.svg
     • 2-autoplot/spcvdisc-fold-1-2.svg
     • 2-autoplot/spcvdisc-fold-1.svg
     • 2-autoplot/spcvdisc-show-omitted.svg
     • 2-autoplot/spcvknndm-all-test-sets.svg
     • 2-autoplot/spcvknndm-fold-1-2.svg
     • 2-autoplot/spcvknndm-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-space-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-space-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-all-test-sets.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-rep-2.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-2d-time-var-fold-1.svg
     • 2-autoplot/sptcvcstf-2d-time-var-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2-sample-fold-n.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-2.svg
     • 2-autoplot/sptcvcstf-3d-time-var-fold-1-sample-fold-n.svg
     • autoplot_buffer/spcvbuffer-fold-1-2.svg
     Error: Test failures
     Execution halted

Check results at: https://cran.r-project.org/web/checks/check_results_mlr3spatiotempcv.html
