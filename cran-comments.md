mlr3spatiotempcv 2.3.2

The DOI in the CITATION is for a new JSS publication that will be registered after publication on CRAN.

## Cran Repository Policy

- [x] Reviewed CRP last edited 2024-08-27.

See changes at https://github.com/eddelbuettel/crp/compare/master@%7B2024-04-04%7D...master@%7B2024-08-27%7D

## R CMD check results

- [x] Checked locally, R 4.4.2
- [x] Checked on CI system, R 4.4.2
- [x] Checked on win-builder, R devel

Check the boxes above after successful execution and remove this line. Then run `fledge::release()`.

## Current CRAN check results

- [x] Checked on 2024-11-27, problems found: https://cran.r-project.org/web/checks/check_results_mlr3spatiotempcv.html
- [x] NOTE: r-devel-linux-x86_64-debian-clang, r-devel-linux-x86_64-debian-gcc, r-devel-windows-x86_64
     Found the following Rd file(s) with Rd \link{} targets missing package
     anchors:
     TaskClassifST.Rd: Task, TaskSupervised, DataBackend,
     DataBackendDataTable, convert_task
     TaskRegrST.Rd: Task, TaskSupervised, DataBackend,
     DataBackendDataTable, convert_task
     as_task_classif_st.Rd: DataBackend, TaskRegr, convert_task
     as_task_regr_st.Rd: DataBackend, TaskClassif, convert_task
     autoplot.ResamplingCV.Rd: ResamplingCV, ResamplingRepeatedCV
     autoplot.ResamplingCustomCV.Rd: ResamplingCustomCV
     autoplot.ResamplingSpCVBlock.Rd: ggplot
     mlr_resamplings_repeated_spcv_block.Rd: Task
     mlr_resamplings_repeated_spcv_coords.Rd: Task
     mlr_resamplings_repeated_spcv_disc.Rd: Task
     mlr_resamplings_repeated_spcv_env.Rd: Task
     mlr_resamplings_repeated_spcv_knndm.Rd: Task
     mlr_resamplings_repeated_spcv_tiles.Rd: Task
     mlr_resamplings_repeated_sptcv_cstf.Rd: Task
     mlr_resamplings_spcv_block.Rd: Task
     mlr_resamplings_spcv_buffer.Rd: Task
     mlr_resamplings_spcv_coords.Rd: Task
     mlr_resamplings_spcv_disc.Rd: Task
     mlr_resamplings_spcv_env.Rd: Task
     mlr_resamplings_spcv_knndm.Rd: Task
     mlr_resamplings_spcv_tiles.Rd: Task
     mlr_resamplings_sptcv_cstf.Rd: Task
     mlr_tasks_cookfarm_mlr3.Rd: TaskRegr, Task, mlr_tasks
     mlr_tasks_diplodia.Rd: TaskClassif, Task, mlr_tasks
     mlr_tasks_ecuador.Rd: TaskClassif, Task, mlr_tasks
     Please provide package anchors for all Rd \link{} targets not in the
     package itself and the base packages.
- [x] ERROR: r-devel-linux-x86_64-debian-clang
     Running ‘testthat.R’ [103s/54s]
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
     [ FAIL 1 | WARN 1 | SKIP 24 | PASS 1206 ]
     
     ══ Skipped tests (24) ══════════════════════════════════════════════════════════
     • On CRAN (19): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-2-autoplot.R:352:3',
     'test-autoplot_buffer.R:19:3'
     • On Linux (2): 'test-2-autoplot.R:8:3', 'test-2-autoplot.R:54:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-TaskClassifST.R:16:3'): Supplying a non-spatio temporal task gives descriptive error message ──
     Error: Element with key 'boston_housing' not found in DictionaryTask!
     Backtrace:
     ▆
     1. ├─testthat::expect_error(...) at test-TaskClassifST.R:16:3
     2. │ └─testthat:::expect_condition_matching(...)
     3. │   └─testthat:::quasi_capture(...)
     4. │     ├─testthat (local) .capture(...)
     5. │     │ └─base::withCallingHandlers(...)
     6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
     7. ├─rsmp("spcv_coords")$instantiate(tsk("boston_housing"))
     8. │ └─mlr3spatiotempcv:::.__ResamplingSpCVCoords__instantiate(...)
     9. │   └─mlr3::assert_task(task)
     10. │     └─checkmate::assert_class(task, "Task", .var.name = .var.name)
     11. │       └─checkmate::checkClass(x, classes, ordered, null.ok)
     12. └─mlr3::tsk("boston_housing")
     13.   └─mlr3misc::dictionary_sugar_get(dict = mlr_tasks, .key, ...)
     14.     └─mlr3misc:::dictionary_get(dict, .key)
     15.       └─mlr3misc:::dictionary_retrieve_item(self, key)
     16.         └─mlr3misc::stopf(...)
     
     [ FAIL 1 | WARN 1 | SKIP 24 | PASS 1206 ]
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
- [x] ERROR: r-devel-linux-x86_64-debian-gcc
     Running ‘testthat.R’ [87s/59s]
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
     [ FAIL 1 | WARN 1 | SKIP 24 | PASS 1206 ]
     
     ══ Skipped tests (24) ══════════════════════════════════════════════════════════
     • On CRAN (19): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-2-autoplot.R:352:3',
     'test-autoplot_buffer.R:19:3'
     • On Linux (2): 'test-2-autoplot.R:8:3', 'test-2-autoplot.R:54:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-TaskClassifST.R:16:3'): Supplying a non-spatio temporal task gives descriptive error message ──
     Error: Element with key 'boston_housing' not found in DictionaryTask!
     Backtrace:
     ▆
     1. ├─testthat::expect_error(...) at test-TaskClassifST.R:16:3
     2. │ └─testthat:::expect_condition_matching(...)
     3. │   └─testthat:::quasi_capture(...)
     4. │     ├─testthat (local) .capture(...)
     5. │     │ └─base::withCallingHandlers(...)
     6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
     7. ├─rsmp("spcv_coords")$instantiate(tsk("boston_housing"))
     8. │ └─mlr3spatiotempcv:::.__ResamplingSpCVCoords__instantiate(...)
     9. │   └─mlr3::assert_task(task)
     10. │     └─checkmate::assert_class(task, "Task", .var.name = .var.name)
     11. │       └─checkmate::checkClass(x, classes, ordered, null.ok)
     12. └─mlr3::tsk("boston_housing")
     13.   └─mlr3misc::dictionary_sugar_get(dict = mlr_tasks, .key, ...)
     14.     └─mlr3misc:::dictionary_get(dict, .key)
     15.       └─mlr3misc:::dictionary_retrieve_item(self, key)
     16.         └─mlr3misc::stopf(...)
     
     [ FAIL 1 | WARN 1 | SKIP 24 | PASS 1206 ]
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
- [x] ERROR: r-devel-linux-x86_64-fedora-clang
     Running ‘testthat.R’ [183s/92s]
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
     [ FAIL 1 | WARN 1 | SKIP 24 | PASS 1206 ]
     
     ══ Skipped tests (24) ══════════════════════════════════════════════════════════
     • On CRAN (19): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-2-autoplot.R:352:3',
     'test-autoplot_buffer.R:19:3'
     • On Linux (2): 'test-2-autoplot.R:8:3', 'test-2-autoplot.R:54:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-TaskClassifST.R:16:3'): Supplying a non-spatio temporal task gives descriptive error message ──
     Error: Element with key 'boston_housing' not found in DictionaryTask!
     Backtrace:
     ▆
     1. ├─testthat::expect_error(...) at test-TaskClassifST.R:16:3
     2. │ └─testthat:::expect_condition_matching(...)
     3. │   └─testthat:::quasi_capture(...)
     4. │     ├─testthat (local) .capture(...)
     5. │     │ └─base::withCallingHandlers(...)
     6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
     7. ├─rsmp("spcv_coords")$instantiate(tsk("boston_housing"))
     8. │ └─mlr3spatiotempcv:::.__ResamplingSpCVCoords__instantiate(...)
     9. │   └─mlr3::assert_task(task)
     10. │     └─checkmate::assert_class(task, "Task", .var.name = .var.name)
     11. │       └─checkmate::checkClass(x, classes, ordered, null.ok)
     12. └─mlr3::tsk("boston_housing")
     13.   └─mlr3misc::dictionary_sugar_get(dict = mlr_tasks, .key, ...)
     14.     └─mlr3misc:::dictionary_get(dict, .key)
     15.       └─mlr3misc:::dictionary_retrieve_item(self, key)
     16.         └─mlr3misc::stopf(...)
     
     [ FAIL 1 | WARN 1 | SKIP 24 | PASS 1206 ]
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
- [x] ERROR: r-devel-linux-x86_64-fedora-gcc
     Running ‘testthat.R’ [189s/106s]
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
     [ FAIL 1 | WARN 1 | SKIP 23 | PASS 1211 ]
     
     ══ Skipped tests (23) ══════════════════════════════════════════════════════════
     • On CRAN (20): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-2-autoplot.R:352:3',
     'test-autoplot_buffer.R:19:3', 'test-mlr3pipelines-graph-integration.R:5:3'
     • On Linux (2): 'test-2-autoplot.R:8:3', 'test-2-autoplot.R:54:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-TaskClassifST.R:16:3'): Supplying a non-spatio temporal task gives descriptive error message ──
     Error: Element with key 'boston_housing' not found in DictionaryTask!
     Backtrace:
     ▆
     1. ├─testthat::expect_error(...) at test-TaskClassifST.R:16:3
     2. │ └─testthat:::expect_condition_matching(...)
     3. │   └─testthat:::quasi_capture(...)
     4. │     ├─testthat (local) .capture(...)
     5. │     │ └─base::withCallingHandlers(...)
     6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
     7. ├─rsmp("spcv_coords")$instantiate(tsk("boston_housing"))
     8. │ └─mlr3spatiotempcv:::.__ResamplingSpCVCoords__instantiate(...)
     9. │   └─mlr3::assert_task(task)
     10. │     └─checkmate::assert_class(task, "Task", .var.name = .var.name)
     11. │       └─checkmate::checkClass(x, classes, ordered, null.ok)
     12. └─mlr3::tsk("boston_housing")
     13.   └─mlr3misc::dictionary_sugar_get(dict = mlr_tasks, .key, ...)
     14.     └─mlr3misc:::dictionary_get(dict, .key)
     15.       └─mlr3misc:::dictionary_retrieve_item(self, key)
     16.         └─mlr3misc::stopf(...)
     
     [ FAIL 1 | WARN 1 | SKIP 23 | PASS 1211 ]
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
- [x] ERROR: r-devel-windows-x86_64
     Running 'testthat.R' [51s]
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
     [ FAIL 1 | WARN 1 | SKIP 24 | PASS 1206 ]
     
     ══ Skipped tests (24) ══════════════════════════════════════════════════════════
     • On CRAN (19): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-2-autoplot.R:352:3',
     'test-autoplot_buffer.R:19:3'
     • On Windows (2): 'test-2-autoplot.R:9:3', 'test-2-autoplot.R:55:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-TaskClassifST.R:16:3'): Supplying a non-spatio temporal task gives descriptive error message ──
     Error: Element with key 'boston_housing' not found in DictionaryTask!
     Backtrace:
     ▆
     1. ├─testthat::expect_error(...) at test-TaskClassifST.R:16:3
     2. │ └─testthat:::expect_condition_matching(...)
     3. │   └─testthat:::quasi_capture(...)
     4. │     ├─testthat (local) .capture(...)
     5. │     │ └─base::withCallingHandlers(...)
     6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
     7. ├─rsmp("spcv_coords")$instantiate(tsk("boston_housing"))
     8. │ └─mlr3spatiotempcv:::.__ResamplingSpCVCoords__instantiate(...)
     9. │   └─mlr3::assert_task(task)
     10. │     └─checkmate::assert_class(task, "Task", .var.name = .var.name)
     11. │       └─checkmate::checkClass(x, classes, ordered, null.ok)
     12. └─mlr3::tsk("boston_housing")
     13.   └─mlr3misc::dictionary_sugar_get(dict = mlr_tasks, .key, ...)
     14.     └─mlr3misc:::dictionary_get(dict, .key)
     15.       └─mlr3misc:::dictionary_retrieve_item(self, key)
     16.         └─mlr3misc::stopf(...)
     
     [ FAIL 1 | WARN 1 | SKIP 24 | PASS 1206 ]
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
- [x] ERROR: r-patched-linux-x86_64
     Running ‘testthat.R’ [102s/55s]
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
     [ FAIL 1 | WARN 1 | SKIP 24 | PASS 1206 ]
     
     ══ Skipped tests (24) ══════════════════════════════════════════════════════════
     • On CRAN (19): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-2-autoplot.R:352:3',
     'test-autoplot_buffer.R:19:3'
     • On Linux (2): 'test-2-autoplot.R:8:3', 'test-2-autoplot.R:54:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-TaskClassifST.R:16:3'): Supplying a non-spatio temporal task gives descriptive error message ──
     Error: Element with key 'boston_housing' not found in DictionaryTask!
     Backtrace:
     ▆
     1. ├─testthat::expect_error(...) at test-TaskClassifST.R:16:3
     2. │ └─testthat:::expect_condition_matching(...)
     3. │   └─testthat:::quasi_capture(...)
     4. │     ├─testthat (local) .capture(...)
     5. │     │ └─base::withCallingHandlers(...)
     6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
     7. ├─rsmp("spcv_coords")$instantiate(tsk("boston_housing"))
     8. │ └─mlr3spatiotempcv:::.__ResamplingSpCVCoords__instantiate(...)
     9. │   └─mlr3::assert_task(task)
     10. │     └─checkmate::assert_class(task, "Task", .var.name = .var.name)
     11. │       └─checkmate::checkClass(x, classes, ordered, null.ok)
     12. └─mlr3::tsk("boston_housing")
     13.   └─mlr3misc::dictionary_sugar_get(dict = mlr_tasks, .key, ...)
     14.     └─mlr3misc:::dictionary_get(dict, .key)
     15.       └─mlr3misc:::dictionary_retrieve_item(self, key)
     16.         └─mlr3misc::stopf(...)
     
     [ FAIL 1 | WARN 1 | SKIP 24 | PASS 1206 ]
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
- [x] ERROR: r-oldrel-windows-x86_64
     Running 'testthat.R' [76s]
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
     [ FAIL 1 | WARN 1 | SKIP 24 | PASS 1206 ]
     
     ══ Skipped tests (24) ══════════════════════════════════════════════════════════
     • On CRAN (19): 'test-1-autoplot.R:40:3', 'test-1-autoplot.R:72:3',
     'test-1-autoplot.R:98:3', 'test-1-autoplot.R:130:3',
     'test-1-autoplot.R:157:3', 'test-1-autoplot.R:189:3',
     'test-1-autoplot.R:224:3', 'test-1-autoplot.R:257:3',
     'test-1-autoplot.R:268:3', 'test-1-autoplot.R:315:3',
     'test-1-autoplot.R:343:3', 'test-1-autoplot.R:377:3',
     'test-2-autoplot.R:129:3', 'test-2-autoplot.R:182:3',
     'test-2-autoplot.R:204:3', 'test-2-autoplot.R:246:3',
     'test-2-autoplot.R:300:3', 'test-2-autoplot.R:352:3',
     'test-autoplot_buffer.R:19:3'
     • On Windows (2): 'test-2-autoplot.R:9:3', 'test-2-autoplot.R:55:3'
     • empty test (1): 'test-helper-DataBackend.R:1:1'
     • raster cannot be loaded (1): 'test-mlr3pipelines-graph-integration.R:4:3'
     • skmeans cannot be loaded (1): 'test-mlr_sptcv_generic.R:70:3'
     
     ══ Failed tests ════════════════════════════════════════════════════════════════
     ── Error ('test-TaskClassifST.R:16:3'): Supplying a non-spatio temporal task gives descriptive error message ──
     Error: Element with key 'boston_housing' not found in DictionaryTask!
     Backtrace:
     ▆
     1. ├─testthat::expect_error(...) at test-TaskClassifST.R:16:3
     2. │ └─testthat:::expect_condition_matching(...)
     3. │   └─testthat:::quasi_capture(...)
     4. │     ├─testthat (local) .capture(...)
     5. │     │ └─base::withCallingHandlers(...)
     6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
     7. ├─rsmp("spcv_coords")$instantiate(tsk("boston_housing"))
     8. │ └─mlr3spatiotempcv:::.__ResamplingSpCVCoords__instantiate(...)
     9. │   └─mlr3::assert_task(task)
     10. │     └─checkmate::assert_class(task, "Task", .var.name = .var.name)
     11. │       └─checkmate::checkClass(x, classes, ordered, null.ok)
     12. └─mlr3::tsk("boston_housing")
     13.   └─mlr3misc::dictionary_sugar_get(dict = mlr_tasks, .key, ...)
     14.     └─mlr3misc:::dictionary_get(dict, .key)
     15.       └─mlr3misc:::dictionary_retrieve_item(self, key)
     16.         └─mlr3misc::stopf(...)
     
     [ FAIL 1 | WARN 1 | SKIP 24 | PASS 1206 ]
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
