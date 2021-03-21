test_that("AutoTuner works with sptcv methods", {
  library("paradox")
  library("mlr3tuning")
  library("mlr3spatiotempcv")

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")

  logger_mlr3 = lgr::get_logger("mlr3")
  logger_mlr3$set_threshold("warn")

  learner = lrn("classif.rpart")
  tune_ps = ParamSet$new(list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("minsplit", lower = 1, upper = 10)
  ))
  terminator = trm("evals", n_evals = 2)
  tuner = tnr("random_search")

  at = AutoTuner$new(
    learner = learner,
    resampling = rsmp("spcv_coords", folds = 3),
    measure = msr("classif.ce"),
    search_space = tune_ps,
    terminator = terminator,
    tuner = tuner
  )
  grid = benchmark_grid(
    task = tsk("ecuador"),
    learner = list(at),
    resampling = rsmp("spcv_coords", folds = 3)
  )

  bmr = benchmark(grid)

  expect_equal(bmr$resamplings$resampling_id, "spcv_coords")
})
