test_that("group by space works", {
  # each space unit contains one observation per day
  data = rowwise_table(
    ~polygon, ~day, ~class,
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B"
  )
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif(data, id = "test", target = "class")
  task$set_col_roles("polygon", roles = "space")

  resampling = rsmp("repeated_sptcv_cstf", stratify = FALSE, folds = 3, repeats = 5)
  instance = resampling$instantiate(task)$instance
  pwalk(list(rep(seq_len(5), each = 3), rep(seq_len(3), 5)), function(rep, fold) {
    expect_integer(instance[[rep]]$train[[fold]], len = 6)
    expect_integer(instance[[rep]]$test[[fold]], len = 3)
    expect_disjunct(instance[[rep]]$train[[fold]], instance[[rep]]$test[[fold]])
    expect_factor(unique(data[instance[[rep]]$train[[fold]], polygon]), len = 2)
    expect_factor(unique(data[instance[[rep]]$test[[fold]], polygon]), len = 1)
  })
})

test_that("group by time works", {
  # each space unit contains one observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B"
  )
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif(data, id = "test", target = "class")
  task$set_col_roles("day", roles = "time")

  resampling = rsmp("repeated_sptcv_cstf", stratify = FALSE, folds = 3, repeats = 5)
  instance = resampling$instantiate(task)$instance
  pwalk(list(rep(seq_len(5), each = 3), rep(seq_len(3), 5)), function(rep, fold) {
    expect_integer(instance[[rep]]$train[[fold]], len = 6)
    expect_integer(instance[[rep]]$test[[fold]], len = 3)
    expect_disjunct(instance[[rep]]$train[[fold]], instance[[rep]]$test[[fold]])
    expect_double(unique(data[instance[[rep]]$train[[fold]], day]), len = 2)
    expect_double(unique(data[instance[[rep]]$test[[fold]], day]), len = 1)
  })
})

test_that("group by space and time works", {
  # each space unit contains one observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B"
  )
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif(data, id = "test", target = "class")
  task$set_col_roles("polygon", roles = "space")
  task$set_col_roles("day", roles = "time")

  resampling = rsmp("repeated_sptcv_cstf", stratify = FALSE, folds = 3, repeats = 5)
  instance = resampling$instantiate(task)$instance
  pwalk(list(rep(seq_len(5), each = 3), rep(seq_len(3), 5)), function(rep, fold) {
    expect_integer(instance[[rep]]$train[[fold]], len = 4)
    expect_integer(instance[[rep]]$test[[fold]], len = 1)
    expect_disjunct(instance[[rep]]$train[[fold]], instance[[rep]]$test[[fold]])
    expect_factor(unique(data[instance[[rep]]$train[[fold]], polygon]), len = 2)
    expect_factor(unique(data[instance[[rep]]$test[[fold]], polygon]), len = 1)
    expect_double(unique(data[instance[[rep]]$train[[fold]], day]), len = 2)
    expect_double(unique(data[instance[[rep]]$test[[fold]], day]), len = 1)
  })
})

test_that("stratify on target and group by space works", {
  # each space unit contains one observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B",
    "L4", 1, "C",
    "L4", 2, "C",
    "L4", 3, "C",
    "L5", 1, "A",
    "L5", 2, "A",
    "L5", 3, "A",
    "L6", 1, "B",
    "L6", 2, "B",
    "L6", 3, "B",
    "L7", 1, "B",
    "L7", 2, "B",
    "L7", 3, "B",
    "L8", 1, "C",
    "L8", 2, "C",
    "L8", 3, "C",
    "L9", 1, "C",
    "L9", 2, "C",
    "L9", 3, "C"
  )
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif(data, id = "test", target = "class")
  task$set_col_roles("polygon", roles = "space")

  resampling = rsmp("repeated_sptcv_cstf", stratify = TRUE, folds = 3, repeats = 5)
  instance = resampling$instantiate(task)$instance
  pwalk(list(rep(seq_len(5), each = 3), rep(seq_len(3), 5)), function(rep, fold) {
    expect_integer(instance[[rep]]$train[[fold]], len = 18)
    expect_integer(instance[[rep]]$test[[fold]], len = 9)
    expect_disjunct(instance[[rep]]$train[[fold]], instance[[rep]]$test[[fold]])
    expect_factor(unique(data[instance[[rep]]$train[[fold]], polygon]), len = 6)
    expect_factor(unique(data[instance[[rep]]$test[[fold]], polygon]), len = 3)
    expect_factor(data[instance[[rep]]$train[[fold]], class], levels = c("A", "B", "C"), empty.levels.ok = FALSE)
    expect_factor(data[instance[[rep]]$test[[fold]], class], levels = c("A", "B", "C"), empty.levels.ok = FALSE)
  })
})

test_that("group by space works with multiple observations within space unit", {
  # each space unit contains three observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B"
  )
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif(data, id = "test", target = "class")
  task$set_col_roles("polygon", roles = "space")

  resampling = rsmp("repeated_sptcv_cstf", stratify = FALSE, folds = 3, repeats = 5)
  instance = resampling$instantiate(task)$instance
  pwalk(list(rep(seq_len(5), each = 3), rep(seq_len(3), 5)), function(rep, fold) {
    expect_integer(instance[[rep]]$train[[fold]], len = 18)
    expect_integer(instance[[rep]]$test[[fold]], len = 9)
    expect_disjunct(instance[[rep]]$train[[fold]], instance[[rep]]$test[[fold]])
    expect_factor(unique(data[instance[[rep]]$train[[fold]], polygon]), len = 2)
    expect_factor(unique(data[instance[[rep]]$test[[fold]], polygon]), len = 1)
  })
})

test_that("group by time works with multiple observations within space unit", {
  # each space unit contains three observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B"
  )
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif(data, id = "test", target = "class")
  task$set_col_roles("day", roles = "time")

  resampling = rsmp("repeated_sptcv_cstf", stratify = FALSE, folds = 3, repeats = 5)
  instance = resampling$instantiate(task)$instance
  pwalk(list(rep(seq_len(5), each = 3), rep(seq_len(3), 5)), function(rep, fold) {
    expect_integer(instance[[rep]]$train[[fold]], len = 18)
    expect_integer(instance[[rep]]$test[[fold]], len = 9)
    expect_disjunct(instance[[rep]]$train[[fold]], instance[[rep]]$test[[fold]])
    expect_double(unique(data[instance[[rep]]$train[[fold]], day]), len = 2)
    expect_double(unique(data[instance[[rep]]$test[[fold]], day]), len = 1)
  })
})

test_that("group by space and time works with multiple observations within space unit", {
  # each space unit contains three observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B"
  )
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif(data, id = "test", target = "class")
  task$set_col_roles("polygon", roles = "space")
  task$set_col_roles("day", roles = "time")

  resampling = rsmp("repeated_sptcv_cstf", stratify = FALSE, folds = 3, repeats = 5)
  instance = resampling$instantiate(task)$instance
  pwalk(list(rep(seq_len(5), each = 3), rep(seq_len(3), 5)), function(rep, fold) {
    expect_integer(instance[[rep]]$train[[fold]], len = 12)
    expect_integer(instance[[rep]]$test[[fold]], len = 3)
    expect_disjunct(instance[[rep]]$train[[fold]], instance[[rep]]$test[[fold]])
    expect_factor(unique(data[instance[[rep]]$train[[fold]], polygon]), len = 2)
    expect_factor(unique(data[instance[[rep]]$test[[fold]], polygon]), len = 1)
    expect_double(unique(data[instance[[rep]]$train[[fold]], day]), len = 2)
    expect_double(unique(data[instance[[rep]]$test[[fold]], day]), len = 1)
  })
})

test_that("stratify on target and group by space works with multiple observations within space unit", {
  # each space unit contains three observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B",
    "L4", 1, "C",
    "L4", 2, "C",
    "L4", 3, "C",
    "L4", 1, "C",
    "L4", 2, "C",
    "L4", 3, "C",
    "L4", 1, "C",
    "L4", 2, "C",
    "L4", 3, "C",
    "L5", 1, "A",
    "L5", 2, "A",
    "L5", 3, "A",
    "L5", 1, "A",
    "L5", 2, "A",
    "L5", 3, "A",
    "L5", 1, "A",
    "L5", 2, "A",
    "L5", 3, "A",
    "L6", 1, "B",
    "L6", 2, "B",
    "L6", 3, "B",
    "L6", 1, "B",
    "L6", 2, "B",
    "L6", 3, "B",
    "L6", 1, "B",
    "L6", 2, "B",
    "L6", 3, "B",
    "L7", 1, "B",
    "L7", 2, "B",
    "L7", 3, "B",
    "L7", 1, "B",
    "L7", 2, "B",
    "L7", 3, "B",
    "L7", 1, "B",
    "L7", 2, "B",
    "L7", 3, "B",
    "L8", 1, "C",
    "L8", 2, "C",
    "L8", 3, "C",
    "L8", 1, "C",
    "L8", 2, "C",
    "L8", 3, "C",
    "L8", 1, "C",
    "L8", 2, "C",
    "L8", 3, "C",
    "L9", 1, "C",
    "L9", 2, "C",
    "L9", 3, "C",
    "L9", 1, "C",
    "L9", 2, "C",
    "L9", 3, "C",
    "L9", 1, "C",
    "L9", 2, "C",
    "L9", 3, "C"
  )
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif(data, id = "test", target = "class")
  task$set_col_roles("polygon", roles = "space")

  resampling = rsmp("repeated_sptcv_cstf", stratify = TRUE, folds = 3, repeats = 5)
  instance = resampling$instantiate(task)$instance
  pwalk(list(rep(seq_len(5), each = 3), rep(seq_len(3), 5)), function(rep, fold) {
    expect_integer(instance[[rep]]$train[[fold]], len = 54)
    expect_integer(instance[[rep]]$test[[fold]], len = 27)
    expect_disjunct(instance[[rep]]$train[[fold]], instance[[rep]]$test[[fold]])
    expect_factor(unique(data[instance[[rep]]$train[[fold]], polygon]), len = 6)
    expect_factor(unique(data[instance[[rep]]$test[[fold]], polygon]), len = 3)
    expect_factor(data[instance[[rep]]$train[[fold]], class], levels = c("A", "B", "C"), empty.levels.ok = FALSE)
    expect_factor(data[instance[[rep]]$test[[fold]], class], levels = c("A", "B", "C"), empty.levels.ok = FALSE)
  })
})

test_that("group by space works with one polygon more than folds", {
  # each space unit contains one observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B",
    "L4", 1, "C",
    "L4", 2, "C",
    "L4", 3, "C"
  )
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif(data, id = "test", target = "class")
  task$set_col_roles("polygon", roles = "space")

  resampling = rsmp("repeated_sptcv_cstf", stratify = FALSE, folds = 3, repeats = 5)
  instance = resampling$instantiate(task)$instance
  # fold
  walk(seq_len(5), function(rep) {
    expect_integer(instance[[rep]]$train[[1]], len = 6)
    expect_integer(instance[[rep]]$test[[1]], len = 6)
    expect_disjunct(instance[[rep]]$train[[1]], instance$test[[1]])
    expect_factor(unique(data[instance[[rep]]$train[[1]], polygon]), len = 2)
    expect_factor(unique(data[instance[[rep]]$test[[1]], polygon]), len = 2)
  })

  # fold 2 and 3
  pwalk(list(rep(seq_len(5), each = 2), rep(2:3, 5)), function(rep, fold) {
    expect_integer(instance[[rep]]$train[[fold]], len = 9)
    expect_integer(instance[[rep]]$test[[fold]], len = 3)
    expect_disjunct(instance[[rep]]$train[[fold]], instance[[rep]]$test[[fold]])
    expect_factor(unique(data[instance[[rep]]$train[[fold]], polygon]), len = 3)
    expect_factor(unique(data[instance[[rep]]$test[[fold]], polygon]), len = 1)
  })
})

test_that("group by space errors with one spatial unit less than folds", {
  # each space unit contains one observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L2", 1, "B",
    "L2", 2, "B",
    "L2", 3, "B"
  )
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif(data, id = "test", target = "class")
  task$set_col_roles("polygon", roles = "space")

  resampling = rsmp("repeated_sptcv_cstf", stratify = FALSE, folds = 3, repeats = 5)
  expect_error(resampling$instantiate(task), "The number of folds is higher than the number of spatial units.")
})

test_that("group by time errors with one temporal unit less than folds", {
  # each space unit contains one observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1", 1, "A",
    "L1", 2, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L3", 1, "B",
    "L3", 2, "B"
  )
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif(data, id = "test", target = "class")
  task$set_col_roles("day", roles = "time")

  resampling = rsmp("repeated_sptcv_cstf", stratify = FALSE, folds = 3, repeats = 5)
  expect_error(resampling$instantiate(task), "The number of folds is higher than the number of temporal units.")
})

test_that("group by space works", {
  # each space unit contains one observation per day
  data = rowwise_table(
    ~polygon, ~day, ~class,
    "L1", 1, "A",
    "L1", 2, "A",
    "L1", 3, "A",
    "L2", 1, "A",
    "L2", 2, "A",
    "L2", 3, "A",
    "L3", 1, "B",
    "L3", 2, "B",
    "L3", 3, "B"
  )
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif(data, id = "test", target = "class")
  resampling = rsmp("repeated_sptcv_cstf", stratify = FALSE, folds = 3, repeats = 5)
  expect_error(resampling$instantiate(task), "has no column role")
})
