test_that("group by space works", {
  # each space unit contains one observation per day
  data = rowwise_table(
    ~polygon, ~day, ~class, 
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B"
  )
  data[, c("x", "y") := list(runif(nrow(data)),  runif(nrow(data)))]
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif_st(data, id = "test", target = "class", coordinate_names = c("x", "y"))
  task$set_col_roles("polygon", roles = "space")

  resampling = rsmp("sptcv_cstf", stratify = FALSE, folds = 3)
  instance = resampling$instantiate(task)$instance
  walk(seq_len(3), function(i) {
    expect_integer(instance$train[[i]], len = 6)
    expect_integer(instance$test[[i]], len = 3)
    expect_disjunct(instance$train[[i]], instance$test[[i]])
    expect_factor(unique(data[instance$train[[i]], polygon]), len = 2)
    expect_factor(unique(data[instance$test[[i]], polygon]), len = 1)
  })
})

test_that("group by time works", {
  # each space unit contains one observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B"
  )
  data[, c("x", "y") := list(runif(nrow(data)),  runif(nrow(data)))]
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif_st(data, id = "test", target = "class", coordinate_names = c("x", "y"))
  task$set_col_roles("day", roles = "time")

  resampling = rsmp("sptcv_cstf", stratify = FALSE, folds = 3)
  instance = resampling$instantiate(task)$instance
  walk(seq_len(3), function(i) {
    expect_integer(instance$train[[i]], len = 6)
    expect_integer(instance$test[[i]], len = 3)
    expect_disjunct(instance$train[[i]], instance$test[[i]])
    expect_double(unique(data[instance$train[[i]], day]), len = 2)
    expect_double(unique(data[instance$test[[i]], day]), len = 1)
  })
})

test_that("group by space and time works", {
  # each space unit contains one observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B"
  )
  data[, c("x", "y") := list(runif(nrow(data)),  runif(nrow(data)))]
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif_st(data, id = "test", target = "class", coordinate_names = c("x", "y"))
  task$set_col_roles("polygon", roles = "space")
  task$set_col_roles("day", roles = "time")

  resampling = rsmp("sptcv_cstf", stratify = FALSE, folds = 3)
  instance = resampling$instantiate(task)$instance
  walk(seq_len(3), function(i) {
    expect_integer(instance$train[[i]], len = 4)
    expect_integer(instance$test[[i]], len = 1)
    expect_disjunct(instance$train[[i]], instance$test[[i]])
    expect_factor(unique(data[instance$train[[i]], polygon]), len = 2)
    expect_factor(unique(data[instance$test[[i]], polygon]), len = 1)
    expect_double(unique(data[instance$train[[i]], day]), len = 2)
    expect_double(unique(data[instance$test[[i]], day]), len = 1)
  })
})

test_that("stratify on target and group by space works", {
  # each space unit contains one observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B",
    "L4",     1,    "C",
    "L4",     2,    "C",
    "L4",     3,    "C",
    "L5",     1,    "A",
    "L5",     2,    "A",
    "L5",     3,    "A",
    "L6",     1,    "B",
    "L6",     2,    "B",
    "L6",     3,    "B",
    "L7",     1,    "B",
    "L7",     2,    "B",
    "L7",     3,    "B",
    "L8",     1,    "C",
    "L8",     2,    "C",
    "L8",     3,    "C",
    "L9",     1,    "C",
    "L9",     2,    "C",
    "L9",     3,    "C"
  )
  data[, c("x", "y") := list(runif(nrow(data)),  runif(nrow(data)))]
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif_st(data, id = "test", target = "class", coordinate_names = c("x", "y"))
  task$set_col_roles("polygon", roles = "space")

  resampling = rsmp("sptcv_cstf", stratify = TRUE, folds = 3)
  instance = resampling$instantiate(task)$instance
  walk(seq_len(3), function(i) {
    expect_integer(instance$train[[i]], len = 18)
    expect_integer(instance$test[[i]], len = 9)
    expect_disjunct(instance$train[[i]], instance$test[[i]])
    expect_factor(unique(data[instance$train[[i]], polygon]), len = 6)
    expect_factor(unique(data[instance$test[[i]], polygon]), len = 3)
    expect_factor(data[instance$train[[i]], class], levels = c("A", "B", "C"), empty.levels.ok = FALSE)
    expect_factor(data[instance$test[[i]], class], levels = c("A", "B", "C"), empty.levels.ok = FALSE)
  })
})

test_that("group by space works with multiple observations within space unit", {
  # each space unit contains three observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B"
  )
  data[, c("x", "y") := list(runif(nrow(data)),  runif(nrow(data)))]
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif_st(data, id = "test", target = "class", coordinate_names = c("x", "y"))
  task$set_col_roles("polygon", roles = "space")

  resampling = rsmp("sptcv_cstf", stratify = FALSE, folds = 3)
  instance = resampling$instantiate(task)$instance
  walk(seq_len(3), function(i) {
    expect_integer(instance$train[[i]], len = 18)
    expect_integer(instance$test[[i]], len = 9)
    expect_disjunct(instance$train[[i]], instance$test[[i]])
    expect_factor(unique(data[instance$train[[i]], polygon]), len = 2)
    expect_factor(unique(data[instance$test[[i]], polygon]), len = 1)
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
  data[, c("x", "y") := list(runif(nrow(data)),  runif(nrow(data)))]
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif_st(data, id = "test", target = "class", coordinate_names = c("x", "y"))
  task$set_col_roles("day", roles = "time")

  resampling = rsmp("sptcv_cstf", stratify = FALSE, folds = 3)
  instance = resampling$instantiate(task)$instance
  walk(seq_len(3), function(i) {
    expect_integer(instance$train[[i]], len = 18)
    expect_integer(instance$test[[i]], len = 9)
    expect_disjunct(instance$train[[i]], instance$test[[i]])
    expect_double(unique(data[instance$train[[i]], day]), len = 2)
    expect_double(unique(data[instance$test[[i]], day]), len = 1)
  })
})

test_that("group by space and time works with multiple observations within space unit", {
  # each space unit contains three observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B"
  )
  data[, c("x", "y") := list(runif(nrow(data)),  runif(nrow(data)))]
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif_st(data, id = "test", target = "class", coordinate_names = c("x", "y"))
  task$set_col_roles("polygon", roles = "space")
  task$set_col_roles("day", roles = "time")

  resampling = rsmp("sptcv_cstf", stratify = FALSE, folds = 3)
  instance = resampling$instantiate(task)$instance
  walk(seq_len(3), function(i) {
    expect_integer(instance$train[[i]], len = 12)
    expect_integer(instance$test[[i]], len = 3)
    expect_disjunct(instance$train[[i]], instance$test[[i]])
    expect_factor(unique(data[instance$train[[i]], polygon]), len = 2)
    expect_factor(unique(data[instance$test[[i]], polygon]), len = 1)
    expect_double(unique(data[instance$train[[i]], day]), len = 2)
    expect_double(unique(data[instance$test[[i]], day]), len = 1)
  })
})

test_that("stratify on target and group by space works with multiple observations within space unit", {
  # each space unit contains three observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B",
    "L4",     1,    "C",
    "L4",     2,    "C",
    "L4",     3,    "C",
    "L4",     1,    "C",
    "L4",     2,    "C",
    "L4",     3,    "C",
    "L4",     1,    "C",
    "L4",     2,    "C",
    "L4",     3,    "C",
    "L5",     1,    "A",
    "L5",     2,    "A",
    "L5",     3,    "A",
    "L5",     1,    "A",
    "L5",     2,    "A",
    "L5",     3,    "A",
    "L5",     1,    "A",
    "L5",     2,    "A",
    "L5",     3,    "A",
    "L6",     1,    "B",
    "L6",     2,    "B",
    "L6",     3,    "B",
    "L6",     1,    "B",
    "L6",     2,    "B",
    "L6",     3,    "B",
    "L6",     1,    "B",
    "L6",     2,    "B",
    "L6",     3,    "B",
    "L7",     1,    "B",
    "L7",     2,    "B",
    "L7",     3,    "B",
    "L7",     1,    "B",
    "L7",     2,    "B",
    "L7",     3,    "B",
    "L7",     1,    "B",
    "L7",     2,    "B",
    "L7",     3,    "B",
    "L8",     1,    "C",
    "L8",     2,    "C",
    "L8",     3,    "C",
    "L8",     1,    "C",
    "L8",     2,    "C",
    "L8",     3,    "C",
    "L8",     1,    "C",
    "L8",     2,    "C",
    "L8",     3,    "C",
    "L9",     1,    "C",
    "L9",     2,    "C",
    "L9",     3,    "C",
    "L9",     1,    "C",
    "L9",     2,    "C",
    "L9",     3,    "C",
    "L9",     1,    "C",
    "L9",     2,    "C",
    "L9",     3,    "C"
  )
  data[, c("x", "y") := list(runif(nrow(data)),  runif(nrow(data)))]
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif_st(data, id = "test", target = "class", coordinate_names = c("x", "y"))
  task$set_col_roles("polygon", roles = "space")

  resampling = rsmp("sptcv_cstf", stratify = TRUE, folds = 3)
  instance = resampling$instantiate(task)$instance
  walk(seq_len(3), function(i) {
    expect_integer(instance$train[[i]], len = 54)
    expect_integer(instance$test[[i]], len = 27)
    expect_disjunct(instance$train[[i]], instance$test[[i]])
    expect_factor(unique(data[instance$train[[i]], polygon]), len = 6)
    expect_factor(unique(data[instance$test[[i]], polygon]), len = 3)
    expect_factor(data[instance$train[[i]], class], levels = c("A", "B", "C"), empty.levels.ok = FALSE)
    expect_factor(data[instance$test[[i]], class], levels = c("A", "B", "C"), empty.levels.ok = FALSE)
  })
})

test_that("group by space works with one polygon more than folds", {
  # each space unit contains one observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B",
    "L4",     1,    "C",
    "L4",     2,    "C",
    "L4",     3,    "C"
  )
  data[, c("x", "y") := list(runif(nrow(data)),  runif(nrow(data)))]
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif_st(data, id = "test", target = "class", coordinate_names = c("x", "y"))
  task$set_col_roles("polygon", roles = "space")

  resampling = rsmp("sptcv_cstf", stratify = FALSE, folds = 3)
  instance = resampling$instantiate(task)$instance
  # fold 1
  expect_integer(instance$train[[1]], len = 6)
  expect_integer(instance$test[[1]], len = 6)
  expect_disjunct(instance$train[[1]], instance$test[[1]])
  expect_factor(unique(data[instance$train[[1]], polygon]), len = 2)
  expect_factor(unique(data[instance$test[[1]], polygon]), len = 2)

  # fold 2 and 3
  walk(2:3, function(i) {
    expect_integer(instance$train[[i]], len = 9)
    expect_integer(instance$test[[i]], len = 3)
    expect_disjunct(instance$train[[i]], instance$test[[i]])
    expect_factor(unique(data[instance$train[[i]], polygon]), len = 3)
    expect_factor(unique(data[instance$test[[i]], polygon]), len = 1)
  })
})

test_that("group by space errors with one spatial unit less than folds", {
  # each space unit contains one observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L2",     1,    "B",
    "L2",     2,    "B",
    "L2",     3,    "B"
  )
  data[, c("x", "y") := list(runif(nrow(data)),  runif(nrow(data)))]
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif_st(data, id = "test", target = "class", coordinate_names = c("x", "y"))
  task$set_col_roles("polygon", roles = "space")

  resampling = rsmp("sptcv_cstf", stratify = FALSE, folds = 3)
  expect_error(resampling$instantiate(task), "The number of folds is higher than the number of spatial units.")
})

test_that("group by time errors with one temporal unit less than folds", {
  # each space unit contains one observation per day
  data = mlr3misc::rowwise_table(
    ~polygon, ~day, ~class,
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L3",     1,    "B",
    "L3",     2,    "B"
  )
  data[, c("x", "y") := list(runif(nrow(data)),  runif(nrow(data)))]
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif_st(data, id = "test", target = "class", coordinate_names = c("x", "y"))
  task$set_col_roles("day", roles = "time")

  resampling = rsmp("sptcv_cstf", stratify = FALSE, folds = 3)
  expect_error(resampling$instantiate(task), "The number of folds is higher than the number of temporal units.")
})

test_that("group by space works", {
  # each space unit contains one observation per day
  data = rowwise_table(
    ~polygon, ~day, ~class,
    "L1",     1,    "A",
    "L1",     2,    "A",
    "L1",     3,    "A",
    "L2",     1,    "A",
    "L2",     2,    "A",
    "L2",     3,    "A",
    "L3",     1,    "B",
    "L3",     2,    "B",
    "L3",     3,    "B"
  )
  data[, c("x", "y") := list(runif(nrow(data)),  runif(nrow(data)))]
  cols = which(sapply(data, is.character))
  data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  data[, x1 := runif(.N)]

  task = as_task_classif_st(data, id = "test", target = "class", coordinate_names = c("x", "y"))
  resampling = rsmp("sptcv_cstf", stratify = FALSE, folds = 3)
  expect_error(resampling$instantiate(task), "has no column role")
})
