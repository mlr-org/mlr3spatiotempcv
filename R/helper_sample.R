sample_cstf <- function(self, task, space_var, time_var, class, k, data) {

  # if classification is used, make sure that classes are equally
  # distributed across folds

  if (!is.null(class)) {
    select_cols = c(space_var, class)
    unit = unique(data[, select_cols, with = FALSE])
    # unit needs to be a data.frame here
    unit$cstf_fold = caret::createFolds(
      as.data.frame(unit)[, which(names(unit) == class)],
      k = k, list = FALSE)
    data = merge(data, unit,
      by.x = c(space_var, class),
      by.y = c(space_var, class), all.x = TRUE, sort = FALSE)
    space_var = "cstf_fold"
  }

  if (!is.null(space_var)) {
    if (k > uniqueN(data[[space_var]])) {
      k = uniqueN(data[[space_var]])
      cli::cli_alert_warning("Number of folds is higher than number of
            unique locations.
            Setting folds to the maximum amount of unique levels of variable
            {space_var} which is {k}.", wrap = TRUE)
    }
  }
  if (!is.null(time_var)) {
    if (k > uniqueN(data[[time_var]])) {
      k = uniqueN(data[[time_var]])
      cli::cli_alert_warning("Number of folds is higher than number of
            unique points in time.
            Setting folds to the maximum amount of unique levels of variable
            {time_var} which is {k}.", wrap = TRUE)
    }
  }

  seed = sample(1:1000, 1)

  # split space into k folds
  if (!is.null(space_var)) {
    set.seed(seed)
    spacefolds = lapply(caret::createFolds(
      1:uniqueN(data[[space_var]]),
      k), function(y) {
      unique(data[[space_var]])[y]
    })
  } else {
    spacefolds = NULL
  }
  # split time into k folds
  if (!is.null(time_var)) {
    set.seed(seed)
    timefolds = lapply(caret::createFolds(
      1:uniqueN(data[[time_var]]),
      k), function(y) {
      unique(data[[time_var]])[y]
    })
  } else {
    timefolds = NULL
  }
  return(list(
    spacefolds = spacefolds, timefolds = timefolds,
    space_var = space_var, data = data))
}
