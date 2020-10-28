sample_cstf = function(self, task, space_var, time_var, class, k, data) {

  # if classification is used, make sure that classes are equally
  # distributed across folds

  if (!is.null(class)) {
    select_cols = c(space_var, class)
    unit = unique(data[, select_cols, with = FALSE])

    # unit needs to be a data.frame here
    unit$cstf_fold = createFolds2(
      as.data.frame(unit)[, which(names(unit) == class)],
      k = k, list = FALSE)

    data = merge(data, unit,
      by.x = c(space_var, class),
      by.y = c(space_var, class), all.x = TRUE, sort = FALSE)
    space_var = "cstf_fold"
  }

  if (!is.null(space_var)) {
    if (k > uniqueN(data[[space_var]])) {
      k = uniqueN(data[[space_var]]) # nocov start
      warningf("'sptcv_cstf': Number of folds is higher than number of unique
        locations.
        Setting folds to the maximum amount of unique levels of variable
        '%s' which is '%s'.", space_var, k, wrap = TRUE) # nocov end
    }
  }
  if (!is.null(time_var)) {
    if (k > uniqueN(data[[time_var]])) {
      k = uniqueN(data[[time_var]]) # nocov start
      warningf("'sptcv_cstf': Number of folds is higher than unique points in
        time.
        Setting folds to the maximum amount of unique levels of variable
        '%s' which is '%s'.", time_var, k, wrap = TRUE) # nocov end
    }
  }

  seed = sample(1:1000, 1)

  # split space into k folds
  if (!is.null(space_var)) {
    set.seed(seed)

    spacefolds = lapply(createFolds2(
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
    timefolds = lapply(createFolds2(
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

# taken from https://github.com/topepo/caret/blob/master/pkg/caret/R/createDataPartition.R
# to avoid having to include {caret} as a dependency
#' @importFrom stats quantile
createFolds2 = function(y, k = 10, list = TRUE, returnTrain = FALSE) {
  if (is.numeric(y)) {
    cuts = floor(length(y) / k)
    if (cuts < 2) cuts = 2
    if (cuts > 5) cuts = 5
    breaks = unique(quantile(y, probs = seq(0, 1, length = cuts)))
    y = cut(y, breaks, include.lowest = TRUE)
  }

  if (k < length(y)) {

    y = factor(as.character(y))
    numInClass = table(y)
    foldVector = vector(mode = "integer", length(y))

    for (i in 1:length(numInClass)) {
      min_reps = numInClass[i] %/% k
      if (min_reps > 0) {
        spares = numInClass[i] %% k
        seqVector = rep(1:k, min_reps)
        if (spares > 0) seqVector = c(seqVector, sample(1:k, spares))
        foldVector[which(y == names(numInClass)[i])] = sample(seqVector)
      } else {
        foldVector[which(y == names(numInClass)[i])] = sample(1:k,
          size = numInClass[i])
      }
    }
  } else {
    foldVector = seq(along = y)
  }
  if (list) {
    out = split(seq(along = y), foldVector)
    names(out) = paste("Fold",
      gsub(" ", "0", format(seq(along = out))),
      sep = "")
    if (returnTrain) {
      out = lapply(out, function(data, y) y[-data],
        y = seq(along = y))
    }
  } else {
    out = foldVector
  }
  out
}
