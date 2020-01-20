test_that("Error when neither cols & rows | range is specified", {
  rsmp = rsmp("spcv-block")
  expect_error(
    rsmp$instantiate(task), "Either 'range' or 'cols' & 'rows' need to be set.")
})

test_that("Error when only one of rows or cols is set", {
  rsmp = rsmp("spcv-block", rows = 4)
  expect_error(
    rsmp$instantiate(task), "Either 'range' or 'cols' & 'rows' need to be set."
  )
})

test_that("Error when length(range) >= 2", {
  expect_error(rsmp("spcv-block", range = c(500, 1000)))
})
