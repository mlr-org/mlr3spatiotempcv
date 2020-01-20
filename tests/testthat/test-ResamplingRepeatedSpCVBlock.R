test_that("Error when neither cols & rows | range is specified", {
  rsmp = rsmp("repeated-spcv-block", repeats = 2)
  expect_error(
    rsmp$instantiate(task), "Either 'range' or 'cols' & 'rows' need to be set.")
})

test_that("Error when length(range) != length(repeats)", {
  rsmp = rsmp("repeated-spcv-block", repeats = 2, range = 5)
  expect_error(
    rsmp$instantiate(task), ".*to be the same length as 'range'."
  )
})


test_that("Error when length(range) != length(repeats)", {
  rsmp = rsmp("repeated-spcv-block", repeats = 2, range = c(500, 1000))
  expect_silent(rsmp$instantiate(task))
})
