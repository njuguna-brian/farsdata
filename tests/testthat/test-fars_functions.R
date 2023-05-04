test_that("make_filename wors", {
  filename = make_filename(2013)
  expect_equal(filename, "accident_2013.csv.bz2")
})
