test_that("examples work", {
  expect_equal(
    object = geometric_mean(x = c(1, NA), remove_NA = FALSE),
    expected = NA_real_)
  expect_equal(
    object = geometric_mean(x = c(1, 4, NA), remove_NA = TRUE),
    expected = 2)
  expect_equal(
    object = geometric_mean(x = c(1, 4), remove_NA = FALSE),
    expected = 2)
})
