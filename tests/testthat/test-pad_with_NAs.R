# Typical Behaviour

test_that("examples work", {
  expect_equal(
    object = pad_with_NAs(1:5, n_left = 0, n_right = 3),
    expected = c(1:5, NA, NA, NA))
  expect_equal(
    object = pad_with_NAs(c("spider", "mouse", "cat", "dog"), n_left = 1, n_right = 2),
    expected = c(NA, "spider", "mouse", "cat", "dog", NA, NA)
  )
})

# Edge Cases

test_that("padding by 0 works", {
  expect_equal(pad_with_NAs(x = 1:3, n_left = 1, n_right = 0), c(NA, 1, 2, 3))
  expect_equal(pad_with_NAs(x = 1:3, n_left = 0, n_right = 1), c(1, 2, 3, NA))
})

# Malformed Inputs

test_that("negative padding produces error", {
  expect_error(pad_with_NAs(x = 1:5, n_left = -1, n_right = 1))
  expect_error(pad_with_NAs(x = 1:5, n_left = 1, n_right = -1))
})

test_that("giving non-vector x produces error", {
  matrix_input <- diag(1:4)
  list_input <- list(1:5, "cat")

  expect_error(pad_with_NAs(x = matrix_input, n_left = 1, n_right = 1))
  expect_error(pad_with_NAs(x = list_input, n_left = 1, n_right = 1))
})
