test_that("examples work", {
  expect_equal(
    object = rolling_mean(x = 1:5, window_width = 3),
    expected = c(NA,2,3,4,NA))
  expect_equal(
    object = rolling_mean(x = 1:5, window_width = 7),
    expected = as.double(c(NA,NA,NA,NA,NA)))
  expect_equal(
    object = rolling_mean(x = 1:5, window_width = 5),
    expected = c(NA,NA,3,NA,NA))
  expect_equal(
    object = rolling_mean(x = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE), window_width = 3),
    expected = c(NA,1,2/3,2/3,2/3,1,NA))
})
