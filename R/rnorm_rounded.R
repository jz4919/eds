#' Simulate rounded Gaussian random variates
#'
#' @param n Number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param mu Vector of means.
#' @param sigma Vector of standard deviations.
#' @param digits Integer indicating the number of decimal places to be used in rounding. Negative values are used to round to a power of ten, so for example `digits = -2` rounds to the nearest hundred. (See 'Details' of `base::round()`).
#'
#' @return Vector of Gaussian random variates, rounded to to specified number of decimal places.
#'
#' @import stats
#' @export
#"
#' @examples
#' rnorm_rounded(n = 10)
rnorm_rounded <- function(n, mu = 0, sigma = 1, digits = 0){
  raw_values <- rnorm(n, mean = mu, sd = sigma)
  rounded_values <- round(raw_values, digits)
  return(rounded_values)
}
