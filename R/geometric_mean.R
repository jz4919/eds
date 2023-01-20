#' Calculate the geometric mean of a numeric vector
#'
#' @param x numeric vector
#' @param remove_NA logical scalar, indicating whether NA values should be
#' stripped before computation proceeds
#'
#' @return the geometric mean of the values in 'x', a numeric scalar value
#' @export
#'
#' @examples
#' geometric_mean(x = 1:10)
#' geometric_mean(x = c(1:10, NA), remove_NA = TRUE)
#'
geometric_mean <- function(x, remove_NA = FALSE){
  if (remove_NA == FALSE){
    return(prod(x) ^ (1/length(x)))
  }
  else {
    x_cleaned <- na.omit(x)
    return(prod(x_cleaned) ^ (1/length(x_cleaned)))
  }
}
