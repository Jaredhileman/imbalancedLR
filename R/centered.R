#' Calculates the center of the normal dist
#'
#' Using the logistic function, takes a desired proportion / probability of
#' success and returns the mean of the distribution that will likely generate
#' probability close to desired proportions.
#'
#' @param prob The desired probability of success
#' @return Where to center the normal distribution
#'
centering <- function(prob) {
  return(log(prob / (1 - prob)))
}
