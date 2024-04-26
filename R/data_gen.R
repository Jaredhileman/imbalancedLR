#' Generating simulated data
#'
#' Utilizes a random normal generation with mean, sd set by user. The function
#' passes the generated data through a logistic function to output
#' probabilities, which are then passed into the rbinom function to generate
#' binary responses motivated by the predictor variable, X ~ N(mean, sd).
#'
#' @param size The number of observations to generate
#' @param center The mean of the normal distribution
#' @param sd The standard deviation of the normal distribution
#' @return A data frame with columns Y and X
#'
#' @export
data_gen <- function(size, center, sd = 1) {
  pred <- rnorm(size, center, sd)
  prob <- exp(pred) / (1 + exp(pred))
  response <- rbinom(size, 1, prob)
  data <- as.data.frame(cbind(response, pred))
  colnames(data) <- c("Y", "X")
  return(data)
}

