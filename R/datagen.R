data_gen <- function(size, center) {
  pred <- rnorm(size, center, 1)
  prob <- exp(pred) / (1 + exp(pred))
  response <- rbinom(size, 1, prob)
  data <- as.data.frame(cbind(response, pred))
  colnames(data) <- c("Y", "X")
  return(data)
}

