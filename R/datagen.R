data_gen <- function(size, center) {
  pred <- rnorm(size, center, 1)
  prob <- exp(pred) / (1 + exp(pred))
  response <- rbinom(size, 1, prob)
  data <- as.data.frame(cbind(response, pred))
  colnames(data) <- c("Y", "X")
  return(data)
}

youdens_J <- function(model, data) {
  pred <- predict(model, data, type = "response")
  cutoff <- seq(0.05, 0.95, .05)

  pred_table <- table(data$Y, )
  return(roc_obj$thresholds[which.max(roc_obj$sensitivities + roc_obj$specificities - 1)])
}
