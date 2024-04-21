youdens_J <- function(model, data) {
  pred <- predict(model, data, type = "response")
  cutoff <- seq(0, 1, .05)
  vector1 <- ifelse(pred < cutoff, 0, 1)
  prediction_matrix()
  lst <- list(cutoff)

  pred_table <- table(data$Y, )
  return(roc_obj$thresholds[which.max(roc_obj$sensitivities + roc_obj$specificities - 1)])
}
