
#' @export
youdens_J <- function(model, dataset) {
  pred <- predict(model, dataset, type = "response")
  cutoff <- seq(0, 1, .05)
  lst <- list()
  lst$prediction_matrix <- lapply(cutoff,
                                  function(val) ifelse(pred > val, 1, 0))
  lst$cutoff <- cutoff

  for (i in 1:length(lst$prediction_matrix)) {
    true_values <- factor(dataset$Y, levels = c(0, 1))
    predicted_values <- factor(lst$prediction_matrix[[i]], levels = c(0, 1))
    confusion <- table(true_values, predicted_values)
    TP <- confusion[2, 2]
    FP <- confusion[1, 2]
    TN <- confusion[1, 1]
    FN <- confusion[2, 1]
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    lst$sensitivity[[i]] <- sensitivity
    lst$specificity[[i]] <- specificity
    lst$Youdens_J[[i]] <- sensitivity + specificity - 1
  }
  print(class(lst$Youdens_J))
  lst$optimal_J <- max(unlist(lst$Youdens_J))
  lst$optimal_cutoff <- lst$cutoff[which(lst$Youdens_J == lst$optimal_J)]
  return(lst)
}




