
#' @export
best_cutoff <- function(dataset) {
  model <- glm(Y ~ ., data = dataset, family = binomial)
  pred <- predict(model, dataset, type = "response")
  cutoff <- seq(0.05, 0.95, .05)
  lst <- list()
  lst$prediction_matrix <- lapply(cutoff,
                                  function(val) ifelse(pred > val, 1, 0))
  lst$cutoff <- cutoff
  lst$true_imbalance <- sum(dataset$Y) / nrow(dataset)

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
    lst$accuracy[[i]] <- (TP + TN) / (TP + TN + FP + FN)
    lst$dist[[i]] <- abs(sensitivity - specificity)
  }

  lst$dist <- unlist(lst$dist)
  lst$accuracy <- unlist(lst$accuracy)
  lst$sensitivity <- unlist(lst$sensitivity)
  lst$specificity <- unlist(lst$specificity)
  lst$Youdens_J <- unlist(lst$Youdens_J)
  done = FALSE
  sorted_indices <- order(lst$Youdens_J, decreasing = TRUE)

  # Iterate over these indices
  for (index in sorted_indices) {
    # Use 'index' to reference the correct items in lst$dist and lst$Youdens_J
    if (lst$dist[index] < 0.1) {
      lst$optimal_J <- lst$Youdens_J[index]
      lst$optimal_cutoff <- lst$cutoff[index]
      lst$optimal_sensitivity <- lst$sensitivity[index]
      lst$optimal_specificity <- lst$specificity[index]
      done = TRUE
      break  # Exit the loop once the condition is met
    }
  }

  if (!done) {
    lst$optimal_J <- max(lst$Youdens_J)
    lst$optimal_cutoff <- lst$cutoff[which.max(lst$Youdens_J)]
    lst$optimal_sensitivity <- lst$sensitivity[which.max(lst$Youdens_J)]
    lst$optimal_specificity <- lst$specificity[which.max(lst$Youdens_J)]
    done = TRUE
  }

  return(lst)
}




