#####
# Simulate 100 datasets of size n = 100

datalist <- list()
youden_list <- list()

centered <- centering(seq(0.05, .95, by = 0.05))
for (i in 1:5) {
  datalist[[i]] <- data_gen(size = 100, center = centered)
  youden_list[[i]] <- best_cutoff(datalist[[i]])
}

View(datalist)
View(youden_list)

