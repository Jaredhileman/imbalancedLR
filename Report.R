########################################
# Simulate 100 datasets of size n = 100

datalist <- list()
cutoff_info <- list()

centered <- centering(seq(0.05, .95, by = 0.05))

for (i in seq_along(centered)) {
  datalist[[i]] <- list()  # Initialize inner list
  cutoff_info[[i]] <- list()  # Initialize inner list

  for (j in 1:5) {
    datalist[[i]][[j]] <- data_gen(size = 100, center = centered[i])
    cutoff_info[[i]][[j]] <- best_cutoff(datalist[[i]][[j]])
  }

  datalist[[i]]$centered <- centered[i]
  cutoff_info[[i]]$centered <- centered[i]

}

View(datalist)
View(cutoff_info)

