# Load necessary library
library(ggplot2)

# 1. Simulate a 2D dataset with 3 obvious clusters
set.seed(123)
n <- 150
group1 <- data.frame(x = rnorm(n, mean = 2), y = rnorm(n, mean = 2))
group2 <- data.frame(x = rnorm(n, mean = 6), y = rnorm(n, mean = 6))
group3 <- data.frame(x = rnorm(n, mean = 10), y = rnorm(n, mean = 2))

# Combine all into one dataset
data <- rbind(group1, group2, group3)

# Optional: visualize the raw data (no clustering yet)
ggplot(data, aes(x, y)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Original Data (Unclustered)")




# Apply k-means with 3 clusters and 25 random starts
k_result <- kmeans(data, centers = 3, nstart = 25)

# Add cluster assignments to data
data$cluster <- as.factor(k_result$cluster)




# Plot the clustered data
ggplot(data, aes(x, y, color = cluster)) +
  geom_point(alpha = 0.7) +
  geom_point(data = as.data.frame(k_result$centers), 
             aes(x = x, y = y), 
             color = "black", 
             shape = 4, size = 5, stroke = 2) +
  theme_minimal() +
  labs(title = "K-means Clustering Result (k = 3)",
       subtitle = "Black X = Cluster Centers")

