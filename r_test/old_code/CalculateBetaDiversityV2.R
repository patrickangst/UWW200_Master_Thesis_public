rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load necessary packages
library(vegan)
library(readxl)
library(dplyr)
library(NbClust)
library(dbscan)
library(ggplot2)

# Load species abundance data
species_data <- read_excel("All_plots_Desktop.xlsx", sheet = 'input')
colnames(species_data) <- trimws(colnames(species_data))

# Find the index of 'Number of species'
index <- which(names(species_data) == 'Number of species')

# Extract required columns (all columns right of 'Number of species' included)
selected_columns <- species_data %>%
  dplyr::select('Table number', 'Dataset', 'Testsite', 'Subzone', 'Richness', 'Shannon', 'Evenness', everything()[index:ncol(.)])

# Remove metadata columns **before the loop**
beta_subset <- selected_columns %>%
  dplyr::select(-c('Dataset', 'Testsite', 'Subzone', 'Richness', 'Shannon', 'Evenness', 'Number of species'))

# List of test sites
test_sites <- unique(selected_columns$Testsite)

# Loop through each test site
for (site in test_sites) {
  # Filter data for the current test site
  dataset_site <- beta_subset %>%
    dplyr::filter(selected_columns$Testsite == site)
  
  # Replace NA values with 0
  dataset_site[is.na(dataset_site)] <- 0
  
  # Skip empty datasets
  if (nrow(dataset_site) < 2) {
    print(paste("Skipping", site, "due to insufficient data"))
    next
  }
  
  # Calculate Bray-Curtis beta diversity
  bray_curtis_matrix <- vegdist(dataset_site, method = "bray")
  
  # Hierarchical clustering
  hc <- hclust(bray_curtis_matrix, method = "ward.D2")
  plot(hc, main = paste("Hierarchical Clustering of Plots -", site), sub = "", xlab = "Plots", ylab = "Dissimilarity")
  
  # DBSCAN clustering
  bray_coords <- cmdscale(bray_curtis_matrix, k = 2)
  db <- dbscan(bray_coords, eps = 0.2, minPts = 3)
  dataset_site$Community <- as.factor(db$cluster)
  
  # DBSCAN plot
  # ggplot(as.data.frame(bray_coords), aes(x = V1, y = V2, color = as.factor(db$cluster))) +
  #   geom_point(size = 4) +
  #   theme_minimal() +
  #   labs(title = paste("DBSCAN Clustering of Plant Communities -", site), color = "Community") +
  #   print()
  
  print(ggplot(as.data.frame(bray_coords), aes(x = V1, y = V2, color = as.factor(db$cluster))) +
          geom_point(size = 4) +
          theme_minimal() +
          labs(title = paste("DBSCAN Clustering of Plant Communities -", site), color = "Community"))
  
  if (nrow(bray_coords) > 2) { # Ensure enough data points exist
    max_clusters <- min(10, nrow(bray_coords) - 1) # Prevent too many clusters
    
    nb <- NbClust(data = bray_coords, distance = "euclidean",
                  min.nc = 2, max.nc = max_clusters, method = "kmeans")
    
    if (!is.null(nb$Best.nc)) { # Check if NbClust produced a valid result
      best_k <- as.integer(nb$Best.nc[1])
      
      # Ensure best_k is within valid bounds
      if (!is.na(best_k) && best_k >= 2 && best_k <= nrow(bray_coords) && best_k <= max_clusters) {
        print(paste("Optimal number of clusters for", site, ":", best_k))
        tryCatch({
          kmeans_result <- kmeans(bray_coords, centers = best_k) # Now safe to run k-means
        }, error = function(e) {
          print(paste("kmeans error for", site, ":", e$message))
        })
      } else {
        print(paste("NbClust suggested an invalid cluster number for", site))
      }
    } else {
      print(paste("NbClust did not return a valid result for", site))
    }
  } else {
    print(paste("Not enough plots for clustering at", site))
  }
}