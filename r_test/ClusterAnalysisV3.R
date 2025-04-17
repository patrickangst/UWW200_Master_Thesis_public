# Load required libraries
library(terra)
library(NbClust)
library(tools)
library(modeest) # For calculating the mode (Most Frequent Value)
library(statip)

min_clusters <- 2
max_clusters <- 50
set.seed(123)

# Step 1: Load the GeoTIFF
hyperspectral_path <-
  "hs/AN_TJ_1_pc_selection.tif" # Replace with your file path
geo_data <- rast(hyperspectral_path)

# Step 2: Convert the GeoTIFF to a 2D matrix
# Rows: Pixels; Columns: Bands
data_matrix <- as.matrix(terra::values(geo_data))
data_matrix <- na.omit(data_matrix)

# Step 3: Define indices excluding GAP, Gamma, Gplus, and Tau
indices <- c(
  "kl",
  "ch",
  "hartigan",
  "ccc",
  "scott",
  "marriot",
  "trcovw",
  "tracew",
  "friedman",
  "rubin",
  "cindex",
  "db",
  "silhouette",
  "duda",
  "pseudot2",
  "beale",
  "ratkowsky",
  "ball",
  "ptbiserial",
  "frey",
  "mcclain",
  "dunn",
  "hubert",
  "sdindex",
  "dindex",
  "sdbw"
)

# Step 4: Initialize a vector to store the best number of clusters for each index
best_cluster_numbers <- numeric()

# Step 5: Loop through each index and calculate the optimal number of clusters
for (index in indices) {
  cat("\nProcessing index:", index, "\n")
  
  # Try NbClust for this index
  nb_result <- tryCatch({
    NbClust(
      data = data_matrix,
      distance = "euclidean",
      min.nc = min_clusters,
      max.nc = max_clusters,
      method = "kmeans",
      index = index
    )
  }, error = function(e) {
    cat("  ❌ Error for index:", index, "-", e$message, "\n")
    return(NULL)
  })
  
  # Check validity of result
  if (!is.null(nb_result) && !is.null(nb_result$Best.nc)) {
    # Try extracting cluster number, robust to different structures
    best_k_try <- tryCatch({
      if (is.matrix(nb_result$Best.nc)) {
        # Matrix format (most common)
        best_k <- as.numeric(nb_result$Best.nc[1,])
      } else if (is.vector(nb_result$Best.nc)) {
        # Vector format (e.g., for 'kl', 'ch', etc.)
        best_k <- as.numeric(nb_result$Best.nc["Number_clusters"])
      } else {
        stop("Unknown Best.nc format")
      }
      cat("  ✅ Best number of clusters for", index, ":", best_k, "\n")
      best_k
    }, error = function(e) {
      cat("  ⚠️ Failed to extract Best.nc for",
          index,
          "-",
          e$message,
          "\n")
      NA
    })
    
    if (!is.na(best_k_try)) {
      best_cluster_numbers <- c(best_cluster_numbers, best_k_try)
    }
  } else {
    cat("  ⚠️ Skipping index:", index, "- Invalid or missing Best.nc\n")
  }
  
  # Print current summary
  cat("  ➕ Accumulated cluster numbers:", best_cluster_numbers, "\n")
  
  rm(nb_result)
  gc()
}

# Step 6: Determine the most frequent number of clusters (majority vote)
majority_vote_number <- mfv(best_cluster_numbers)

# Step 7: Print and validate the result
print(paste("Most frequent number:", majority_vote_number))

if (is.na(majority_vote_number)) {
  stop("Best number of clusters (majority_vote_number) is NA. Cannot proceed with k-means.")
}

# Step 8: Save the workspace and most frequent number to files
base_name <- file_path_sans_ext(basename(hyperspectral_path))
workspace_filename <-
  file.path('nbclust_analysis',
            paste0(base_name, "_clusteranalysis.RData"))
save.image(file = workspace_filename)

txt_filename <-
  file.path('nbclust_analysis',
            paste0(base_name, "_most_frequent_number.txt"))
write(majority_vote_number, file = txt_filename)

cat("Analysis completed and results saved.")
