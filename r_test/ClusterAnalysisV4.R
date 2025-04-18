# Clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()


# Load required libraries
library(terra)
library(NbClust)
library(tools)
library(modeest)  # For calculating the mode (Most Frequent Value)
library(statip)
library(openxlsx)

# Parameters
min_clusters <- 2
max_clusters <- 50
set.seed(123)

# Define the indices to use
indices <- c(
  "silhouette",   # Compactness and separation
  "dunn",         # Separation vs. compactness
  "db",           # Davies-Bouldin
  "ch",           # Calinski-Harabasz
  "ratkowsky",    # Cluster separation
  "ptbiserial",   # Point-biserial correlation
  "cindex",       # Clustering index
  "tracew",       # Within-group sum of squares
  "sdindex",      # Standard deviation-based
  "sdbw",         # SD-BW (between-within SD ratio)
  "scott",        # Scott's index (parametric)
  "marriot",      # Marriott's index (compactness)
  "trcovw",       # Trace of within-cluster covariance
  "duda",         # Duda-Hart
  "pseudot2"      # Related to Duda-Hart
)


# Create output folder if it doesn't exist
if (!dir.exists("nbclust_analysis")) {
  dir.create("nbclust_analysis")
}

# List all .tif files in the hs/ directory
tif_files <- list.files("hs", pattern = "\\.tif$", full.names = TRUE)

# Loop through each file
for (hyperspectral_path in tif_files) {
  cat("\n============================\n")
  cat("Processing file:", hyperspectral_path, "\n")
  cat("============================\n")
  
  # Step 1: Load the GeoTIFF
  geo_data <- rast(hyperspectral_path)
  
  # Step 2: Convert to 2D matrix (pixels x bands)
  data_matrix <- as.matrix(terra::values(geo_data))
  data_matrix <- na.omit(data_matrix)
  
  if (nrow(data_matrix) < max_clusters) {
    cat("  ⚠️ Not enough data points for clustering (", nrow(data_matrix), " rows). Skipping.\n")
    next
  }
  
  # Step 3: Initialize
  best_cluster_numbers <- numeric()
  results_df <- data.frame(index = character(), best_k = numeric(), stringsAsFactors = FALSE)
  
  # Step 4: Loop over all indices
  for (index in indices) {
    cat("\n  ▶ Processing index:", index, "\n")
    
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
      cat("    ❌ Error for index:", index, "-", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(nb_result) && !is.null(nb_result$Best.nc)) {
      best_k_try <- tryCatch({
        if (is.matrix(nb_result$Best.nc)) {
          best_k <- as.numeric(nb_result$Best.nc[1,])
        } else if (is.vector(nb_result$Best.nc)) {
          best_k <- as.numeric(nb_result$Best.nc["Number_clusters"])
        } else {
          stop("Unknown Best.nc format")
        }
        cat("    ✅ Best number of clusters for", index, ":", best_k, "\n")
        best_k
      }, error = function(e) {
        cat("    ⚠️ Failed to extract Best.nc for", index, "-", e$message, "\n")
        NA
      })
      
      if (!is.na(best_k_try)) {
        best_cluster_numbers <- c(best_cluster_numbers, best_k_try)
        results_df <- rbind(results_df, data.frame(index = index, best_k = best_k_try))
        
        # 🔄 Save/update Excel after each index
        base_name <- file_path_sans_ext(basename(hyperspectral_path))
        excel_filename <- file.path('nbclust_analysis', paste0(base_name, "_most_frequent_number.xlsx"))
        write.xlsx(results_df, excel_filename, rowNames = FALSE)
        cat("    💾 Intermediate results written to", excel_filename, "\n")
      }
    } else {
      cat("    ⚠️ Skipping index:", index, "- Invalid or missing Best.nc\n")
    }
    
    cat("    ➕ Accumulated cluster numbers so far:", best_cluster_numbers, "\n")
    rm(nb_result)
    gc()
  }
  
  # Step 5: Majority vote (most frequent number of clusters)
  majority_vote_number <- mfv(best_cluster_numbers)
  if (length(majority_vote_number) > 1) {
    majority_vote_number <- mean(majority_vote_number)
  }
  
  cat("  📌 Most frequent number of clusters:", majority_vote_number, "\n")
  
  if (is.na(majority_vote_number)) {
    cat("  ⚠️ No valid result for this file. Skipping save.\n")
    next
  }
  
  # Step 6: Save results
  base_name <- file_path_sans_ext(basename(hyperspectral_path))
  
  workspace_filename <- file.path('nbclust_analysis', paste0(base_name, "_clusteranalysis.RData"))
  save.image(file = workspace_filename)
  
  txt_filename <- file.path('nbclust_analysis', paste0(base_name, "_most_frequent_number.txt"))
  write(majority_vote_number, file = txt_filename)
  
  excel_filename <- file.path('nbclust_analysis', paste0(base_name, "_most_frequent_number.xlsx"))
  write.xlsx(results_df, excel_filename)
  
  cat("  ✅ Results saved for", base_name, "\n")
}

cat("\n🎉 Batch analysis complete.\n")
