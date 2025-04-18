# Load required libraries
library(terra)
library(NbClust)
library(parallel)
library(doParallel)
library(foreach)
library(statip)

# Step 1: Load the GeoTIFF
geo_data <- rast("hs/BRW_PW_1_pc_selection.tif") # Replace with the path to your GeoTIFF file

# Step 2: Convert the GeoTIFF to a 2D matrix
# Rows: Pixels; Columns: Bands
data_matrix <- as.matrix(terra::values(geo_data))

# Step 3: Set up parallel computing
num_cores <- detectCores() # Detect number of cores available
cl <- makeCluster(num_cores - 1) # Use all but one core
registerDoParallel(cl)

# Step 4: Define indices excluding GAP, Gamma, Gplus, and Tau
indices <- c("kl", "ch", "hartigan", "ccc", "scott", "marriot",
             "trcovw", "tracew", "friedman", "rubin", "cindex", "db",
             "silhouette", "duda", "pseudot2", "beale", "ratkowsky",
             "ball", "ptbiserial", "frey", "mcclain", "dunn", "hubert",
             "sdindex", "dindex", "sdbw")

# Step 5: Parallel computation of clustering indices
results <- foreach(index = indices, .combine = rbind, .packages = "NbClust") %dopar% {
  cat("Computing for index:", index, "\n")
  NbClust(data_matrix, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = index)
}

# Stop parallel cluster
stopCluster(cl)

# Step 6: Combine and analyze results
combined_results <- do.call(cbind, lapply(results, function(res) res$Best.nc))
print(combined_results)
