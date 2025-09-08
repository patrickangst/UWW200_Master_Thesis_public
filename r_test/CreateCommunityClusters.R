# Clear R environment, free memory, and close open graphics devices
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load required packages
library(vegan)
library(readxl)
library(writexl)
library(openxlsx)
library(dplyr)
library(NbClust)
library(dbscan)
library(ggplot2)
library(tidyr)
library(pracma)
library(ggdendro)
library(sf)

# Set directory paths
input_dir <- 'site_data'
output_dir_graphs <- "cluster_plots"
output_dir_xlsx <- "plot_metrics"
output_dir_shp <- "cluster_info_shp"
clusterinfo_file_path <- 'All_plots_Desktop.xlsx'
cluster_file_path <- file.path(output_dir_xlsx, "Cluster_Assignement.xlsx")

# Create necessary folders if they do not exist
if (!dir.exists(output_dir_graphs)) dir.create(output_dir_graphs)
if (!dir.exists(output_dir_xlsx)) dir.create(output_dir_xlsx)
if (!dir.exists(output_dir_shp)) dir.create(output_dir_shp)

# Remove old cluster assignment file if it exists
if (file.exists(cluster_file_path)) {
  file.remove(cluster_file_path)
}

# Load testsite cluster info and prepare it
testsite_clusterinfo_df <- read_excel(clusterinfo_file_path, sheet = 'ClusterInfo') %>%
  as.data.frame() %>%
  mutate(Testsite = paste0(`Table number`, '_', Testsite)) %>%
  select(Testsite, Subzone, Longitude, Latitude)

# Initialize an empty dataframe to collect overall results
results_df <- data.frame(
  Testsite = character(),
  simpson_index_PC = numeric(),
  Unique_Plant_Cummunities = integer(),
  stringsAsFactors = FALSE
)

# Define function to perform clustering and plotting for each input file
create_plots <- function(file_path, clusterinfo_df) {
  file_name_no_ext <- tools::file_path_sans_ext(basename(file_path))
  
  # Read species abundance data
  species_data <- read_excel(file_path, sheet = 'Sheet 1')
  colnames(species_data) <- trimws(colnames(species_data))
  
  df <- as.data.frame(species_data)
  df_cluster_assignement <- df %>% select(PlotIdentifier)
  rownames(df) <- df$PlotIdentifier
  
  # Select species abundance columns only
  df_species_list <- df %>%
    select(-PlotIdentifier, -Shannon, -Simpson, -Evenness, -Richness, -HabitatType)
  
  df_habitat_info <- df %>%
    select(PlotIdentifier, HabitatType) %>%
    rename(Testsite = PlotIdentifier)
  
  # Calculate Bray-Curtis dissimilarity matrix
  bray_dist <- vegdist(df_species_list, method = "bray")
  bray_matrix <- as.matrix(bray_dist)
  
  # Principal Coordinates Analysis (PCoA)
  bray_pcoa <- cmdscale(bray_dist, eig = TRUE, k = 2)
  bray_coords <- as.data.frame(bray_pcoa$points)
  colnames(bray_coords) <- c("PCoA1", "PCoA2")
  
  # Perform DBSCAN clustering
  db <- dbscan(as.matrix(bray_dist), eps = 0.805, minPts = 2)
  bray_coords$ClusterDBSCAN <- as.factor(db$cluster)
  
  # Perform Hierarchical clustering
  hc <- hclust(bray_dist, method = "ward.D2")
  
  # Create and save dendrogram plot
  dendro_data <- ggdendro::dendro_data(hc)
  p_dendro <- ggplot(segment(dendro_data)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    theme_minimal() +
    labs(title = paste0("Hierarchical Clustering of Plot Site ", file_name_no_ext),
         x = "", y = "Height") +
    scale_y_continuous(expand = c(0.05, 0)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_continuous(breaks = 1:length(hc$labels), labels = hc$labels)
  
  ggsave(
    filename = paste0(output_dir_graphs, "/", file_name_no_ext, "_DENDRO.png"),
    plot = p_dendro, width = 8, height = 6, dpi = 300
  )
  print(p_dendro)
  
  # Create and save DBSCAN clustering plot
  p_dbscan <- ggplot(bray_coords, aes(x = PCoA1, y = PCoA2, color = ClusterDBSCAN)) +
    geom_point(size = 4) +
    theme_minimal() +
    labs(
      title = paste("DBSCAN Clustering -", file_name_no_ext),
      x = "PCoA Dimension 1",
      y = "PCoA Dimension 2",
      color = "Cluster"
    )
  
  ggsave(
    filename = paste0(output_dir_graphs, "/", file_name_no_ext, "_DBSCAN_clusters.png"),
    plot = p_dbscan, width = 8, height = 6, dpi = 300
  )
  print(p_dbscan)
  
  # Perform HDBSCAN clustering on PCoA coordinates
  hdb <- hdbscan(bray_coords[, 1:2], minPts = 2)
  bray_coords$ClusterHDBSCAN <- as.factor(hdb$cluster)
  
  # Create and save HDBSCAN clustering plot
  p_hdbscan <- ggplot(bray_coords, aes(x = PCoA1, y = PCoA2, color = ClusterHDBSCAN)) +
    geom_point(size = 4) +
    theme_minimal() +
    labs(
      title = paste0("HDBSCAN Plot Clustering (Bray-Curtis) for test site ", file_name_no_ext),
      x = "PCoA Dimension 1",
      y = "PCoA Dimension 2",
      color = "Cluster"
    )
  
  ggsave(
    filename = paste0(output_dir_graphs, "/", file_name_no_ext, "_HDBSCAN_clusters.png"),
    plot = p_hdbscan, width = 8, height = 6, dpi = 300
  )
  print(p_hdbscan)
  
  # Prepare cluster assignment table
  df_clusters <- bray_coords %>%
    tibble::rownames_to_column(var = "Testsite") %>%
    select(Testsite, ClusterHDBSCAN)
  
  # Join with spatial info
  df_combined <- clusterinfo_df %>%
    inner_join(df_clusters, by = "Testsite") %>%
    rename(Cluster = ClusterHDBSCAN) %>%
    arrange(Testsite)
  
  df_combined <- merge(df_combined, df_habitat_info, by = "Testsite", all.x = TRUE)
  
  df_combined <- df_combined %>%
    mutate(
      HTS1 = sub("\\.[^.]*$", "", HabitatType),      # Removes last segment
      HTS2 = sub("\\..*", "", HabitatType)           # Keeps only first segment
    )

  
  # Create spatial data frame
  df_combined_sf <- st_as_sf(df_combined, coords = c("Longitude", "Latitude"), crs = 4326)
  
  # Write shapefile with cluster info
  shp_name <- paste0(file_name_no_ext, '_clusterinfo.shp')
  st_write(df_combined_sf, file.path(output_dir_shp, shp_name), delete_layer = TRUE)
  
  # Prepare Excel cluster assignment output
  df_testsite_combined <- clusterinfo_df %>%
    inner_join(df_clusters, by = "Testsite") %>%
    rename(Cluster = ClusterHDBSCAN) %>%
    arrange(Testsite)
  
  # Append to existing Excel file or create new one
  if (file.exists(cluster_file_path)) {
    existing_data <- read_xlsx(cluster_file_path)
    combined_data <- bind_rows(existing_data, df_testsite_combined) %>%
      distinct(Testsite, .keep_all = TRUE) # Avoid duplicate entries
    write_xlsx(combined_data, path = cluster_file_path)
  } else {
    write_xlsx(df_testsite_combined, path = cluster_file_path)
  }
  
  # Calculate cluster diversity metrics
  bray_coords_transformed <- bray_coords %>%
    select(ClusterHDBSCAN) %>%
    tibble::rownames_to_column("PlotIdentifier")
  
  bray_coords_summary <- bray_coords_transformed %>%
    count(ClusterHDBSCAN) %>%
    pivot_wider(names_from = ClusterHDBSCAN, values_from = n, values_fill = 0)
  
  simpson_index <- diversity(bray_coords_summary, index = "simpson")
  unique_clusters <- length(unique(bray_coords$ClusterHDBSCAN))
  
  # Append summary statistics to the results dataframe
  assign("results_df", rbind(
    results_df,
    data.frame(
      Testsite = file_name_no_ext,
      simpson_index_PC = simpson_index,
      Unique_Plant_Cummunities = unique_clusters,
      stringsAsFactors = FALSE
    )
  ), envir = .GlobalEnv)
}

# List all Excel files in input directory
files <- list.files(path = input_dir, pattern = "\\.xlsx$", full.names = TRUE)

# Process each file
for (file in files) {
  #debug(create_plots)
  create_plots(file, testsite_clusterinfo_df)
}

# Save summary statistics to Excel file
write_xlsx(results_df, path = file.path(output_dir_xlsx, "Cluster_Summary.xlsx"))