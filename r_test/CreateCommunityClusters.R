rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load necessary packages
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

# Load species abundance data
input_dir <- 'site_data'
output_dir_graphs <- "cluster_plots"
output_dir_xlsx <- "plot_metrics"
output_dir_shp <- "cluster_info_shp"
clusterinfo_file_path <- 'All_plots_Desktop.xlsx'
cluster_file_path <- file.path(output_dir_xlsx, "Cluster_Assignement.xlsx")


# Create folder for plots if not exists
if (!dir.exists(output_dir_graphs)) {
  dir.create(output_dir_graphs)
}
if (!dir.exists(output_dir_xlsx)) {
  dir.create(output_dir_xlsx)
}
if (!dir.exists(output_dir_shp)) {
  dir.create(output_dir_shp)
}

# Create CLuster assignement file
testsite_clusterinfo_df <- read_excel(clusterinfo_file_path, sheet = 'ClusterInfo')
testsite_clusterinfo_df <- testsite_clusterinfo_df %>%
  as.data.frame() %>%
  mutate(Testsite = paste0(`Table number`, '_', Testsite)) %>%
  select(Testsite, Subzone, Longitude, Latitude)

# Initialize an empty data frame to store results
results_df <- data.frame(
  Testsite = character(),
  simpson_index_PC = numeric(),
  Unique_Plant_Cummunities = integer(),
  stringsAsFactors = FALSE  # Important for character columns
)

create_plots <- function(file_path, clusterinfo_df) {
  file_name_no_ext <- tools::file_path_sans_ext(basename(file_path))
  
  species_data <- read_excel(file_path, sheet = 'Sheet 1')
  
  colnames(species_data) <- trimws(colnames(species_data))

  df <- species_data %>%
    as.data.frame()
  
  df_cluster_assignement <- df %>%
    select(PlotIdentifier)
  
  rownames(df) <- df$PlotIdentifier
  
  df_species_list <- df %>%
    select(-PlotIdentifier, -Shannon, -Simpson, -Evenness, -Richness)
  
  # Assuming your data frame is called `df_species_list`
  # (rows = plots, columns = species, values = percent cover)
  bray_dist <- vegdist(df_species_list, method = "bray")
  bray_matrix <- as.matrix(bray_dist)
  
  bray_pcoa <- cmdscale(bray_dist, eig = TRUE, k = 2)
  bray_coords <- as.data.frame(bray_pcoa$points)
  colnames(bray_coords) <- c("PCoA1", "PCoA2")

  # Once you choose eps (based on elbow of above plot):
  db <- dbscan(as.matrix(bray_dist), eps = 0.805, minPts = 2)
  
  # Add cluster assignment
  bray_coords$ClusterDBSCAN <- as.factor(db$cluster)
  
  
  hc <- hclust(bray_dist, method = "ward.D2")  # Ward’s method (minimizes variance)
  # hc <- hclust(beta_dist, method = "average")
  plot(
    hc,
    labels = rownames(df_species_list),
    main = paste0("Hierarchical Clustering of Plot Site ", file_name_no_ext)
  )
  
  # Convert to dendrogram object for ggplot2
  dendro_data <- ggdendro::dendro_data(hc)
  
  # Plot with ggplot2
  p_dendro <- ggplot(segment(dendro_data)) +
    geom_segment(aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    )) +
    theme_minimal() +
    labs(
      title = paste0("Hierarchical Clustering of Plot Site ", file_name_no_ext),
      x = "",
      y = "Height"
    ) +
    scale_y_continuous(expand = c(0.05, 0)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_continuous(breaks = 1:length(hc$labels),
                       labels = hc$labels)
  
  ggsave(
    filename = paste0(output_dir_graphs, "/", file_name_no_ext, "_DENDRO.png"),
    plot = p_dendro,
    width = 8,
    height = 6
  )
  
  print(p_dendro)
  
  # Plot and save
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
    filename = paste0(
      output_dir_graphs,
      "/",
      file_name_no_ext,
      "_DBSCAN_clusters.png"
    ),
    plot = p_dbscan,
    width = 8,
    height = 6
  )
  
  print(p_dbscan)
  
  # similar_plots <- split(rownames(df_species_list), bray_coords$ClusterDBSCAN)
  # print(similar_plots)
  
  
  # Use PCoA coordinates
  hdb <- hdbscan(bray_coords[, 1:2], minPts = 2)  # Only one parameter
  bray_coords$ClusterHDBSCAN <- as.factor(hdb$cluster)
  
  # Plot
  p_hdbscan <- ggplot(bray_coords, aes(x = PCoA1, y = PCoA2, color = ClusterHDBSCAN)) +
    geom_point(size = 4) +
    theme_minimal() +
    labs(
      title = paste0(
        "HDBSCAN Plot Clustering (Bray-Curtis) for test site ",
        file_name_no_ext
      ),
      x = "PCoA Dimension 1",
      y = "PCoA Dimension 2",
      color = "Cluster"
    )
  
  ggsave(
    filename = paste0(
      output_dir_graphs,
      "/",
      file_name_no_ext,
      "_HDBSCAN_clusters.png"
    ),
    plot = p_hdbscan,
    width = 8,
    height = 6
  )
  
  print(p_hdbscan)
  
  df_clusters <- bray_coords %>%
    tibble::rownames_to_column(var = "Testsite")
  
  df_clusters <- df_clusters %>%
    select(Testsite, ClusterHDBSCAN)
  
  
  df_combined <- testsite_clusterinfo_df %>%
    inner_join(df_clusters, by = "Testsite") %>%
    rename(Cluster = ClusterHDBSCAN) %>%
    arrange(Testsite)
  
  df_combined_sf <- st_as_sf(df_combined,
                             coords = c("Longitude", "Latitude"),
                             crs = 4326)  # WGS84
  
  shp_name <- paste0(file_name_no_ext, '_clusterinfo.shp')
  
  st_write(df_combined_sf,
           file.path(output_dir_shp, shp_name),
           delete_layer = TRUE)
  
  
  # Create xlsx with the cluster info for all the plots in one file
  df_testsite_combined <- testsite_clusterinfo_df %>%
    inner_join(df_clusters, by = "Testsite") %>%
    rename(Cluster = ClusterHDBSCAN) %>%
    arrange(Testsite)
  
  
  if (file.exists(cluster_file_path)) {
    # Read existing
    existing_data <- read_xlsx(cluster_file_path)
    
    # Append new
    combined_data <- bind_rows(existing_data, df_testsite_combined)
    
    # Save
    write_xlsx(combined_data, path = cluster_file_path)
    
  } else {
    # If file does not exist, just write the new one
    write_xlsx(df_testsite_combined, path = cluster_file_path)
  }
  

  bray_coords_transfomed <- bray_coords %>%
    select(ClusterHDBSCAN) %>%
    tibble::rownames_to_column("PlotIdentifier")
  
  # Count how many plots per cluster
  bray_coords_summary <- bray_coords_transfomed %>%
    count(ClusterHDBSCAN) %>%
    pivot_wider(
      names_from = ClusterHDBSCAN,
      values_from = n,
      values_fill = 0
    )
  
  simpson_index <- diversity(bray_coords_summary, index = "simpson")
  
  unique_clusters <- length(unique(bray_coords$ClusterHDBSCAN))
  print(unique_clusters)
  
  # Append results to the data frame (inside the function)
  assign("results_df", rbind(
    results_df,
    data.frame(
      Testsite = file_name_no_ext,
      simpson_index_PC = simpson_index,
      Unique_Plant_Cummunities = unique_clusters,
      stringsAsFactors = FALSE
    )
  ), envir = .GlobalEnv) # Use assign to modify the global variable
  
}

files <- list.files(path = input_dir,
                    pattern = "\\.xlsx$",
                    full.names = TRUE)

for (file in files) {
  file_path <- file
  
  #debug(create_plots)
  create_plots(file_path, testsite_clusterinfo_df)
}

write_xlsx(results_df, path = file.path(output_dir_xlsx, "Cluster_Summary.xlsx"))
