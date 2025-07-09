rm(list = ls(all = TRUE))
gc()
graphics.off()


library(terra)
library(readxl)
library(ggplot2)
library(dplyr)
library(sf)

# Paths
tiff_name <- 'AN_TJ_1_SpectralSpecies'
tiff_path <- paste0("data/MasterThesis/03_Spectral_Species/", tiff_name, ".tiff")
excel_file <- "data/MasterThesis/11_PlotClusters/Cluster_Assignement.xlsx"

# Read the GeoTIFF
r <- rast(tiff_path)

# Read the Excel
cluster_info <- read_excel(excel_file)

# Extract the relevant testsite name (without "_SpectralSpecies")
tiff_name <- tools::file_path_sans_ext(basename(tiff_path))
tiff_name_clean <- sub("_SpectralSpecies$", "", tiff_name)

# Filter the Excel data to entries matching this TIFF
matching_entries <- cluster_info %>%
  filter(grepl(tiff_name_clean, Testsite))

# If matching entries are found
if (nrow(matching_entries) > 0) {
  # Turn into sf points
  points_sf <- st_as_sf(matching_entries,
                        coords = c("Longitude", "Latitude"),
                        crs = 4326)

  # Transform to raster CRS if different
  if (st_crs(points_sf) != st_crs(r)) {
    points_sf <- st_transform(points_sf, crs = st_crs(r))
  }

  # Extract raster values
  extracted_values <- terra::extract(r, vect(points_sf))

  # Add the extracted raster values back to the matching entries
  matching_entries$Raster_Value <- extracted_values[, 2]  # [,2] because first column is ID

  matching_entries <- matching_entries %>%
    filter(Raster_Value != 0) %>%
    mutate(Raster_Value = factor(Raster_Value),
           Cluster = factor(Cluster)) %>%
    droplevels()


  # matching_entries <- matching_entries %>%
  #   mutate(
  #     Cluster = droplevels(Cluster),
  #     Raster_Value = droplevels(Raster_Value)
  #   )


  # View result
  print(matching_entries)

} else {
  print("No matching entries found for this TIFF.")
}

contingency_df <- matching_entries %>%
  select(Cluster, Raster_Value) %>%
  droplevels()

str(contingency_df)

# Create contingency table
contingency_table <- table(matching_entries$Cluster, matching_entries$Raster_Value)

# Perform Chi-squared test
chi_result <- chisq.test(contingency_table)

# Print interpretation
if (chi_result$p.value < 0.01) {
  cat("Strong association (p < 0.01) between Cluster and Raster_Value.\n")
} else if (chi_result$p.value < 0.05) {
  cat("Moderate association (p < 0.05) between Cluster and Raster_Value.\n")
} else {
  cat("No significant association (p ≥ 0.05) between Cluster and Raster_Value.\n")
}

# Print p-value
cat(sprintf("Chi-squared test p-value: %.4f\n", chi_result$p.value))

# Visualize with a heatmap
df_plot <- as.data.frame(contingency_table)
colnames(df_plot) <- c("Cluster", "Raster_Value", "Count")

ggplot(df_plot, aes(x = Raster_Value, y = Cluster, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(
    title = paste0("Heatmap of Cluster vs Raster_Value Testsite ", tiff_name_clean),
    x = "Raster Value",
    y = "Cluster",
    fill = "Count"
  )

# Initialize Fisher's p-value and significance
fisher_p_value <- NA
fisher_sig <- "Error/NA"
fischer_error <- FALSE

# Fisher's exact test
fisher_result <- tryCatch({
  fisher_test_result <- fisher.test(contingency_table)
  fisher_p_value <- fisher_test_result$p.value

  fisher_test_result # Return the result object
}, error = function(e) {
  if (grepl("FEXACT error 501", e$message) || grepl("FEXACT error 5", e$message)) {
    cat("Exact Fisher's test failed due to FEXACT error (501 or 5). Performing simulation...\n")
    fischer_error <<- TRUE
    fisher_test_result_simulated <- fisher.test(contingency_table, simulate.p.value = TRUE)
    fisher_p_value <- fisher_test_result_simulated$p.value
    fisher_test_result_simulated # Return the simulated result object
  } else {
    stop(e)
  }
})

fischer_suffix <- ""
if(fischer_error){
  fischer_suffix <- " (Sim.)"
}

fisher_sig <- ifelse(
  fisher_result$p.value < 0.01,
  paste0("Strong", fischer_suffix),
  ifelse(
    fisher_result$p.value < 0.05,
    paste0("Moderate", fischer_suffix),
    paste0("None", fischer_suffix)
  )
)

# Now you can access fisher_p_value and fisher_sig
if (!is.null(fisher_result)) {
  cat(sprintf("Fisher's Exact Test p-value: %.4f\n", fisher_result$p.value))
  cat(sprintf("Fisher's Exact Test significance: %s\n", fisher_sig))
} else {
  cat("Fisher's Exact Test did not complete successfully.\n")
}

