
# Define file paths for both raster maps

# Clear workspace
rm(list = ls())
graphics.off()

# Load necessary libraries
library(terra)
library(ggplot2)

# Define file paths for both raster maps
raster1_path <- "C:/Users/Patrick/Documents/GitHub/UWW200_Master_Thesis_public/MasterThesisPythonCode/test_data/SAVI"
raster2_path <- "C:/Users/Patrick/Documents/GitHub/UWW200_Master_Thesis_public/MasterThesisPythonCode/test_data/spectral_species.tif"

# Load both rasters
raster1 <- rast(raster1_path)
raster2 <- rast(raster2_path)


if (!compareGeom(raster1, raster2, stopOnError = FALSE)) {
  raster2 <- resample(raster2, raster1, method = "bilinear")  # Adjust resolution
}


common_extent <- intersect(ext(raster1), ext(raster2))
raster1 <- crop(raster1, common_extent)
raster2 <- crop(raster2, common_extent)


df1 <- as.data.frame(raster1, xy = TRUE, na.rm = FALSE)
df2 <- as.data.frame(raster2, xy = TRUE, na.rm = FALSE)

plot_data <- merge(df1, df2, by = c("x", "y"), all = FALSE)  # Keep only common pixels

# Rename columns for clarity
colnames(plot_data) <- c("x", "y", "raster1_values", "raster2_values")

# Remove NA values
plot_data <- na.omit(plot_data)

# Calculate Pearson correlation coefficient
cor_value <- cor(plot_data$raster1_values, plot_data$raster2_values, method = "pearson")

# Create scatter plot
scatter_plot <- ggplot(plot_data, aes(x = raster1_values, y = raster2_values)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  annotate("text", x = max(plot_data$raster1_values) * 0.7,
           y = max(plot_data$raster2_values) * 0.9,
           label = paste("Correlation: ", round(cor_value, 3)),
           size = 5, color = "black") +
  labs(
    x = "Pixel Values (Raster 1)",
    y = "Pixel Values (Raster 2)",
    title = "Comparison of Two Raster Maps"
  ) +
  theme_bw()

# Print the scatter plot
print(scatter_plot)

# Save the plot as a PNG file
ggsave("D:/MasterThesis/final_hs_data_folder_test/raster_comparison_plot.png",
       scatter_plot, dpi = 300, width = 10, height = 6)
