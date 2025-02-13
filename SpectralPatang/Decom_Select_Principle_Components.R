rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load necessary libraries
library(terra)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(RColorBrewer)

# Set the directory containing the GeoTIFFs
tiff_folder <- "D:/MasterThesis/01_principle_components"

# Set the directory containing the PNGs
png_folder <- "D:/MasterThesis/02_principle_components_png"

# Ensure the PNG folder exists
if (!dir.exists(png_folder)) {
  dir.create(png_folder, recursive = TRUE)
}

# List all .tif files in the folder
tiff_files <- list.files(tiff_folder, pattern = "\\.tif$", full.names = TRUE)

# Function to save each band separately inside its own folder
save_tiff_bands <- function(tiff_path) {
  # Load the raster stack
  raster_stack <- rast(tiff_path)

  # Get the filename without extension
  filename_base <- tools::file_path_sans_ext(basename(tiff_path))

  # Create a folder for this specific image inside png_folder
  image_folder <- file.path(png_folder, filename_base)
  if (!dir.exists(image_folder)) {
    dir.create(image_folder, recursive = TRUE)
  }

  # Get the first 30 bands (or max available)
  bands_to_plot <- min(30, nlyr(raster_stack))
  raster_subset <- raster_stack[[1:bands_to_plot]]

  # Loop through each band and save individually
  for (i in 1:bands_to_plot) {
    r <- raster_subset[[i]]
    df <- as.data.frame(r, xy = TRUE)
    colnames(df) <- c("x", "y", "value")

    # Create plot
    p <- ggplot(df, aes(x = x, y = y, fill = value)) +
      geom_raster() +
      scale_fill_gradientn(colors = brewer.pal(11, "Spectral")) +
      theme_minimal(base_size = 16) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "right",
            strip.text = element_text(size = 14, face = "bold")) +
      labs(title = paste("Band", i, "of", filename_base))

    # Save each band in the specific image folder
    output_filename <- file.path(image_folder, paste0(filename_base, "_band_", i, ".png"))
    ggsave(filename = output_filename, plot = p, width = 6, height = 6, dpi = 400)

    print(p)  # Display the plot
  }
}

# Loop through all GeoTIFF files and generate plots
for (tiff in tiff_files) {
  save_tiff_bands(tiff)
}
