# Clear workspace and graphics
rm(list = ls())
graphics.off()

# Load the multi-band image
library(terra)

rectified_image_file_path <- "data/rectified/ang20190712t231624_rfl_v2v2_img_rectified"
multi_band_image <- rast(rectified_image_file_path)

# Extract individual bands by their index (assuming known order)
R705 <- multi_band_image[[1]]  # 705 nm
R750 <- multi_band_image[[2]]  # 750 nm
R685 <- multi_band_image[[3]]  # 685 nm
R900 <- multi_band_image[[4]]  # 900 nm

# Replace all -9999 values with NA
R705[R705 == -9999] <- NA
R750[R750 == -9999] <- NA
R685[R685 == -9999] <- NA
R900[R900 == -9999] <- NA


# Calculate SR (Simple Ratio)
sr_array <- R750 / R705

# Calculate SAVI with L = 0.5
L <- 0.5
savi_array <- L * (R900 - R685) / (R900 + R685 + L)

# Calculate NDVI for plotting (optional)
ndvi_array <- (R900 - R685) / (R900 + R685)

# Convert rasters to data frames for ggplot2 plotting
sr_df <- as.data.frame(sr_array, xy = TRUE, na.rm = TRUE)
savi_df <- as.data.frame(savi_array, xy = TRUE, na.rm = TRUE)
ndvi_df <- as.data.frame(ndvi_array, xy = TRUE, na.rm = TRUE)

# Plot using ggplot2
library(ggplot2)
library(gridExtra)

p1 <- ggplot(ndvi_df, aes(x = x, y = y, fill = layer)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "NDVI", fill = "Value") +
  theme_minimal()

p2 <- ggplot(sr_df, aes(x = x, y = y, fill = layer)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "SR (R750/R705)", fill = "Value") +
  theme_minimal()

p3 <- ggplot(savi_df, aes(x = x, y = y, fill = layer)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "SAVI", fill = "Value") +
  theme_minimal()

grid.arrange(p1, p2, p3, ncol = 3)
