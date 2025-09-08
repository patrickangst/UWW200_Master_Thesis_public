rm(list = ls(all = TRUE))
gc()
graphics.off()


library(terra)
library(torch)

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)


cutline_shapefile_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/Cutline/TemporalTestCutline.shp'
raw_image_file_1_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/ang20220710t005818rfl/ang20220710t005818_rfl_v2aa2_img'
rectified_image_file_1_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/ang20220710t005818rfl/ang20220710t005818_rfl_v2aa2_img_rectified'

raw_image_file_2_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/ang20220709t192338_rfl_v2aa2/ang20220709t192338_rfl_v2aa2_img'
rectified_image_file_2_path  <-  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/ang20220709t192338_rfl_v2aa2/ang20220709t192338_rfl_v2aa2_img_rectified'

tif_1 <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/ang20220710t005818rfl/ang20220710t005818_rfl_v2aa2_img_rectified_nornalized.tif'
tif_2 <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/ang20220709t192338_rfl_v2aa2/ang20220709t192338_rfl_v2aa2_img_rectified_nornalized.tif'


################################################################################
################################################################################
################################################################################
################################################################################


# gdal_command_rectify_1 <- sprintf(
#   "gdalwarp -cutline %s -crop_to_cutline -of ENVI -co INTERLEAVE=BIL -dstnodata -9999 %s %s",
#   cutline_shapefile_path,
#   raw_image_file_1_path,
#   rectified_image_file_1_path
# )
# system(gdal_command_rectify_1)
#
# gdal_command_rectify_2 <- sprintf(
#   "gdalwarp -cutline %s -crop_to_cutline -of ENVI -co INTERLEAVE=BIL -dstnodata -9999 %s %s",
#   cutline_shapefile_path,
#   raw_image_file_2_path,
#   rectified_image_file_2_path
# )
# system(gdal_command_rectify_2)

################################################################################
################################################################################
################################################################################
################################################################################


# # Load hyperspectral image
# hsi_rast <- rast(rectified_image_file_1_path)
#
# # 2. Convert to array [rows, cols, bands]
# hsi_array <- as.array(hsi_rast)   # should be [424, 421, 425]
#
# # 3. Convert to torch tensor
# hsi <- torch_tensor(hsi_array, dtype = torch_float())
#
# # 4a. Per-pixel Min-Max normalization
# mins <- hsi$amin(dim = 3, keepdim = TRUE)
# maxs <- hsi$amax(dim = 3, keepdim = TRUE)
# hsi_norm <- (hsi - mins) / (maxs - mins + 1e-8)
#
# # (Alternative: L2 normalization)
# # norms <- hsi$norm(p = 2, dim = 3, keepdim = TRUE)
# # hsi_norm <- hsi / (norms + 1e-8)
#
# # 5. Convert back to array
# hsi_norm_array <- as.array(hsi_norm)
#
# # 6. Create new SpatRaster
# hsi_norm_rast <- rast(hsi_norm_array,
#                       crs = crs(hsi_rast),
#                       extent = ext(hsi_rast))
#
# # 7. Restore band names (wavelengths)
# names(hsi_norm_rast) <- names(hsi_rast)
#
# # 8. Save as GeoTIFF
# writeRaster(hsi_norm_rast, tif_1, overwrite = TRUE)



################################################################################
################################################################################
################################################################################
################################################################################

# Hyperspectral images (assuming ENVI, GeoTIFF, or similar formats)
hs1 <- rast(rectified_image_file_1_path)
hs2 <- rast(rectified_image_file_2_path)

# Shapefile with plot locations
plots <- st_read('~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/PointLocation/PlotLocations.shp')

plots <- st_transform(plots, crs(hs1))

# --- Ensure CRS is consistent ---
if (!st_crs(plots) == crs(hs1)) {
  plots <- st_transform(plots, crs(hs1))
}

# --- Create 5 m buffer around plots ---
plots_buffer <- st_buffer(plots, dist = 5)

# --- Function to extract reflectance data from one hyperspectral image ---
extract_reflectance <- function(hs_raster, plots_buffer, source_name) {

  # Extract all pixel values in each buffer
  extracted <- terra::extract(hs_raster, plots_buffer, df = TRUE)

  # Pivot all band columns (everything except ID)
  reflectance_long <- extracted %>%
    tidyr::pivot_longer(
      cols = -ID,
      names_to = "Band",
      values_to = "Reflectance"
    ) %>%
    mutate(
      # Extract numeric part from band name ("Band_1" -> 1, "Band_23" -> 23)
      Wavelength = as.integer(sub(" .*", "", Band)),
      #Wavelength = as.numeric(gsub("[^0-9]", "", Band)),
      Source = source_name
    )

  # Add plot metadata (join back by ID)
  reflectance_long <- reflectance_long %>%
    left_join(plots %>% st_drop_geometry() %>% mutate(ID = row_number()),
              by = "ID")

  reflectance_long_cleaned <- reflectance_long %>%
    mutate(
      Reflectance = case_when(
        Wavelength >= 0 & Wavelength <= 442 ~ NA_real_,
        Wavelength >= 1329 & Wavelength <= 1429 ~ NA_real_,
        Wavelength >= 1800 & Wavelength <= 1980 ~ NA_real_,
        Wavelength >= 2456 ~ NA_real_,
        TRUE ~ Reflectance
      )
    )

  return(reflectance_long_cleaned)
}

# --- Extract from both images separately ---
# debug(extract_reflectance)
reflectance_hs1 <- extract_reflectance(hs1, plots_buffer, "HS1")
reflectance_hs2 <- extract_reflectance(hs2, plots_buffer, "HS2")

# --- Combine both ---
reflectance_all <- bind_rows(reflectance_hs1, reflectance_hs2)

# --- Aggregate statistics (mean, min, max reflectance) per plot ---
reflectance_summary <- reflectance_all %>%
  group_by(Source, ID, Wavelength) %>%
  summarize(
    MeanReflectance = mean(Reflectance, na.rm = TRUE),
    MinReflectance  = min(Reflectance, na.rm = TRUE),
    MaxReflectance  = max(Reflectance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    MeanReflectance = ifelse(is.infinite(MeanReflectance), NaN, MeanReflectance),
    MinReflectance = ifelse(is.infinite(MinReflectance), NaN, MinReflectance),
    MaxReflectance = ifelse(is.infinite(MaxReflectance), NaN, MaxReflectance)
  )



# =========================================================
# Define spectral regions (nm ranges)
# =========================================================
band_data <- data.frame(
  Lower = c(375, 450, 485, 500, 565, 590, 625, 740, 1100),
  Upper = c(450, 485, 500, 565, 590, 625, 740, 1100, 2500),
  FillCategory = c(
    "Violet",
    "Blue",
    "Cyan",
    "Green",
    "Yellow",
    "Orange",
    "Red",
    "Near-Infrared",
    "Shortwave-Infrared"
  )
)

# =========================================================
# Faceted spectral signatures by plot with background bands
# =========================================================
ggplot() +
  # 1. Background spectral regions (repeat across facets)
  geom_rect(
    data = band_data,
    aes(
      xmin = Lower, xmax = Upper,
      ymin = -Inf, ymax = Inf,
      fill = FillCategory
    ),
    alpha = 0.15,
    inherit.aes = FALSE
  ) +

  # 2. Plot ribbons for reflectance ranges
  geom_ribbon(
    data = reflectance_summary,
    aes(
      x = Wavelength,
      ymin = MinReflectance,
      ymax = MaxReflectance,
      fill = Source,
      group = Source
    ),
    alpha = 0.25,
    color = NA
  ) +

  # 3. Plot reflectance mean line
  geom_line(
    data = reflectance_summary,
    aes(
      x = Wavelength,
      y = MeanReflectance,
      color = Source,
      group = Source
    ),
    linewidth = 0.5
  ) +

  # 4. Facet per plot
  facet_wrap(~ ID, scales = "free_y") +

  # 5. Labels and theme
  labs(
    x = "Wavelength [nm]",
    y = "Reflectance",
    title = "Spectral signatures by plot (5 m buffer)",
    subtitle = "Comparison between HS1 and HS2"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 6. Color/fill scales
  scale_color_viridis(discrete = TRUE, name = "Image") +
  scale_fill_manual(
    name = "Spectral Region",
    values = c(
      "Violet" = "violet",
      "Blue" = "blue",
      "Cyan" = "cyan",
      "Green" = "green",
      "Yellow" = "yellow",
      "Orange" = "darkorange",
      "Red" = "red",
      "Near-Infrared" = "darkgrey",
      "Shortwave-Infrared" = "indianred"
    ),
    guide = guide_legend(order = 2)
  ) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  )


################################################################################
################################################################################
################################################################################
################################################################################

reflectance_norm <- reflectance_summary %>%
  group_by(ID, Source) %>%
  mutate(
    NormReflectance = (MeanReflectance - min(MeanReflectance, na.rm = TRUE)) /
      (max(MeanReflectance, na.rm = TRUE) - min(MeanReflectance, na.rm = TRUE))
  )

similarity <- reflectance_summary %>%
  select(ID, Source, Wavelength, MeanReflectance) %>%
  pivot_wider(names_from = Source, values_from = MeanReflectance) %>%
  group_by(ID) %>%
  summarise(
    correlation = cor(HS1, HS2, use = "complete.obs")
  )

print(similarity)


ggplot() +
  # Background spectral regions
  geom_rect(
    data = band_data,
    aes(xmin = Lower, xmax = Upper, ymin = -Inf, ymax = Inf, fill = FillCategory),
    alpha = 0.15,
    inherit.aes = FALSE
  ) +

  # Normalized reflectance lines
  geom_line(
    data = reflectance_norm,
    aes(x = Wavelength, y = NormReflectance, color = Source, group = Source),
    linewidth = 0.5
  ) +

  # Facet per plot
  facet_wrap(~ ID, scales = "free_y") +

  labs(
    x = "Wavelength [nm]",
    y = "Normalized Reflectance (0–1)",
    title = "Normalized spectral signatures by plot (5 m buffer)",
    subtitle = "HS1 vs HS2 (shape comparison)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  scale_color_viridis(discrete = TRUE, name = "Image") +
  scale_fill_manual(
    name = "Spectral Region",
    values = c(
      "Violet" = "violet",
      "Blue" = "blue",
      "Cyan" = "cyan",
      "Green" = "green",
      "Yellow" = "yellow",
      "Orange" = "darkorange",
      "Red" = "red",
      "Near-Infrared" = "darkgrey",
      "Shortwave-Infrared" = "indianred"
    ),
    guide = guide_legend(order = 2)
  ) +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2))




















rm(list = ls(all = TRUE))
gc()
graphics.off()


library(terra)
library(torch)

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)

cutline_shapefile_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/Cutline/TemporalTestCutline.shp'
raw_image_file_1_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/ang20220710t005818rfl/ang20220710t005818_rfl_v2aa2_img'
rectified_image_file_1_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/ang20220710t005818rfl/ang20220710t005818_rfl_v2aa2_img_rectified'

raw_image_file_2_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/ang20220709t192338_rfl_v2aa2/ang20220709t192338_rfl_v2aa2_img'
rectified_image_file_2_path  <-  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/ang20220709t192338_rfl_v2aa2/ang20220709t192338_rfl_v2aa2_img_rectified'

tif_1 <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/ang20220710t005818rfl/ang20220710t005818_rfl_v2aa2_img_rectified_nornalized.tif'
tif_2 <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/ang20220709t192338_rfl_v2aa2/ang20220709t192338_rfl_v2aa2_img_rectified_nornalized.tif'


# Hyperspectral images (assuming ENVI, GeoTIFF, or similar formats)
hs1 <- rast(rectified_image_file_1_path)
hs2 <- rast(rectified_image_file_2_path)

# Shapefile with plot locations
plots <- st_read('~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/TemporalTest/PointLocation/PlotLocations.shp')

plots <- st_transform(plots, crs(hs1))

# --- Ensure CRS is consistent ---
if (!st_crs(plots) == crs(hs1)) {
  plots <- st_transform(plots, crs(hs1))
}

# --- Create 5 m buffer around plots ---
plots_buffer <- st_buffer(plots, dist = 5)

extract_reflectance <- function(hs_raster, plot_points, source_name) {
  extracted <- terra::extract(hs_raster, plot_points, df = TRUE)

  reflectance_long <- extracted %>%
    pivot_longer(
      cols = -ID,
      names_to = "Band",
      values_to = "Reflectance"
    ) %>%
    mutate(
      Wavelength = as.integer(sub(" .*", "", Band)),
      Source = source_name
    ) %>%
    left_join(plot_points %>% st_drop_geometry() %>% mutate(ID = row_number()), by = "ID") %>%
    mutate(
      Reflectance = case_when(
        Wavelength >= 0 & Wavelength <= 442 ~ NA_real_,
        Wavelength >= 1329 & Wavelength <= 1429 ~ NA_real_,
        Wavelength >= 1800 & Wavelength <= 1980 ~ NA_real_,
        Wavelength >= 2456 ~ NA_real_,
        TRUE ~ Reflectance
      )
    )

  return(reflectance_long)
}

# Remove buffer creation
# plots_buffer <- st_buffer(plots, dist = 5)

# Use plots directly
reflectance_hs1 <- extract_reflectance(hs1, plots, "HS1")
reflectance_hs2 <- extract_reflectance(hs2, plots, "HS2")


# --- Combine both ---
reflectance_all <- bind_rows(reflectance_hs1, reflectance_hs2)

# --- Aggregate statistics (mean, min, max reflectance) per plot ---
reflectance_summary <- reflectance_all %>%
  group_by(Source, ID, Wavelength) %>%
  summarize(
    MeanReflectance = mean(Reflectance, na.rm = TRUE),
    MinReflectance  = min(Reflectance, na.rm = TRUE),
    MaxReflectance  = max(Reflectance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    MeanReflectance = ifelse(is.infinite(MeanReflectance), NaN, MeanReflectance),
    MinReflectance = ifelse(is.infinite(MinReflectance), NaN, MinReflectance),
    MaxReflectance = ifelse(is.infinite(MaxReflectance), NaN, MaxReflectance)
  )



# =========================================================
# Define spectral regions (nm ranges)
# =========================================================
band_data <- data.frame(
  Lower = c(375, 450, 485, 500, 565, 590, 625, 740, 1100),
  Upper = c(450, 485, 500, 565, 590, 625, 740, 1100, 2500),
  FillCategory = c(
    "Violet",
    "Blue",
    "Cyan",
    "Green",
    "Yellow",
    "Orange",
    "Red",
    "Near-Infrared",
    "Shortwave-Infrared"
  )
)

# =========================================================
# Faceted spectral signatures by plot with background bands
# =========================================================
ggplot() +
  # 1. Background spectral regions (repeat across facets)
  geom_rect(
    data = band_data,
    aes(
      xmin = Lower, xmax = Upper,
      ymin = -Inf, ymax = Inf,
      fill = FillCategory
    ),
    alpha = 0.15,
    inherit.aes = FALSE
  ) +

  # 2. Plot ribbons for reflectance ranges
  geom_ribbon(
    data = reflectance_summary,
    aes(
      x = Wavelength,
      ymin = MinReflectance,
      ymax = MaxReflectance,
      fill = Source,
      group = Source
    ),
    alpha = 0.25,
    color = NA
  ) +

  # 3. Plot reflectance mean line
  geom_line(
    data = reflectance_summary,
    aes(
      x = Wavelength,
      y = MeanReflectance,
      color = Source,
      group = Source
    ),
    linewidth = 0.5
  ) +

  # 4. Facet per plot
  facet_wrap(~ ID, scales = "free_y") +

  # 5. Labels and theme
  labs(
    x = "Wavelength [nm]",
    y = "Reflectance",
    title = "Spectral signatures by plot (5 m buffer)",
    subtitle = "Comparison between HS1 and HS2"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 6. Color/fill scales
  scale_color_viridis(discrete = TRUE, name = "Image") +
  scale_fill_manual(
    name = "Spectral Region",
    values = c(
      "Violet" = "violet",
      "Blue" = "blue",
      "Cyan" = "cyan",
      "Green" = "green",
      "Yellow" = "yellow",
      "Orange" = "darkorange",
      "Red" = "red",
      "Near-Infrared" = "darkgrey",
      "Shortwave-Infrared" = "indianred"
    ),
    guide = guide_legend(order = 2)
  ) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  )

reflectance_norm <- reflectance_summary %>%
  group_by(ID, Source) %>%
  mutate(
    NormReflectance = (MeanReflectance - min(MeanReflectance, na.rm = TRUE)) /
      (max(MeanReflectance, na.rm = TRUE) - min(MeanReflectance, na.rm = TRUE))
  )

similarity <- reflectance_summary %>%
  select(ID, Source, Wavelength, MeanReflectance) %>%
  pivot_wider(names_from = Source, values_from = MeanReflectance) %>%
  group_by(ID) %>%
  summarise(
    correlation = cor(HS1, HS2, use = "complete.obs")
  )

print(similarity)


ggplot() +
  # Background spectral regions
  geom_rect(
    data = band_data,
    aes(xmin = Lower, xmax = Upper, ymin = -Inf, ymax = Inf, fill = FillCategory),
    alpha = 0.15,
    inherit.aes = FALSE
  ) +

  # Normalized reflectance lines
  geom_line(
    data = reflectance_norm,
    aes(x = Wavelength, y = NormReflectance, color = Source, group = Source),
    linewidth = 0.5
  ) +

  # Facet per plot
  facet_wrap(~ ID, scales = "free_y") +

  labs(
    x = "Wavelength [nm]",
    y = "Normalized Reflectance (0–1)",
    title = "Normalized spectral signatures by plot (5 m buffer)",
    subtitle = "HS1 vs HS2 (shape comparison)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  scale_color_viridis(discrete = TRUE, name = "Image") +
  scale_fill_manual(
    name = "Spectral Region",
    values = c(
      "Violet" = "violet",
      "Blue" = "blue",
      "Cyan" = "cyan",
      "Green" = "green",
      "Yellow" = "yellow",
      "Orange" = "darkorange",
      "Red" = "red",
      "Near-Infrared" = "darkgrey",
      "Shortwave-Infrared" = "indianred"
    ),
    guide = guide_legend(order = 2)
  ) +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2))



