# clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(viridis)
library(lme4)
library(lmerTest)
library(writexl)
library(spdep)
library(units)
library(ape)
library(performance)
library(DHARMa)
library(see)

testsite_name <- 'AN_TJ_1'

img_file_path <- file.path('hs_raw',testsite_name)
hyperspectral <- rast(img_file_path)


cluster_shp <- 'cluster_info_shp/metrics_data_filtered.shp'
cluster_shp_data <- st_read(cluster_shp)

cluster_shp_data <- st_transform(cluster_shp_data, crs(hyperspectral))


AN_TJ_1 <- cluster_shp_data %>%
  filter(Testsit==testsite_name)


AN_TJ_1_pixel_values <- terra::extract(hyperspectral, vect(AN_TJ_1))


# Extract mean of pixel values within a 5-meter buffer around each point/polygon
AN_TJ_1_pixel_values <- terra::extract(
  hyperspectral,
  vect(AN_TJ_1),
  buffer = 5,           # buffer size in map units (e.g., meters)
  fun = mean,           # function to apply (mean, median, etc.)
  na.rm = TRUE,         # ignore NA values
  df = TRUE             # return as data frame
)




AN_TJ_1_combined <- bind_cols(AN_TJ_1, AN_TJ_1_pixel_values)

AN_TJ_1_combined_df <- AN_TJ_1_combined %>%
  select(-c(Tblnmbr,ID)) %>%
  st_drop_geometry() %>%  # Completely remove spatial attributes
  as.data.frame()         # Convert to a standard data frame


# Identify spectral band columns (exclude metadata)
band_columns <- setdiff(names(AN_TJ_1_combined_df), c("HbttTyp", "Testsit", "PltIdnt"))

# Rename band columns sequentially
names(AN_TJ_1_combined_df)[match(band_columns, names(AN_TJ_1_combined_df))] <- paste0("Band ", seq_along(band_columns))


# Convert spectral data into long format for plotting
AN_TJ_1_pixel_values_long <- AN_TJ_1_combined_df %>%
  pivot_longer(-c(HbttTyp, Testsit, PltIdnt),
               names_to = "Band",
               values_to = "Reflectance") %>%
  mutate(Wavelength = as.numeric(str_extract(Band, "\\d+"))) %>%
  mutate(Reflectance = Reflectance)  # Scaling reflectance if needed

# AN_TJ_1_pixel_values_long <- AN_TJ_1_pixel_values_long %>%
#   mutate(
#     Reflectance = case_when(
#       Wavelength >= 1340 & Wavelength <= 1445 ~ NA_real_,
#       Wavelength >= 1790 & Wavelength <= 1970 ~ NA_real_,
#       Wavelength >= 2450 ~ NA_real_,
#       TRUE ~ Reflectance
#     )
#   )
# 
# # Ensure wavelengths are sorted correctly
# unique_wavelengths_signature <- sort(unique(AN_TJ_1_pixel_values_long$Wavelength))
# 
# 
# # Generate spectral signature plot
# signature_plot <- ggplot(
#   AN_TJ_1_pixel_values_long,
#   aes(
#     x = Wavelength,
#     y = Reflectance,
#     color = HbttTyp,
#     group = PltIdnt
#   )
# ) +
#   geom_line() +
#   labs(x = "Wavelength (nm)", y = "Reflectance", title = paste0("Spectral Signatures ",testsite_name)) +
#   theme_minimal() +
#   scale_x_continuous(breaks = unique_wavelengths_signature) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_color_viridis(discrete = TRUE)
# 
# # Display the plot
# print(signature_plot)


################################################################################
################################################################################

AN_TJ_1_pixel_values_long <- AN_TJ_1_pixel_values_long %>%
  mutate(
    Reflectance = case_when(
      Wavelength >= 191 & Wavelength <= 211 ~ NA_real_,
      Wavelength >= 285 & Wavelength <= 321 ~ NA_real_,
      Wavelength >= 416 ~ NA_real_,
      TRUE ~ Reflectance
    )
  )

# Ensure wavelengths are sorted correctly
unique_wavelengths_signature <- sort(unique(AN_TJ_1_pixel_values_long$Wavelength))



# Generate spectral signature plot
signature_plot <- ggplot(
  AN_TJ_1_pixel_values_long,
  aes(
    x = Wavelength,
    y = Reflectance,
    color = HbttTyp,
    group = PltIdnt
  )
) +
  geom_line() +
  labs(x = "Wavelength (nm)", y = "Reflectance", title = paste0("Spectral signatures per habitat type ",testsite_name)) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(AN_TJ_1_pixel_values_long$Wavelength), max(AN_TJ_1_pixel_values_long$Wavelength), by = 5)) + # Example: breaks every 50 nm
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis(discrete = TRUE)

# Display the plot
print(signature_plot)

################################################################################
################################################################################



AN_TJ_1_pixel_values_summary <- AN_TJ_1_pixel_values_long %>%
  group_by(HbttTyp, Wavelength) %>%
  summarize(
    MeanReflectance = mean(Reflectance, na.rm = TRUE),
    MinReflectance = min(Reflectance, na.rm = TRUE),
    MaxReflectance = max(Reflectance, na.rm = TRUE),
    .groups = "drop"
  )


mean_signature_plot <- ggplot(
  AN_TJ_1_pixel_values_summary,
  aes(x = Wavelength, y = MeanReflectance, color = HbttTyp, fill = HbttTyp)
) +
  geom_ribbon(
    aes(ymin = MinReflectance, ymax = MaxReflectance),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "Wavelength (nm)",
    y = "Mean Reflectance",
    title = paste0("Mean spectral signatures per habitat type ", testsite_name)
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(min(AN_TJ_1_pixel_values_summary$Wavelength),
                 max(AN_TJ_1_pixel_values_summary$Wavelength), by = 5)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE)

# Display the plot
print(mean_signature_plot)



















# 
# # Compute mean reflectance per habitat type per wavelength
# AN_TJ_1_pixel_values_mean <- AN_TJ_1_pixel_values_long %>%
#   group_by(HbttTyp, Wavelength) %>%
#   summarize(MeanReflectance = mean(Reflectance, na.rm = TRUE), .groups = "drop")
# 
# # Generate the mean spectral signature plot
# mean_signature_plot <- ggplot(
#   AN_TJ_1_pixel_values_mean,
#   aes(
#     x = Wavelength,
#     y = MeanReflectance,
#     color = HbttTyp
#   )
# ) +
#   geom_line(linewidth = 1.2) +
#   labs(x = "Wavelength (nm)", y = "Mean Reflectance", title = paste0("Mean Spectral Signatures per Habitat type ",testsite_name)) +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(AN_TJ_1_pixel_values_mean$Wavelength), max(AN_TJ_1_pixel_values_mean$Wavelength), by = 5)) + # Example: breaks every 50 nm
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_color_viridis(discrete = TRUE)
# 
# # Display the plot
# print(mean_signature_plot)
# 


#
# Statistical Testing
#
AN_TJ_1_pixel_values_long_stat <- AN_TJ_1_pixel_values_long %>%
  dplyr::select(-c(Band,Testsit))  %>%
  mutate(Wavelength = as.factor(Wavelength)) %>%
  mutate(HbttTyp = as.factor(HbttTyp)) %>%
  mutate(PltIdnt = as.factor(PltIdnt))

str(AN_TJ_1_pixel_values_long_stat)

model_lm <- lm(Reflectance ~ HbttTyp  + PltIdnt + Wavelength + HbttTyp:Wavelength ,
               data = AN_TJ_1_pixel_values_long_stat)
anova(model_lm)
par(mfrow = c(2, 2))
plot(model_lm)






# 
# 
# # Convert metrics data to spatial points
# metrics_data_filtered_sf <- st_as_sf(metrics_data_filtered, coords = c("Longitude", "Latitude"), crs = crs(hyperspectral))
# 
# metrics_data_filtered_sf <- st_transform(metrics_data_filtered_sf, crs(hyperspectral))
# 
# # Extract pixel values at given locations
# pixel_values <- terra::extract(hyperspectral, vect(metrics_data_filtered_sf))
# 
# 
# print(st_crs(metrics_data_filtered_sf))
# 
# 
# 
# # Combine extracted values with the original data
# metrics_data_with_pixels <- cbind(metrics_data_filtered, pixel_values)
# 
# # Print the extracted pixel values
# print(metrics_data_with_pixels)
# 
# 
# plot(hyperspectral)
# plot(metrics_data_filtered_sf, add = TRUE, col = 'red')
# 
# 
# 
# 
# 
# 
# 
# shp_files_base_path <- 'biodivMapR_Example/01_DATA/S2A_T33NUD_Plots'
# 
# # hyperspectral_matrix <- as.matrix(rast('/Users/patrickangst/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/test/final_hs_data_folder/AN_TJ_1/image_rectified/ang20220711t002111_rfl_v2aa2_img_rectified'))
# 
# #
# # High Diversity
# #
# Forest_HighDiversity_file_path <- file.path(shp_files_base_path, 'Forest_HighDiversity.shp')
# Forest_HighDiversity <- st_read(Forest_HighDiversity_file_path)
# Forest_HighDiversity <- st_transform(Forest_HighDiversity, crs(hyperspectral))
# Forest_HighDiversity_pixel_values <- terra::extract(hyperspectral, vect(Forest_HighDiversity))
# Forest_HighDiversity_pixel_values_mean <- Forest_HighDiversity_pixel_values %>%
#   group_by(ID) %>%
#   summarise(across(everything(), mean, na.rm = TRUE)) %>%
#   mutate(VegetationGroup = 'High Diversity') %>%
#   mutate(PlotID = paste0(ID, '_High_Diversity')) %>%
#   dplyr::select(-ID)
# 
# 
# #
# # Low Diversity
# #
# Forest_LowDiversity_file_path <- file.path(shp_files_base_path, 'Forest_LowDiversity.shp')
# Forest_LowDiversity <- st_read(Forest_LowDiversity_file_path)
# Forest_LowDiversity <- st_transform(Forest_LowDiversity, crs(hyperspectral))
# Forest_LowDiversity_pixel_values <- terra::extract(hyperspectral, vect(Forest_LowDiversity))
# Forest_LowDiversity_pixel_values_mean <- Forest_LowDiversity_pixel_values %>%
#   group_by(ID) %>%
#   summarise(across(everything(), mean, na.rm = TRUE)) %>%
#   mutate(VegetationGroup = 'Low Diversity') %>%
#   mutate(PlotID = paste0(ID, '_Low_Diversity')) %>%
#   dplyr::select(-ID)
# 
# 
# #
# # Medium Diversity
# #
# Forest_MediumDiversity_file_path <- file.path(shp_files_base_path, 'Forest_MediumDiversity.shp')
# Forest_MediumDiversity <- st_read(Forest_MediumDiversity_file_path)
# Forest_MediumDiversity <- st_transform(Forest_MediumDiversity, crs(hyperspectral))
# Forest_MediumDiversity_pixel_values <- terra::extract(hyperspectral, vect(Forest_MediumDiversity))
# Forest_MediumDiversity_pixel_values_mean <- Forest_MediumDiversity_pixel_values %>%
#   group_by(ID) %>%
#   summarise(across(everything(), mean, na.rm = TRUE)) %>%
#   mutate(VegetationGroup = 'Medium Diversity') %>%
#   mutate(PlotID = paste0(ID, '_Medium_Diversity')) %>%
#   dplyr::select(-ID)
# 
# 
# #
# # Low Vegetation
# #
# LowVegetation_file_path <- file.path(shp_files_base_path, 'LowVegetation.shp')
# LowVegetation <- st_read(LowVegetation_file_path)
# LowVegetation <- st_transform(LowVegetation, crs(hyperspectral))
# LowVegetation_pixel_values <- terra::extract(hyperspectral, vect(LowVegetation))
# LowVegetation_pixel_values_mean <- LowVegetation_pixel_values %>%
#   group_by(ID) %>%
#   summarise(across(everything(), mean, na.rm = TRUE)) %>%
#   mutate(VegetationGroup = 'Low Vegetation') %>%
#   mutate(PlotID = paste0(ID, '_Low_Vegetation')) %>%
#   dplyr::select(-ID)
# 
# 
# combined_tbl <- rbind(
#   Forest_HighDiversity_pixel_values_mean,
#   Forest_LowDiversity_pixel_values_mean,
#   Forest_MediumDiversity_pixel_values_mean,
#   LowVegetation_pixel_values_mean
# )
# 
# 
# # Pivot the data
# spectral_long <- combined_tbl %>%
#   pivot_longer(-c(VegetationGroup, PlotID),
#                names_to = "Band",
#                values_to = "Reflectance") %>%
#   mutate(Wavelength = as.numeric(str_extract(Band, "\\d+"))) %>%
#   mutate(Wavelength = str_extract(Band, "(?<=\\()\\d+\\.\\d+(?= Nanometers)") %>%
#            as.numeric()) %>%
#   mutate(Reflectance = Reflectance / 10000)
# 
# 
# unique_wavelengths_signature_2 <- sort(unique(spectral_long$Wavelength))
# 
# signature_2 <- ggplot(
#   spectral_long,
#   aes(
#     x = Wavelength,
#     y = Reflectance,
#     color = VegetationGroup,
#     group = PlotID
#   )
# ) +
#   geom_line() +
#   labs(x = "Wavelength (nm)", y = "Reflectance", title = "Spectral Signatures") +
#   theme_minimal() +
#   scale_x_continuous(breaks = unique_wavelengths_signature_2) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_color_manual(values = c("green", "red", "blue", "purple"))
# 
# plot(signature_2)
# 
# filename_spectral_signature <- '~/SynologyDrive/UZH/Quantitative Environmental Sciences/Studium/EEE311/Final_Project/biodivMapR-master/biodivMapR_Example/01_DATA/spectral_png/spectral_signature.png'
# 
# ggsave(
#   filename = filename_spectral_signature,
#   plot = signature_2,
#   width = 12,
#   height = 6,
#   dpi = 400
# )
# 
# 
# 
# # Plot the mean reflectance
# mean_spectral_long <- spectral_long %>%
#   group_by(VegetationGroup, Wavelength) %>%
#   summarise(mean_reflectance = mean(Reflectance, na.rm = TRUE))
# 
# unique_wavelengths <- sort(unique(mean_spectral_long$Wavelength))
# 
# 
# signature_mean <- ggplot(mean_spectral_long,
#                          aes(x = Wavelength, y = mean_reflectance, color = VegetationGroup)) +
#   geom_line() +
#   labs(x = "Wavelength (nm)", y = "Mean Reflectance", title = "Mean Spectral Signatures by Vegetation Group") +
#   theme_minimal() +
#   scale_x_continuous(breaks = unique_wavelengths) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_color_manual(values = c("green", "red", "blue", "purple"))
# 
# plot(signature_mean)
# 
# filename_spectral_signature_mean <- '~/SynologyDrive/UZH/Quantitative Environmental Sciences/Studium/EEE311/Final_Project/biodivMapR-master/biodivMapR_Example/01_DATA/spectral_png/spectral_signature_mean.png'
# 
# ggsave(
#   filename = filename_spectral_signature_mean,
#   plot = signature_mean,
#   width = 12,
#   height = 6,
#   dpi = 400
# )
# 
# 
# #
# # Statistical Testing
# #
# spectral_long <- spectral_long %>%
#   dplyr::select(-Band)  %>%
#   mutate(Wavelength = as.factor(Wavelength)) %>%
#   mutate(VegetationGroup = as.factor(VegetationGroup)) %>%
#   mutate(PlotID = as.factor(PlotID))
# 
# 
# model_lm <- lm(Reflectance ~ VegetationGroup  + PlotID + Wavelength + VegetationGroup:Wavelength ,
#                data = spectral_long)
# anova(model_lm)
# par(mfrow = c(2, 2))
# plot(model_lm)
# 
# 
# 
# model_lmer <- lmer(Reflectance ~ VegetationGroup * Wavelength + (1 |
#                                                                    PlotID), data = spectral_long)
# # summary(model_2)
# anova(model_lmer, type = "I")
# par(mfrow = c(2, 2))
# plot(model_lmer)
# 
# # Extract residuals and fitted values
# residuals <- resid(model_lmer)
# fitted <- fitted(model_lmer)
# 
# # Residuals vs Fitted Plot
# plot(fitted, residuals, main = "Residuals vs Fitted")
# abline(h = 0, col = "red")
# 
# # Simulate residuals for diagnostics
# sim_res <- simulateResiduals(fittedModel = model_lmer)
# 
# # Plot diagnostics
# plot(sim_res)
# 
# 
# # Check random effects
# check_model(model_lmer)
# 
# 
# shp_files_base_path <- 'biodivMapR_Example/01_DATA/S2A_T33NUD_Plots'
# 
# shapefile_names <- c(
#   "Forest_HighDiversity",
#   "Forest_LowDiversity",
#   "Forest_MediumDiversity",
#   "LowVegetation"
# )
# 
# combined_tbl <- data.frame()
# 
# for (shp_name in shapefile_names) {
#   file_path <- file.path(shp_files_base_path, paste0(shp_name, ".shp"))
#   shapefile <- st_read(file_path, quiet = TRUE)
#   shapefile <- st_transform(shapefile, crs(hyperspectral, proj = TRUE))
#   
#   # Extract pixel values
#   pixel_values <- terra::extract(hyperspectral, vect(shapefile))
#   
#   # Calculate means
#   pixel_values_mean <- pixel_values %>%
#     group_by(ID) %>%
#     summarise(across(everything(), ~ if (all(is.na(.)))
#       NA_real_
#       else
#         mean(., na.rm = TRUE))) %>%
#     mutate(VegetationGroup = str_replace(shp_name, "Forest_", "")) %>%
#     mutate(PlotID = paste0(ID, '_', VegetationGroup)) %>%
#     dplyr::select(-ID)
#   
#   combined_tbl <- rbind(combined_tbl, pixel_values_mean)
# }
# 
# # --- Pivot Data ---
# spectral_long <- combined_tbl %>%
#   pivot_longer(-c(VegetationGroup, PlotID),
#                names_to = "Band",
#                values_to = "Reflectance") %>%
#   mutate(Wavelength = as.numeric(str_extract(Band, "\\d+"))) %>%
#   mutate(Wavelength = str_extract(Band, "(?<=\\()\\d+\\.\\d+(?= Nanometers)") %>%
#            as.numeric()) %>%
#   mutate(Reflectance = Reflectance / 10000)
# 
# # Spatial Autocorrelation Analysis ---
# 
# # Create a Spatial Object of Plot Centroids
# all_plots <- st_as_sf(do.call(rbind, lapply(shapefile_names, function(shp_name) {
#   file_path <- file.path(shp_files_base_path, paste0(shp_name, ".shp"))
#   shapefile <- st_read(file_path, quiet = TRUE)
#   shapefile <- st_transform(shapefile, crs(hyperspectral, proj = TRUE))
#   shapefile$VegetationGroup <- str_replace(shp_name, "Forest_", "")
#   shapefile$PlotID = paste0(shapefile$ID, '_', shapefile$VegetationGroup)
#   return(shapefile)
# })))
# 
# has_multipart <- any(st_geometry_type(all_plots) %in% c("MULTIPOLYGON", "GEOMETRYCOLLECTION"))
# 
# if (has_multipart) {
#   warning("Your data contains multi-part geometries. Ensure attributes are consistent before suppressing warnings.")
#   plot_centroids <- st_centroid(all_plots)
# } else {
#   plot_centroids <- suppressWarnings(st_centroid(all_plots))
#   cat("No multi-part geometries found. st_centroid warning suppressed.\n")
# }
# 
# plot_centroids <- plot_centroids %>%
#   mutate(PlotID = paste0(id, PlotID))
# 
# # Create a Spatial Weights Matrix (Inverse Distance)
# distances <- st_distance(plot_centroids)
# 
# # Convert distance matrix to numeric
# distances_numeric <- as.matrix(distances)
# units(distances_numeric) <- NULL  # Drop units
# 
# # Compute inverse distance weights, avoiding division by zero
# distances_inv <- 1 / (distances_numeric + 1e-6)
# diag(distances_inv) <- 0  # Set diagonal to zero
# 
# # Test for Spatial Autocorrelation in spectral_long
# wavelengths <- sort(unique(spectral_long$Wavelength))
# moran_results <- data.frame(Wavelength = wavelengths, Moran_I = NA, P_value = NA)
# 
# str(plot_centroids)
# 
# merged_df <- merge(plot_centroids,spectral_long,by = "PlotID")
# 
# merged_df <- merged_df %>%
#   rename(VegetationGroup = VegetationGroup.x) %>%
#   select(-VegetationGroup.y)
# 
# merged_df <- cbind(merged_df, st_coordinates(merged_df))
# 
# wavelengths <- sort(unique(merged_df$Wavelength))
# moran_results <- data.frame(Wavelength = wavelengths,
#                             Moran_I = NA,
#                             P_value = NA)
# 
# for (i in seq_along(wavelengths)) {
#   wavelength_data <- merged_df[merged_df$Wavelength == wavelengths[i], ]
#   
#   if (any(is.na(wavelength_data$Reflectance))) {
#     warning(paste("NA found in reflectance at wavelength:", wavelengths[i]))
#     print(wavelength_data$Reflectance)
#   }
#   
#   if (sum(!is.na(wavelength_data$Reflectance)) > 1) {
#     #print("NA found Hugo")
#     # Calculate Moran's I
#     moran_result <- tryCatch({
#       Moran.I(wavelength_data$Reflectance, distances_inv, na.rm = TRUE)
#     }, error = function(e) {
#       list(observed = NA, p.value = NA)
#     })
#     moran_results$Moran_I[i] <- moran_result$observed
#     moran_results$P_value[i] <- moran_result$p.value
#   } else {
#     moran_results$Moran_I[i] <- NA
#     moran_results$P_value[i] <- NA
#   }
# }
# print(moran_results)
# 
