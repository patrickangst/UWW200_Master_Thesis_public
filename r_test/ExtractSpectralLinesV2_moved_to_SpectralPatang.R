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
x_axis_intervall <- 25

img_file_path <- file.path('hs_reflectance_exctraction', testsite_name)
hyperspectral <- rast(img_file_path)


cluster_shp <- 'cluster_info_shp/metrics_data_filtered.shp'
cluster_shp_data <- st_read(cluster_shp)

cluster_shp_data <- st_transform(cluster_shp_data, crs(hyperspectral))


TEST_SITE_NAME <- cluster_shp_data %>%
  filter(Testsit == testsite_name)


TEST_SITE_NAME_pixel_values <- terra::extract(hyperspectral, vect(TEST_SITE_NAME))


# Extract mean of pixel values within a 5-meter buffer around each point/polygon
TEST_SITE_NAME_pixel_values <- terra::extract(
  hyperspectral,
  vect(TEST_SITE_NAME), # 
  buffer = 5,
  # buffer size in map units (e.g., meters)
  fun = mean,
  # function to apply (mean, median, etc.)
  na.rm = TRUE,
  # ignore NA values
  df = TRUE             # return as data frame
)


TEST_SITE_NAME_combined <- bind_cols(TEST_SITE_NAME, TEST_SITE_NAME_pixel_values)

TEST_SITE_NAME_combined_df <- TEST_SITE_NAME_combined %>%
  select(-c(Tblnmbr, ID)) %>%
  st_drop_geometry() %>%  # Completely remove spatial attributes
  as.data.frame()         # Convert to a standard data frame


# Identify spectral band columns (exclude metadata)
band_columns <- setdiff(names(TEST_SITE_NAME_combined_df),
                        c("HbttTyp", "Testsit", "PltIdnt"))

rounded_wavelength <- str_extract(band_columns, "\\d+\\.\\d+") %>%
  as.numeric() %>%
  round()

band_info <- data.frame(BandNr = seq_along(rounded_wavelength),
                        Wavelength = rounded_wavelength)

# Rename band columns sequentially
names(TEST_SITE_NAME_combined_df)[match(band_columns, names(TEST_SITE_NAME_combined_df))] <- paste0("Band ", seq_along(band_columns))


# Convert spectral data into long format for plotting
TEST_SITE_NAME_pixel_values_long <- TEST_SITE_NAME_combined_df %>%
  pivot_longer(-c(HbttTyp, Testsit, PltIdnt),
               names_to = "Band",
               values_to = "Reflectance") %>%
  mutate(BandNr = as.numeric(str_extract(Band, "\\d+"))) %>%
  mutate(Reflectance = Reflectance) %>%
  left_join(band_info, by = "BandNr")  # Adds Wavelength column

################################################################################
################################################################################
# 
# TEST_SITE_NAME_pixel_values_long <- TEST_SITE_NAME_pixel_values_long %>%
#   mutate(
#     Reflectance = case_when(
#       Wavelength >= 191 & Wavelength <= 211 ~ NA_real_,
#       Wavelength >= 285 & Wavelength <= 321 ~ NA_real_,
#       Wavelength >= 416 ~ NA_real_,
#       TRUE ~ Reflectance
#     )
#   )


TEST_SITE_NAME_pixel_values_long <- TEST_SITE_NAME_pixel_values_long %>%
  mutate(
    Reflectance = case_when(
      Wavelength >= 1329 & Wavelength <= 1429 ~ NA_real_,
      Wavelength >= 1800 & Wavelength <= 1980 ~ NA_real_,
      Wavelength >= 2456 ~ NA_real_,
      TRUE ~ Reflectance
    )
  )

# Ensure wavelengths are sorted correctly
unique_wavelengths_signature <- sort(unique(TEST_SITE_NAME_pixel_values_long$Wavelength))



# Generate spectral signature plot
signature_plot <- ggplot(
  TEST_SITE_NAME_pixel_values_long,
  aes(
    x = Wavelength,
    y = Reflectance,
    color = HbttTyp,
    group = PltIdnt
  )
) +
  geom_line() +
  labs(
    x = "Wavelength (nm)",
    y = "Reflectance",
    title = paste0("Spectral signatures per habitat type ", testsite_name)
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(TEST_SITE_NAME_pixel_values_long$Wavelength),
    max(TEST_SITE_NAME_pixel_values_long$Wavelength),
    by = x_axis_intervall
  )) + # Example: breaks every 50 nm
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis(discrete = TRUE)

# Display the plot
print(signature_plot)

################################################################################
################################################################################



TEST_SITE_NAME_pixel_values_summary <- TEST_SITE_NAME_pixel_values_long %>%
  group_by(HbttTyp, Wavelength) %>%
  summarize(
    MeanReflectance = mean(Reflectance, na.rm = TRUE),
    MinReflectance = min(Reflectance, na.rm = TRUE),
    MaxReflectance = max(Reflectance, na.rm = TRUE),
    .groups = "drop"
  )


mean_signature_plot <- ggplot(
  TEST_SITE_NAME_pixel_values_summary,
  aes(
    x = Wavelength,
    y = MeanReflectance,
    color = HbttTyp,
    fill = HbttTyp
  )
) +
  geom_ribbon(aes(ymin = MinReflectance, ymax = MaxReflectance),
              alpha = 0.2,
              color = NA) +
  geom_line(linewidth = 0.75) +
  labs(
    x = "Wavelength (nm)",
    y = "Mean Reflectance",
    title = paste0("Mean spectral signatures per habitat type ", testsite_name)
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(TEST_SITE_NAME_pixel_values_summary$Wavelength),
    max(TEST_SITE_NAME_pixel_values_summary$Wavelength),
    by = x_axis_intervall
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE)

# Display the plot
print(mean_signature_plot)





























