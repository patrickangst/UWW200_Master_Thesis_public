# Clean environment
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
library(purrr)
library(tools)

# Define folders
hs_folder <- "hs_raw/"
cluster_shp <- "cluster_info_shp/metrics_data_filtered.shp"

# Load shapefile with metadata
cluster_shp_data <- st_read(cluster_shp)

# Get list of all files in "hs_raw/"
raster_files <- list.files("hs_raw/", full.names = TRUE)

# Remove extensions to get unique test site names
raster_files <- unique(tools::file_path_sans_ext(raster_files))

# Initialize empty list to store processed data
all_sites_data <- list()

# Iterate over all raster files
for (img_file_path in raster_files) {
  
  # Extract test site name (filename without extension)
  testsite_name <- tools::file_path_sans_ext(basename(img_file_path))
  
  cat("\nProcessing:", testsite_name, "\n")
  
  # Load hyperspectral raster
  hyperspectral <- rast(img_file_path)
  
  # Ensure CRS matches shapefile
  cluster_shp_data <- st_transform(cluster_shp_data, crs(hyperspectral))
  
  # Filter relevant test site
  site_data <- cluster_shp_data %>%
    filter(Testsit == testsite_name)
  
  # Skip if no matching site data found
  if (nrow(site_data) == 0) {
    cat("⚠️ No matching site data found for:", testsite_name, "\n")
    next
  }
  
  # Extract pixel values
  pixel_values <- terra::extract(hyperspectral, vect(site_data))
  
  # Combine spatial + spectral data
  site_combined <- bind_cols(site_data, pixel_values) %>%
    select(-Tblnmbr, -ID) %>%
    st_drop_geometry() %>%
    as.data.frame()
  
  # Identify spectral band columns (exclude metadata)
  band_columns <- setdiff(names(site_combined), c("HbttTyp", "Testsit", "PltIdnt"))
  
  # Rename band columns sequentially
  names(site_combined)[match(band_columns, names(site_combined))] <- paste0("Band ", seq_along(band_columns))
  
  
  # Convert to long format for spectral data
  site_long <- site_combined %>%
    pivot_longer(-c(HbttTyp, Testsit, PltIdnt),
                 names_to = "Band",
                 values_to = "Reflectance") %>%
    mutate(BandNumber = as.numeric(str_extract(Band, "\\d+"))) %>%
    mutate(Reflectance = Reflectance / 10000)  # Scaling reflectance
  
  # Apply wavelength exclusions
  site_long <- site_long %>%
    mutate(
      Reflectance = case_when(
        BandNumber >= 1 & BandNumber <= 14 ~ NA_real_,
        BandNumber >= 193 & BandNumber <= 215 ~ NA_real_,
        BandNumber >= 283 & BandNumber <= 320 ~ NA_real_,
        BandNumber >= 415 ~ NA_real_,
        TRUE ~ Reflectance
      )
    )
  
  # Store processed data for current site
  all_sites_data[[testsite_name]] <- site_long
}

# Merge all processed sites into one large data frame
merged_data <- bind_rows(all_sites_data)


# Ensure wavelengths are sorted correctly
unique_bandnumber_signature <- sort(unique(merged_data$BandNumber))


# Generate spectral signature plot
signature_plot <- ggplot(
  merged_data,
  aes(
    x = BandNumber,
    y = Reflectance,
    color = HbttTyp,
    group = PltIdnt
  )
) +
  geom_line() +
  labs(x = "Band number", y = "Reflectance", title = paste0("Spectral Signatures ",testsite_name)) +
  theme_minimal() +
  scale_x_continuous(breaks = unique_bandnumber_signature) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis(discrete = TRUE)

# Display the plot
print(signature_plot)


# Compute mean reflectance per habitat type per wavelength
merged_data_mean <- merged_data %>%
  group_by(HbttTyp, BandNumber) %>%
  summarize(MeanReflectance = mean(Reflectance, na.rm = TRUE), .groups = "drop")

# Generate the mean spectral signature plot
mean_signature_plot <- ggplot(
  merged_data_mean,
  aes(
    x = BandNumber,
    y = MeanReflectance,
    color = HbttTyp
  )
) +
  geom_line(linewidth = 1.2) +
  labs(x = "Band number", y = "Mean Reflectance", title = paste0("Mean Spectral Signatures per Habitat type ",testsite_name)) +
  theme_minimal() +
  scale_x_continuous(breaks = unique_bandnumber_signature) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis(discrete = TRUE)

# Display the plot
print(mean_signature_plot)



#
# Statistical Testing
#
merged_data_stat <- merged_data %>%
  dplyr::select(-c(BandNumber))  %>%
  mutate(Band = as.factor(Band)) %>%
  mutate(HbttTyp = as.factor(HbttTyp)) %>%
  mutate(PltIdnt = as.factor(PltIdnt)) %>%
  mutate(Testsit = as.factor(Testsit)) %>%
  na.omit()

str(merged_data_stat)

model_lm <- lm(Reflectance ~ HbttTyp  + PltIdnt + Band + HbttTyp:Band ,
               data = merged_data_stat)
anova(model_lm)
par(mfrow = c(2, 2))
plot(model_lm)




w

# Save combined dataset
write.csv(merged_data, "Combined_Spectral_Data.csv", row.names = FALSE)
cat("\n✅ All spectral data combined and saved successfully!\n")

# Print summary of final dataset
print(head(merged_data))
