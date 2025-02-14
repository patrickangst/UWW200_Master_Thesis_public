# Clear workspace
rm(list = ls())
graphics.off()

# Load necessary libraries
library(terra)

# Set base path
base_path <- "D:/MasterThesis/final_hs_data_folder_test"

# List all subfolders in the base directory
subfolders <- list.dirs(base_path, recursive = FALSE)

# Iterate over each subfolder
for (subfolder in subfolders) {
  # Define the rectified data folder path
  rectified_folder <- file.path(subfolder, "data", "rectified")

  # Check if the rectified folder exists
  if (!dir.exists(rectified_folder)) {
    next  # Skip if the folder does not exist
  }

  # List files inside the rectified folder
  files <- list.files(rectified_folder, full.names = TRUE)

  # Filter files without an extension (ENVI file)
  envi_files <- files[!grepl("\\.[a-zA-Z0-9]+$", basename(files))]

  # Check if exactly one ENVI file exists
  if (length(envi_files) != 1) {
    warning(paste("Skipping folder:", subfolder, "- No unique ENVI file found."))
    next
  }

  # Extract folder name
  folder_name <- basename(subfolder)

  # Extract part before "_ang"
  extracted_name <- sub("(_ang.*)$", "", folder_name)

  # Print results
  print(paste("Processing:", extracted_name))
  print(paste("ENVI File:", envi_files))

  # TODO: Add further processing (e.g., reading the raster)
  # raster_data <- rast(envi_files)

  cut_shapefile_path <- file.path(
    subfolder,
    'data',
    'cut_shapefile',
    paste0(extracted_name, '_cutshape_rotated.shp')
  )

  # Define the bands you want to select
  bandselection <- '-b 59 -b 34 -b 20'

  # GDAL translate command to extract the specified bands and create a new image
  gdal_translate_command <- sprintf(
    "gdal_translate %s -of GTiff %s %s",
    bandselection,
    raw_image_file_path,
    output_image_file_path
  )

  # Execute the GDAL translate command
  system(gdal_translate_command)

  # Check if the output file was created successfully
  if (file.exists(output_image_file_path)) {
    cat("Output file created successfully:",
        output_image_file_path,
        "\n")

    # GDAL edit command to set color interpretation for each band
    gdal_edit_command <- sprintf(
      "gdal_edit.py -colorinterp_1 Red -colorinterp_2 Green -colorinterp_3 Blue %s",
      output_image_file_path
    )

    # Execute the GDAL edit command
    system(gdal_edit_command)

    # Confirm that color interpretation was set successfully
    cat("Color interpretation set to RGB for each band in",
        output_image_file_path,
        "\n")

  } else {
    cat("Failed to create output file.\n")
  }

}












rm(list = ls())
graphics.off()

devtools::load_all()

# Load necessary libraries
library(SpectralPatang)
library(biodivMapR)

Input_Image_File <- "D:/MasterThesis/final_hs_data_folder_test/FRST_AK_Plot3_ang20190713t024201rfl/data/rectified/ang20190713t024201_rfl_v2v2_img_rectified"
Output_Dir <- "D:/MasterThesis/final_hs_data_folder_test/FRST_AK_Plot3_ang20190713t024201rfl/result"

test <- biodivMapR::map_beta_div(
  Input_Image_File = Input_Image_File,
  Output_Dir = Output_Dir,
  window_size = 3,
  TypePCA = "SPCA",
  nbclusters = 38,
  Nb_Units_Ordin = 2000,
  MinSun = 0.25,
  pcelim = 0.02,
  scaling = "PCO",
  dimMDS = 3,
  FullRes = FALSE,
  LowRes = TRUE,
  nbCPU = 1,
  MaxRAM = 0.25,
  ClassifMap = FALSE
)


mask_path <- "D:/MasterThesis/final_hs_data_folder_test/BRW_PW_Plot1_ang20190712t212208rfl/mask"
hs_path <- "D:/MasterThesis/final_hs_data_folder_test/BRW_PW_Plot1_ang20190712t212208rfl/data/rectified"

tet <- SpectralPatang::create_SAVI_mask(hs_path, mask_path, 0.3, 0.5)


































# Load necessary libraries
library(sp)
library(gstat)
library(sf)
library(raster)
library(ggplot2)

# Set random seed for reproducibility
set.seed(42)

# Step 1: Create Sample Data (Unevenly Spaced Plots)
n_plots <- 30  # Number of sample plots
longitude <- runif(n_plots, min = -150, max = -140)  # Random longitudes
latitude <- runif(n_plots, min = 65, max = 70)  # Random latitudes
shannon_index <- runif(n_plots, min = 0.5, max = 3.5)  # Random Shannon values

# Create dataframe
data <- data.frame(longitude, latitude, shannon_index)

# Convert to spatial object
coordinates(data) <- ~longitude+latitude
proj4string(data) <- CRS("+proj=longlat +datum=WGS84")  # Set CRS

# Step 2: Create a Grid for Interpolation
x_range <- seq(min(longitude), max(longitude), length.out = 100)
y_range <- seq(min(latitude), max(latitude), length.out = 100)
grid <- expand.grid(longitude = x_range, latitude = y_range)
coordinates(grid) <- ~longitude+latitude
gridded(grid) <- TRUE
proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")

# Step 3: Fit Variogram Model
variogram_model <- variogram(shannon_index ~ 1, data)  # Compute experimental variogram
fitted_model <- fit.variogram(variogram_model, vgm("Sph"))  # Fit a spherical model

# Step 4: Perform Kriging
kriging_result <- krige(shannon_index ~ 1, data, grid, model = fitted_model)

# Step 5: Convert Kriging Result to Data Frame for ggplot
kriging_df <- as.data.frame(kriging_result)  # Extract data from kriging result
colnames(kriging_df)[3] <- "shannon_pred"  # Rename prediction column

# Step 6: Plot Results with ggplot2
ggplot() +
  geom_tile(data = kriging_df, aes(x = longitude, y = latitude, fill = shannon_pred)) +
  scale_fill_viridis_c(option = "plasma", name = "Shannon Index") +
  geom_point(data = as.data.frame(data), aes(x = longitude, y = latitude), color = "black", size = 2) +
  labs(title = "Kriging Interpolation of Shannon Diversity",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()














# Clear workspace
rm(list = ls())
graphics.off()



# Load necessary libraries
library(sp)
library(gstat)
library(sf)
library(ggplot2)

# Step 1: Load Data from CSV
data <- read.csv("data/Book1.csv")  # Change to your file path

data$shannon_index <- (data$shannon_index - min(data$shannon_index)) /
  (max(data$shannon_index) - min(data$shannon_index))

# Convert to spatial object
coordinates(data) <- ~longitude+latitude
proj4string(data) <- CRS("+proj=longlat +datum=WGS84")  # Set CRS

# Step 2: Define Grid for Interpolation
x_range <- seq(min(data$longitude), max(data$longitude), length.out = 100)
y_range <- seq(min(data$latitude), max(data$latitude), length.out = 100)
grid <- expand.grid(longitude = x_range, latitude = y_range)
coordinates(grid) <- ~longitude+latitude
gridded(grid) <- TRUE
proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")

# Step 3: Fit Variogram Model
variogram_model <- variogram(shannon_index ~ 1, data)  # Compute experimental variogram
fitted_model <- fit.variogram(variogram_model, vgm("Sph"))  # Fit a spherical model

# Step 4: Perform Kriging
kriging_result <- krige(shannon_index ~ 1, data, grid, model = fitted_model)

# Step 5: Convert Kriging Result to Data Frame for ggplot
kriging_df <- as.data.frame(kriging_result)  # Extract kriging result
colnames(kriging_df)[3] <- "shannon_pred"  # Rename prediction column

# Step 6: Plot Results with ggplot2
ggplot() +
  geom_tile(data = kriging_df, aes(x = longitude, y = latitude, fill = shannon_pred)) +
  scale_fill_viridis_c(option = "plasma", name = "Shannon Index") +
  geom_point(data = as.data.frame(data), aes(x = longitude, y = latitude), color = "black", size = 2) +
  labs(title = "Kriging Interpolation of Shannon Diversity",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()



# Clear workspace
rm(list = ls())
graphics.off()

devtools::load_all()

# Load necessary libraries
library(SpectralPatang)

SpectralPatang::create_rectangular_shapefile('~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/AN_TJ_Plot1_ang20220711t002111rfl/data/plotlocation_shapefile/AN_TJ_Plot1/AN_TJ_Plot1.shp','~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/AN_TJ_Plot1_ang20220711t002111rfl/data/cut_shapefile',Buffer = 100, Square = TRUE)





# Clear workspace
rm(list = ls())
graphics.off()

library("readxl")

# Specify sheet by its name
my_data <- read_excel("D:/MasterThesis/gound_data/datasets/All_plots.xlsx", sheet = "input")

number_species_pos <- which(names(my_data) == 'Number of species')

plot_data_filtered <- my_data %>%
  dplyr::select('Dataset','Testsite','Releve number','Year','Month','Day','Elevation (m)','Number of species', all_of(names(plot_data_filtered)[(number_species_pos + 1):ncol(plot_data_filtered)]))


df_list <- split(my_data, my_data$Testsite)







# Clear workspace
rm(list = ls())
graphics.off()

library("readxl")
library("dplyr")
library(vegan)

# Specify sheet by its name
my_data <- read_excel("D:/MasterThesis/gound_data/datasets/hugo.xlsx", sheet = "input")

# get the position of the 'Number of species' column
number_species_pos <- which(names(my_data) == 'Number of species')

# Select relevant columns
plot_data_filtered <- my_data %>%
  dplyr::select('Dataset','Testsite','Releve number','Number of species', all_of(names(my_data)[(number_species_pos + 1):ncol(my_data)]))

# replace -9 with NA
plot_data_filtered <- plot_data_filtered %>% mutate(across(everything(), ~ replace(.x, .x == -9, NA)))

# Replace NA with 0
plot_data_filtered[is.na(plot_data_filtered)] <- 0

# Keep columns that have at least one non-zero value
beta_df_cleaned <- plot_data_filtered %>%
  select(where(~ any(. != 0)))










