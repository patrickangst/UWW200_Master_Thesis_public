rm(list = ls(all = TRUE))
gc()
graphics.off()

devtools::load_all()

# Load necessary libraries
library(SpectralPatang)
library(biodivMapR)
library(doParallel)
library(terra)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(readxl)
library(writexl)
library(dplyr)

# Set main folder
main_folder_path <- 'D:/MasterThesis'

# Set the directory containing the test site folders
test_sites_folder_path <- file.path(main_folder_path, 'final_hs_data_folder')

# Set the directory containing the GeoTIFFs
pca_superfolder_path <- file.path(main_folder_path, '01_principle_components')

# Set the directory containing the GeoTIFFs
pc_selection_path <- file.path(main_folder_path, '08_principle_components_selection')

# Set the directory containing the PNGs
png_folder <- file.path(main_folder_path, '02_principle_components_png')

# Set the directory for the RGP plots
rgb_superfolder_path <- file.path(main_folder_path, '04_RGB')

# Set the directory for the Metrics Excel
metrics_file_path <- file.path(main_folder_path, '07_Testsite_Metrics', 'Metrics.xlsx')

# various variables
num_cores_to_use <- detectCores() - 2
Window_size <- 5
TypePCA <- 'SPCA'


test_sites <- list.files(test_sites_folder_path)

################################################################################
################################################################################
# Function definition
################################################################################
################################################################################

gdal_translate_tif <- function(pca_envi_input_file_path,
                               pca_gtiff_output_file_path) {
  gdal_translate_command <- sprintf(
    "gdal_translate -of GTiff %s %s",
    pca_envi_input_file_path,
    pca_gtiff_output_file_path
  )

  # Execute the GDAL edit command
  system(gdal_translate_command)

}

#
# Function definition part 1 - rectification
#

part_one <- function(test_site_folder_path) {
  # define folder path variables
  shapefile_plotlocation_folder_path <- file.path(test_site_folder_path, 'shapefile_plotlocation')
  shapefile_cutline_folder_path <- file.path(test_site_folder_path, 'shapefile_cutline')
  mask_folder_path <- file.path(test_site_folder_path, 'mask')
  image_rgb_folder_path <- file.path(test_site_folder_path, 'image_rgb')
  image_rectified_folder_path <- file.path(test_site_folder_path, 'image_rectified')
  image_raw_folder_path <- file.path(test_site_folder_path, 'image_raw')
  result_biodivMapR_folder_path <- file.path(test_site_folder_path, 'result_biodivMapR')

  test_site_name <- basename(test_site_folder_path)

  # create cut shape for gdal_warp
  # SpectralPatang::create_rectangular_shapefile(
  #   shapefile_plotlocation_folder_path,
  #   shapefile_cutline_folder_path,
  #   Buffer = 100,
  #   Square = TRUE
  # )

  # rectify image
  #debug(rectify_Image)
  image_rectified_file_path <- SpectralPatang::rectify_Image(
    image_raw_folder_path,
    image_rectified_folder_path,
    Cutfile = TRUE,
    shapefile_cutline_folder_path
  )


  # create RGB for entire picture
  SpectralPatang::create_RGB(image_raw_folder_path, image_rgb_folder_path, Site_Name = test_site_name)
  SpectralPatang::create_RGB(image_raw_folder_path, rgb_superfolder_path, Site_Name = test_site_name)

  # create RGB for rectified picture
  SpectralPatang::create_RGB(image_rectified_folder_path,
                             image_rgb_folder_path,
                             Site_Name = test_site_name)
  SpectralPatang::create_RGB(image_rectified_folder_path,
                             rgb_superfolder_path,
                             Site_Name = test_site_name)

  #create MASK
  mask_file_path <- SpectralPatang::create_SAVI_mask(image_rectified_folder_path, mask_folder_path)

  # peform PCA
  cat(paste0('Start PCA for: ', test_site_name, '\n'))

  # perform biodivMapR PCA
  PCA_Output <- biodivMapR::perform_PCA(
    Input_Image_File = image_rectified_file_path,
    Input_Mask_File = mask_file_path,
    Output_Dir = result_biodivMapR_folder_path,
    Continuum_Removal = FALSE,
    TypePCA = TypePCA,
    NbPCs_To_Keep = 30,
    FilterPCA = FALSE,
    nbCPU = num_cores_to_use,
    MaxRAM = 8
  )

  # Save PCs as Geotiff
  pca_file_path <- PCA_Output[["PCA_Files"]]
  pca_gtiff_file_path <- file.path(pca_superfolder_path, paste0(test_site_name, '_pcs.tif'))
  gdal_translate_tif(pca_file_path, pca_gtiff_file_path)


  # create a txt file for the pc selection
  # selected_components_file_path <- file.path(
  #   result_biodivMapR_folder_path,
  #   basename(image_rectified_file_path),
  #   TypePCA,
  #   'PCA',
  #   'Selected_Components.txt'
  # )
  # file.create(selected_components_file_path)

  cat(paste0('PCA done for: ', test_site_name, '\n'))

}


#
# Function definition part 2 - pca analysis
#

part_two <- function() {
  # Ensure the PNG folder exists
  if (!dir.exists(png_folder)) {
    dir.create(png_folder, recursive = TRUE)
  }

  # List all .tif files in the folder
  tiff_files <- list.files(pca_superfolder_path,
                           pattern = "\\.tif$",
                           full.names = TRUE)

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
        theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "right",
          strip.text = element_text(size = 14, face = "bold")
        ) +
        labs(title = paste("Band", i, "of", filename_base))

      # Save each band in the specific image folder
      output_filename <- file.path(image_folder, paste0(filename_base, "_band_", i, ".png"))
      ggsave(
        filename = output_filename,
        plot = p,
        width = 6,
        height = 6,
        dpi = 400
      )

      #print(p)  # Display the plot

    }
  }

  # Loop through all GeoTIFF files and generate plots
  for (tiff in tiff_files) {
    save_tiff_bands(tiff)
    cat(paste0('Site done: ', basename(tiff), '\n'))
  }
}

#
# Function definition part 3 - pca selection processing
#

part_three <- function(test_site_folder_path) {
  result_biodivMapR_folder_path <- file.path(test_site_folder_path, 'result_biodivMapR')
  test_site_name <- basename(test_site_folder_path)

  # List all files in the folder
  result_files_path <- list.files(result_biodivMapR_folder_path, full.names = TRUE)

  # Filter files without an extension
  result_folder_path <- result_files_path[!grepl("\\.[a-zA-Z0-9]+$", basename(result_files_path))]

  # Check if exactly one file without extension exists
  if (length(result_folder_path) != 1) {
    stop("No result folder found")
  }

  # get pca folder path
  pca_folder_path <- file.path(result_folder_path, TypePCA, 'PCA')

  # set pc selection txt
  pca_selection_txt_file_path <- file.path(pca_folder_path, 'Selected_Components.txt')
  file.create(pca_selection_txt_file_path)

  #read excel with pca info
  metrics_data <- read_excel(metrics_file_path, sheet = "Sheet1")

  # Read the selected PC values, e.g. 1,2,3,4
  pc_selection <- metrics_data %>%
    filter(Plot_Location_Shp_Subset_Name == test_site_name) %>%
    select(PCs)

  # read numbers splitted
  numbers <- as.numeric(unlist(strsplit(pc_selection$PCs, ",")))

  # Write numbers to .txt file
  writeLines(as.character(numbers), pca_selection_txt_file_path)

  # Create string for gdal_translate
  b_string <- paste("-b", numbers, collapse = " ")

  # Create GeoTiff with only the selected principle components
  pc_envi_file_path <- file.path(pca_folder_path, 'OutputPCA_30_PCs')
  pc_selection_geotiff_file_path <- file.path(pca_folder_path,
                                              paste0(test_site_name, '_pc_selection.tif'))

  # GDAL translate command
  gdal_translate_command <- sprintf(
    "gdal_translate %s -of GTiff %s %s",
    b_string,
    pc_envi_file_path,
    pc_selection_geotiff_file_path
  )

  # Execute the GDAL translate command
  system(gdal_translate_command)

}


#
# Function definition part 4 - cluster analysis
#

part_four <- function(test_site_folder_path) {
  result_biodivMapR_folder_path <- file.path(test_site_folder_path, 'result_biodivMapR')
  test_site_name <- basename(test_site_folder_path)

  # List all files in the folder
  result_files_path <- list.files(result_biodivMapR_folder_path, full.names = TRUE)

  # Filter files without an extension
  result_folder_path <- result_files_path[!grepl("\\.[a-zA-Z0-9]+$", basename(result_files_path))]

  # Check if exactly one file without extension exists
  if (length(result_folder_path) != 1) {
    stop("No result folder found")
  }

  # get pca folder path
  pca_folder_path <- file.path(result_folder_path, TypePCA, 'PCA')

  # set optimal clusternumber txt
  optimal_cluster_number_file_path <- file.path(pca_folder_path, 'Optimal_cluster_number.txt')
  file.create(optimal_cluster_number_file_path)

  #read excel with pca info
  metrics_data <- read_excel(metrics_file_path, sheet = "Sheet1")

  # Read the selected PC values, e.g. 1,2,3,4
  pc_selection <- metrics_data %>%
    filter(Plot_Location_Shp_Subset_Name == test_site_name) %>%
    select(PCs)

  # GeoTiff with only the selected principle components
  pc_selection_geotiff_file_path <- file.path(pca_folder_path,
                                              paste0(test_site_name, '_pc_selection.tif'))

  # get the optimal cluster number
  cat(paste0('Start getting optimal cluster number: ', test_site_name, '\n'))
  optimal_cluster_number <- get_optimal_cluster_number(
    pc_selection_geotiff_file_path,
    Downsample = FALSE,
    Downsample_factor = 2,
    Downsample_function = "sd",
    Min_Cluster = 2,
    Max_Cluster = 50
  )
  cat(paste0('Getting optimal cluster number finished. Optimal number: ', optimal_cluster_number, '\n'))

  # Write numbers to .txt file
  writeLines(as.character(optimal_cluster_number), optimal_cluster_number_file_path)

  # Update the 'SpectralSpecies' column
  metrics_data$SpectralSpecies[metrics_data$Plot_Location_Shp_Subset_Name == test_site_name] <- optimal_cluster_number
  write_xlsx(metrics_data, path = metrics_file_path)

  gc()
}



################################################################################
################################################################################
# Start Workflow
################################################################################
################################################################################


################################################################################
# perform part 1: Masking, rectification, PCA
################################################################################

sites_done_part1 <- c("hugo")

for (site in test_sites) {
  if (!(site %in% sites_done_part1)) {
    test_site_folder_path_loop <- file.path(test_sites_folder_path, site)
    cat(paste0('Start process part 1: ', site, '\n'))
    part_one(test_site_folder_path_loop)
    cat(paste0('End process part 1: ', site, '\n'))
    sites_done_part1 <- c(sites_done_part1, site)
  } else {
    print(paste(site, " is already done."))
  }
}

################################################################################
# perform part 2: Create PC plot for analysis
################################################################################

part_two()

# Analyse principle components and fill seletion into Metrics.xlsx

################################################################################
# perform part 3: PC selection processing
################################################################################

sites_done_part3 <- c("hugo")

for (site in test_sites) {
  if (!(site %in% sites_done_part3)) {
    test_site_folder_path_loop <- file.path(test_sites_folder_path, site)
    cat(paste0('Start process part 3: ', site, '\n'))
    part_three(test_site_folder_path_loop)
    cat(paste0('End process part 3: ', site, '\n'))
    sites_done_part3 <- c(sites_done_part3, site)
  } else {
    print(paste(site, " is already done."))
  }
}

################################################################################
# perform part 4: Cluster analysis
################################################################################

sites_done_part3 <- c("hugo")

for (site in test_sites) {
  if (!(site %in% sites_done_part3)) {
    test_site_folder_path_loop <- file.path(test_sites_folder_path, site)
    cat(paste0('Start process part 3: ', site, '\n'))
    part_three(test_site_folder_path_loop)
    cat(paste0('End process part 3: ', site, '\n'))
    sites_done_part3 <- c(sites_done_part3, site)
  } else {
    print(paste(site, " is already done."))
  }
}
