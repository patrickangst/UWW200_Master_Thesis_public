rm(list = ls())
graphics.off()

devtools::load_all()

# Load necessary libraries
library(doParallel)
library(foreach)
library(SpectralPatang)

# Function to process each subfolder (define your custom logic here)
process_subfolder <- function(subfolder_path) {

  cat(paste0('Start processing: ', basename(subfolder_path), '\n'))

  rectified_image_folder_path <- file.path(subfolder_path,'data','rectified')

  # List all files in the folder
  rectified_image_files <- list.files(rectified_image_folder_path, full.names = TRUE)

  # Filter files without an extension
  rectified_file_without_ext <- rectified_image_files[!grepl("\\.[a-zA-Z0-9]+$", basename(rectified_image_files))]

  # Check if exactly one file without extension exists
  if (length(rectified_file_without_ext) != 1) {
    stop("Either no or multiple files without extensions found in the folder.")
  }

  # Extract the file name
  rectified_image_file_name <- basename(rectified_file_without_ext)

  # Construct the full raw file path
  rectified_image_file_path <- file.path(rectified_image_folder_path, rectified_image_file_name)

  mask_image_folder_path <- file.path(subfolder_path,'mask')
  # List all files in the folder
  mask_image_files <- list.files(mask_image_folder_path, full.names = TRUE)

  # Filter files without an extension
  mask_file_without_ext <- mask_image_files[!grepl("\\.[a-zA-Z0-9]+$", basename(mask_image_files))]

  # Check if exactly one file without extension exists
  if (length(mask_file_without_ext) != 1) {
    stop("Either no or multiple files without extensions found in the folder.")
  }

  # Extract the file name
  mask_image_file_name <- basename(mask_file_without_ext)

  # Construct the full raw file path
  mask_image_file_path <- file.path(mask_image_folder_path, mask_image_file_name)

  Window_size <- 10

  cat(paste0('Start PCA for: ', basename(subfolder_path), '\n'))
  pca_selection_file_path <- SpectralPatang::analyse_biodiversity(rectified_image_file_path,
                                                          mask_image_file_path,
                                                          NBbclusters = 20,
                                                          Window_size = Window_size,
                                                          NbCPU = num_cores_to_use,
                                                          MaxRAM = 8,
                                                          Perform_PCA = TRUE,
                                                          Map_Species = FALSE,
                                                          Map_Alpha = FALSE,
                                                          MAP_Beta = FALSE,
                                                          PCA_Threshold = 99)

  cat(paste0('PCA done for: ', basename(subfolder_path), '\n'))

  # Check, if cluster ananlysis has to be done.
  perform_cluster_analysis <- FALSE
  output_folder_path <- dirname(rectified_image_file_path)
  Output_Dir <- sub("data/rectified", "result", output_folder_path)
  cluster_number_file_path <- file.path(Output_Dir,
                                        rectified_image_file_name,
                                        'SPCA',
                                        'PCA',
                                        'optimal_number_of_clusters.txt')

  # Check if the file exists
  if (file.exists(cluster_number_file_path)) {
    # Read the number from the file
    NBbclusters <- as.numeric(readLines(cluster_number_file_path))

    # Check if the value is numeric
    if (!is.na(NBbclusters)) {
      cat("The optimal number of clusters is:", NBbclusters, "\n")
    } else {
      cat("The file exists, but it does not contain a valid number.\n")
      perform_cluster_analysis <- TRUE
    }
  } else {
    cat("The file does not exist:", cluster_number_file_path, "\n")
    perform_cluster_analysis <- TRUE
  }

  if(perform_cluster_analysis){
    cat(paste0('Start cluster analysis for: ', basename(subfolder_path), '\n'))
    NBbclusters <- SpectralPatang::get_optimal_cluster_number(pca_selection_file_path,
                                                              Downsample = TRUE,
                                                              Downsample_factor = 2,
                                                              Downsample_function = "sd",
                                                              Min_Cluster = 2,
                                                              Max_Cluster = 30)
    cat(paste0('Cluster analysis done for: ', basename(subfolder_path), " number of clusters: ", NBbclusters, '\n'))
  }

  cat(paste0('Start spectral analysis for: ', basename(subfolder_path), '\n'))
  spectral_analysis <- SpectralPatang::analyse_biodiversity(rectified_image_file_path,
                                                                  mask_image_file_path,
                                                                  NBbclusters = NBbclusters,
                                                                  Window_size = Window_size,
                                                                  NbCPU = num_cores_to_use,
                                                                  MaxRAM = 8,
                                                                  Perform_PCA = FALSE,
                                                                  Map_Species = TRUE,
                                                                  Map_Alpha = TRUE,
                                                                  MAP_Beta = TRUE,
                                                                  PCA_Threshold = 99)
  cat(paste0('Spectral analysis done for: ', basename(subfolder_path), '\n'))

  cat(paste0('Processing done for: ', basename(subfolder_path), '\n'))

}


# Main function to process all subfolders
process_all_subfolders <- function(main_folder_path) {
  # Check if the main folder exists
  if (!dir.exists(main_folder_path)) {
    stop("The specified folder does not exist!")
  }

  # List subfolders in the main folder
  subfolders <- list.dirs(main_folder_path, recursive = FALSE)

  # Ensure there are sub folders to process
  if (length(subfolders) == 0) {
    stop("No subfolders found in the specified folder.")
  }

  # Export the custom function to the workers
  for(folder in subfolders){
    process_subfolder(folder)
  }

}

# Example usage
main_folder <- "/media/patang/T9/e_done"
num_cores_to_use <- detectCores()  # Adjust the number of cores based on your system

# Call the main function
#debug(process_all_subfolders)
#results <- process_all_subfolders_cluster(main_folder, num_cores = num_cores_to_use)
process_all_subfolders(main_folder)
# Print results
# print(results)


