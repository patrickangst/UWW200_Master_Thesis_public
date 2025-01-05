rm(list = ls())
graphics.off()

devtools::load_all()

# Load necessary libraries
library(doParallel)
library(foreach)
library(SpectralPatang)

# Function to process each subfolder (define your custom logic here)
process_subfolder <- function(subfolder_path) {

  print(paste0('Start processing ', subfolder_path))

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


  analysis_result <- SpectralPatang::analyse_biodiversity(rectified_image_file_path,
                                                          mask_image_file_path,
                                                          NBbclusters = 20,
                                                          Window_size = 20,
                                                          NbCPU = num_cores_to_use,
                                                          MaxRAM = 8,
                                                          Perform_PCA = TRUE,
                                                          Map_Species = FALSE,
                                                          Map_Alpha = FALSE,
                                                          MAP_Beta = FALSE,
                                                          PCA_Threshold = 99)

  # Placeholder for your custom logic
  message(paste("Processed:", basename(subfolder_path)))

  # Example function call (replace with your actual processing functions)
  # result <- your_function(subfolder_path)

  # Return a result (optional)
  #return(paste("Processed", basename(subfolder_path)))
}

# # Main function to process all subfolders in parallel
# process_all_subfolders_cluster <- function(main_folder_path, num_cores = 2) {
#   # Check if the main folder exists
#   if (!dir.exists(main_folder_path)) {
#     stop("The specified folder does not exist!")
#   }
#
#   # List subfolders in the main folder
#   subfolders <- list.dirs(main_folder_path, recursive = FALSE)
#
#   # Ensure there are subfolders to process
#   if (length(subfolders) == 0) {
#     stop("No subfolders found in the specified folder.")
#   }
#
#   # Set up parallel backend
#   cl <- makeCluster(num_cores)
#   registerDoParallel(cl)
#
#   # Export the custom function to the workers
#   results <- foreach(
#     subfolder = subfolders,
#     .combine = c,
#     .packages = c(),  # Add necessary package names here if your logic depends on them
#     .export = c("process_subfolder","create_SAVI_mask")  # Export the custom function
#   ) %dopar% {
#     process_subfolder(subfolder)
#   }
#
#   # Stop the parallel backend
#   stopCluster(cl)
#
#   # Return results
#   return(results)
# }

# Main function to process all subfolders
process_all_subfolders <- function(main_folder_path) {
  # Check if the main folder exists
  if (!dir.exists(main_folder_path)) {
    stop("The specified folder does not exist!")
  }

  # List subfolders in the main folder
  subfolders <- list.dirs(main_folder_path, recursive = FALSE)

  # Ensure there are subfolders to process
  if (length(subfolders) == 0) {
    stop("No subfolders found in the specified folder.")
  }

  # Export the custom function to the workers
  for(folder in subfolders){
    process_subfolder(folder)
  }

}

# Example usage
main_folder <- "/media/patang/T9/d_done"
num_cores_to_use <- detectCores()  # Adjust the number of cores based on your system

# Call the main function
#debug(process_all_subfolders)
#results <- process_all_subfolders_cluster(main_folder, num_cores = num_cores_to_use)
process_all_subfolders(main_folder)
# Print results
# print(results)


