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
  mask_image_folder_path <- file.path(subfolder_path,'mask')

  mask <- create_SAVI_mask(rectified_image_folder_path,mask_image_folder_path)

  # Placeholder for your custom logic
  #message(paste("Processing:", subfolder_path))

  # Example function call (replace with your actual processing functions)
  # result <- your_function(subfolder_path)

  # Return a result (optional)
  message(paste("Processed:", basename(subfolder_path)))

}

# Main function to process all subfolders in parallel
process_all_subfolders_cluster <- function(main_folder_path, num_cores = 2) {
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

  # Set up parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  # Export the custom function to the workers
  results <- foreach(
    subfolder = subfolders,
    .combine = c,
    .packages = c(),  # Add necessary package names here if your logic depends on them
    .export = c("process_subfolder","create_SAVI_mask")  # Export the custom function
  ) %dopar% {
    process_subfolder(subfolder)
  }

  # Stop the parallel backend
  stopCluster(cl)

  # Return results
  return(results)
}

# Main function to process all subfolders in parallel
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
main_folder <- "/media/patang/T9/e_done"
num_cores_to_use <- detectCores()  # Adjust the number of cores based on your system

# Call the main function
#debug(process_all_subfolders)
#results <- process_all_subfolders_cluster(main_folder, num_cores = num_cores_to_use)
process_all_subfolders(main_folder)

# Print results
# print(results)


