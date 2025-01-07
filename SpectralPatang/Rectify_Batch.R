rm(list = ls())
graphics.off()

devtools::load_all()

# Load necessary libraries
library(doParallel)
library(foreach)
library(SpectralPatang)

# Function to process each subfolder (define your custom logic here)
process_subfolder <- function(subfolder_path) {

  cat(paste0('Start processing ', subfolder_path, '\n'))

  raw_image_folder_path <- file.path(subfolder_path,'data','hs_raw_image')
  rectified_image_folder_path <- file.path(subfolder_path,'data','rectified')

  rectrified <- rectify_Image(raw_image_folder_path,rectified_image_folder_path)

  # Placeholder for your custom logic
  message(paste("Processing:", subfolder_path))

  # Example function call (replace with your actual processing functions)
  # result <- your_function(subfolder_path)

  # Return a result (optional)
  cat(paste0('Processing ended', basename(subfolder_path), '\n'))
  return(paste("Processed", basename(subfolder_path)))
}

# Main function to process all subfolders in parallel
process_all_subfolders <- function(main_folder_path, num_cores = 2) {
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
    .export = c("process_subfolder","rectify_Image")  # Export the custom function
  ) %dopar% {
    process_subfolder(subfolder)
  }

  # Stop the parallel backend
  stopCluster(cl)

  # Return results
  return(results)
}

# Example usage
main_folder <- "/Volumes/AvirisNG/osx/subzone_e"
num_cores_to_use <- detectCores()  # Adjust the number of cores based on your system

# Call the main function
#debug(process_all_subfolders)
results <- process_all_subfolders(main_folder, num_cores = num_cores_to_use)

# Print results
print(results)



