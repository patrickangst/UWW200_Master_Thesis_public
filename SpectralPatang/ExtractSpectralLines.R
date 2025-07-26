## Patrick Angst
## Master thesis
## 11.07.2025
## Wrapper script to call the function extract_spectral_signature() in
## ExtractSpectralLines_Script.R. This script calls the function for every test
## site found in a specific folder.

# clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

source('ExtractSpectralLines_Script.R')

# testsite_name <- 'AN_TJ_1'
# x_axis_intervall <- 50
# pixel_buffer <- 5 # Buffer in meters around the coordinates to calculate the mean reflectance value



folder_names <- folder_names <- list.dirs("data/MasterThesis/final_hs_data_folder", full.names = FALSE, recursive = FALSE)

for (name in folder_names) {
  # debug(extract_spectral_signature)
  extract_spectral_signature(name)

  print(paste0(name, " done"))
}
