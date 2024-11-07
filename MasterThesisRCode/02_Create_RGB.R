# Clear workspace and graphics
rm(list = ls())
graphics.off()

# Define your image path and output path


raw_image_file_path <- "data/hs_raw_image/ang20190712t231624_rfl_v2v2_img"
rgb_image_file_path <- "data/rgb/ang20190712t231624_rfl_v2v2_img_rgb.tif"

# Define the bands you want to select
bandselection <- "-b 59 -b 34 -b 20"

# GDAL translate command to extract the specified bands and create a new image
gdal_translate_command <- sprintf("gdal_translate %s -of GTiff %s %s", bandselection, raw_image_file_path, rgb_image_file_path)

# Execute the GDAL translate command
system(gdal_translate_command)

# Check if the output file was created successfully
if (file.exists(output_image)) {
  cat("Output file created successfully:", output_image, "\n")

  # GDAL edit command to set color interpretation for each band
  gdal_edit_command <- sprintf("gdal_edit -colorinterp_1 Red -colorinterp_2 Green -colorinterp_3 Blue %s", rgb_image_file_path)

  # Execute the GDAL edit command
  system(gdal_edit_command)

  # Confirm that color interpretation was set successfully
  cat("Color interpretation set to RGB for each band in", rgb_image_file_path, "\n")

} else {
  cat("Failed to create output file.\n")
}
