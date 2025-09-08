# Clean workspace
rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(scico)

image_paths <- c(
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_1/image_rectified/ang20220711t002111_rfl_v2aa2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_2/image_rectified/ang20220711t003358_rfl_v2aa2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/ATQ_VK_1/image_rectified/ang20190712t231624_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_PW_1/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_VS_1/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_1/image_rectified/ang20190712t211646_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_2/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_3/image_rectified/ang20190712t231624_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_4/image_rectified/ang20190706t210739_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_2/image_rectified/ang20220709t233937_rfl_v2aa2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_3/image_rectified/ang20190713t024201_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUAIR_DW_1/image_rectified/ang20170709t003728_corr_v2p9_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUARC_DW_1/image_rectified/ang20170709t000442_corr_v2p9_img_rectified'
)

savi_threshold <- 0.2

for (image_path in image_paths) {
  hyperspectral <- rast(image_path)
  test_site_image_name <- basename(dirname(dirname(image_path)))
  message(test_site_image_name, " masked start")

  # --- Vegetation mask using SAVI ---
  red_average <- terra::app(hyperspectral[[56:65]], fun = mean, na.rm = TRUE)
  NIR_average <- terra::app(hyperspectral[[86:105]], fun = mean, na.rm = TRUE)
  L <- 0.5
  savi <- ((NIR_average - red_average) / (NIR_average + red_average + L)) * (1 + L)
  veg_mask <- savi > savi_threshold
  hyperspectral <- mask(hyperspectral, veg_mask, maskvalues = FALSE)

  # Check bands
  if (nlyr(hyperspectral) != 425) stop("Expected 425 bands")

  # --- Convert to pixel × band matrix ---
  # Extract values: matrix [n_pixels × n_bands]
  # Extract values: [n_pixels × n_bands]
  pixel_matrix <- values(hyperspectral)
  colnames(pixel_matrix) <- names(hyperspectral)  # keep wavelength labels

  # Define bad bands (to keep as NA in the final raster)
  bad_bands <- c(1:15, 191:211, 285:320, 418:425)

  # Make a copy for normalization
  pixel_norm <- pixel_matrix

  # Normalize only good bands, per-pixel (row-wise L2 norm)
  good_bands <- setdiff(seq_len(ncol(pixel_matrix)), bad_bands)

  pixel_norm[, good_bands] <- t(apply(pixel_matrix[, good_bands, drop = FALSE], 1, function(row) {
    if (all(is.na(row))) return(rep(NA, length(row)))
    row / sqrt(sum(row^2, na.rm = TRUE))
  }))

  # Bad bands stay NA (we didn’t touch them)

  # Create new raster with all 425 bands
  norm_rast <- hyperspectral

  # Put values back
  values(norm_rast) <- pixel_norm

  # Restore names
  names(norm_rast) <- names(hyperspectral)

  # Save
  out_dir <- file.path(dirname(dirname(image_path)), "image_normalized")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  out_file <- file.path(out_dir, paste0(test_site_image_name, "_NormalizedHyperspectral.tif"))
  writeRaster(norm_rast, out_file, overwrite = TRUE)

  message(test_site_image_name, " masked + normalized done")
  gc()
}

