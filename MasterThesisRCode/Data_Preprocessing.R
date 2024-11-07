# ==============================================================================
# biodivMapR
# Lib_FilterData.R
# ==============================================================================
# PROGRAMMERS:
# Jean-Baptiste FERET <jb.feret@teledetection.fr>
# Florian de Boissieu <fdeboiss@gmail.com>
# Copyright 2020/06 Jean-Baptiste FERET
# ==============================================================================
# This library contains functions to filter raster based on radiometric criteria
# ==============================================================================

#' Performs radiometric filtering based on three criteria: NDVI, NIR reflectance, Blue reflectance
#'
#' @param raw_img_file_path character. Path of the image to be processed
#' @param raw_img_file_path character. Path of the mask corresponding to the image
#' @param cutline_file_path character. Path of the mask corresponding to the image
#' @param centroid_coords description
#' @param rectangle_x_dim_m description
#' @param rectangle_y_dim_m description
#' @param Output_Dir character. Path for output directory
#' @param NDVI_Thresh numeric. NDVI threshold applied to produce a mask (select pixels with NDVI>NDVI_Thresh)




  perform_create_cutline_shapefile <- function(raw_img_file_path,
                                               Output_Dir,
                                               centroid_coords,
                                               rectangle_x_dim_m,
                                               rectangle_y_dim_m) {

  return(Mask_Path)
}
