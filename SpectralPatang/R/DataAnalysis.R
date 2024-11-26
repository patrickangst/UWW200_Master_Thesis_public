#' Perform diversity calculations
#'
#' This function is a wrapper for the biodiversity calculations using the
#' package BiodivmapR.
#' @param Hyperspectral_Image_File_Path character. Path of the image to be processed
#' @param Mask_Image_File_Path character. Path of the mask file image
#'
#' @return Returns the full rectified image file path
#' @export
#'

analyse_biodiversity <- function(Hyperspectral_Image_File_Path,
                                 Mask_Image_File_Path,
                                 NBbclusters = 20,
                                 Window_size = 20,
                                 NbCPU = 4,
                                 MaxRAM = 8) {

  Input_Mask_File <- Mask_Image_File_Path
  Input_Image_File <- Hyperspectral_Image_File_Path
  Input_HDR_File <- biodivMapR::get_HDR_name(Hyperspectral_Image_File_Path,showWarnings = FALSE)
  rectified_image_file_name <- basename(Hyperspectral_Image_File_Path)

  output_folder_path <- dirname(Hyperspectral_Image_File_Path)
  output_folder_path <- sub("data/rectified", "result", output_folder_path)

  Output_Dir <- file.path(output_folder_path,rectified_image_file_name)
  dir.create(path = Output_Dir, recursive = TRUE, showWarnings = FALSE)

  # Apply normalization without continuum removal
  Continuum_Removal <- FALSE
  # Type of dimensionality reduction
  TypePCA <- 'SPCA'
  # PCA FILTERING:        Set to TRUE if you want second filtering based on PCA outliers to be processed.
  # Slower process
  # Automatically set to FALSE if TypePCA     = 'MNF'
  FilterPCA <- FALSE
  # window size for computation of spectral diversity

  Excluded_WL <- c(0, 442)
  Excluded_WL <- rbind(Excluded_WL, c(1368, 1499))
  Excluded_WL <- rbind(Excluded_WL, c(1779, 2055))
  Excluded_WL <- rbind(Excluded_WL, c(2400, 2501))

  ################################################################################
  ##                  Perform PCA & Dimensionality reduction                    ##
  ## https://jbferet.github.io/biodivMapR/articles/biodivMapR_4.html            ##
  ################################################################################
  print("PERFORM DIMENSIONALITY REDUCTION")
  #debug(perform_PCA)
  PCA_Output <- biodivMapR::perform_PCA(Input_Image_File = Input_Image_File,
                                        Input_Mask_File = Input_Mask_File,
                                        Output_Dir = Output_Dir,
                                        TypePCA = TypePCA,
                                        FilterPCA = FilterPCA,
                                        Excluded_WL = Excluded_WL,
                                        nbCPU = NbCPU,
                                        MaxRAM = MaxRAM,
                                        Continuum_Removal = Continuum_Removal)


  # Save the list as an RDS file
  pca_output_rds_file_path = paste0(Output_Dir,"/",rectified_image_file_name,"/",TypePCA,"/PCA/","PCA_Output.rds")
  saveRDS(PCA_Output, file = pca_output_rds_file_path)

  # Later, load the list back into R
  PCA_Output <- readRDS(pca_output_rds_file_path)

  # path for the updated mask
  Input_Mask_File <- PCA_Output$MaskPath


  # Select components from the PCA/SPCA/MNF raster
  # Sel_PC = path of the file where selected components are stored
  Sel_PC <- biodivMapR::select_PCA_components(Input_Image_File = Input_Image_File,
                                  Output_Dir = Output_Dir,
                                  PCA_Files = PCA_Output$PCA_Files,
                                  TypePCA = PCA_Output$TypePCA,
                                  File_Open = TRUE)

  ################################################################################
  ##                  Perform Spectral species mapping                          ##
  ## https://jbferet.github.io/biodivMapR/articles/biodivMapR_5.html            ##
  ################################################################################
  print("MAP SPECTRAL SPECIES")
  Kmeans_info <- biodivMapR::map_spectral_species(Input_Image_File = Input_Image_File,
                                      Input_Mask_File = PCA_Output$MaskPath,
                                      Output_Dir = Output_Dir,
                                      SpectralSpace_Output = PCA_Output,
                                      nbclusters = NBbclusters,
                                      nbCPU = NbCPU, MaxRAM = MaxRAM)

  ################################################################################
  ##                Perform alpha and beta diversity mapping                    ##
  ## https://jbferet.github.io/biodivMapR/articles/biodivMapR_6.html            ##
  ################################################################################
  print("MAP ALPHA DIVERSITY")
  Index_Alpha   = c('Shannon','Simpson')
  #Index_Alpha <- c('Shannon')
  biodivMapR::map_alpha_div(Input_Image_File = Input_Image_File,
                Output_Dir = Output_Dir,
                TypePCA = TypePCA,
                window_size = Window_size,
                nbCPU = NbCPU,
                MaxRAM = MaxRAM,
                Index_Alpha = Index_Alpha,
                nbclusters = NBbclusters)

  print("MAP BETA DIVERSITY")
  biodivMapR::map_beta_div(Input_Image_File = Input_Image_File,
               Output_Dir = Output_Dir,
               TypePCA = TypePCA,
               window_size = Window_size,
               nbCPU = NbCPU,
               MaxRAM = MaxRAM,
               nbclusters = NBbclusters)

  return('Hugo')

}

#debug(analyse_biodiversity)
#path_name <- analyse_biodiversity('ang20180729t212542rfl/data/rectified/ang20180729t212542_rfl_v2r2_img_rectified','ang20180729t212542rfl/mask/ang20180729t212542_rfl_v2r2_img_rectified_savi_mask_02')

