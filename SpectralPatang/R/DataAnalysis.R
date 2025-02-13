#' Perform diversity calculations
#'
#' This function is a wrapper for the biodiversity calculations using the
#' package BiodivmapR.
#' @param Hyperspectral_Image_Folder_Path character. Path of the image folder
#' @param Mask_Folder_Path character. Path of the mask folder
#' @param Output_Folder_Path character. Path of the ouput folder
#' @param NBbclusters numeric. number of clusters defined in k-Means
#' @param nb_partitions numeric. Number of repetitions to estimate diversity from the raster (averaging repetitions).
#' @param Window_size numeric. Size of spatial units (in pixels) to compute diversity.
#' @param NbCPU numeric. Number of CPUs to use in parallel.
#' @param MaxRAM numeric. MaxRAM maximum size of chunk in GB to limit RAM allocation when reading image file.
#' @param Perform_PCA boolean. False if already PCA_Output.rds file available
#' @param External_PCA boolean. True if PCA is performed seperately.
#' @param Map_Species boolean. True if spectral species mapping has to be done
#' @param Map_Alpha boolean. True if calculating alpha diversity has to be done
#' @param MAP_Beta boolean. True if calculating beta diversity has to be done
#' @param PCA_Threshold number. Percentage explained by PCs for selecting PCs.
#'
#' @return Returns the full file path of the selected PCA file
#' @export
#'

analyse_biodiversity <- function(Hyperspectral_Image_Folder_Path,
                                 Mask_Folder_Path,
                                 Output_Folder_Path,
                                 NBbclusters = 20,
                                 nb_partitions = 1,
                                 Window_size = 10,
                                 NbCPU = 4,
                                 MaxRAM = 8,
                                 Perform_PCA = TRUE,
                                 External_PCA = FALSE,
                                 Map_Species = TRUE,
                                 Map_Alpha = TRUE,
                                 MAP_Beta = TRUE,
                                 PCA_Threshold = 99) {

  #
  # Get image file path
  #

  # List all files in the folder
  hs_files <- list.files(Hyperspectral_Image_Folder_Path, full.names = TRUE)
  # Filter files without an extension
  hs_file_without_ext <- hs_files[!grepl("\\.[a-zA-Z0-9]+$", basename(hs_files))]
  # Check if exactly one file without extension exists
  if (length(hs_file_without_ext) != 1) {
    stop("Either no or multiple files without extensions found in the hs image folder.")
  }
  # Extract the file name
  hs_image_file_name <- basename(hs_file_without_ext)
  # Construct the full raw file path
  Input_Image_File <- file.path(Hyperspectral_Image_Folder_Path, hs_image_file_name)

  #
  # Get mask file path
  #

  # List all files in the folder
  mask_files <- list.files(Mask_Folder_Path, full.names = TRUE)
  # Filter files without an extension
  mask_file_without_ext <- mask_files[!grepl("\\.[a-zA-Z0-9]+$", basename(mask_files))]
  # Check if exactly one file without extension exists
  if (length(mask_file_without_ext) != 1) {
    stop("Either no or multiple files without extensions found in the mask folder.")
  }
  # Extract the file name
  mask_image_file_name <- basename(mask_file_without_ext)
  # Construct the full raw file path
  Input_Mask_File <- file.path(Mask_Folder_Path, mask_image_file_name)


  Input_HDR_File <- biodivMapR::get_HDR_name(Input_Image_File, showWarnings = FALSE)
  rectified_image_file_name <- basename(Input_Image_File)

  output_folder_path <- dirname(Hyperspectral_Image_File_Path)
  Output_Dir <- Output_Folder_Path
  dir.create(path = Output_Dir,
             recursive = TRUE,
             showWarnings = FALSE)

  # Apply normalization without continuum removal
  Continuum_Removal <- FALSE
  # Type of dimensionality reduction
  TypePCA <- 'SPCA'
  # PCA FILTERING:        Set to TRUE if you want second filtering based on PCA outliers to be processed.
  # Slower process
  # Automatically set to FALSE if TypePCA     = 'MNF'
  FilterPCA <- FALSE
  # window size for computation of spectral diversity

  # this defines the wavelength that have to be excluded (e.g. water vaper region)
  Excluded_WL <- c(0, 442)
  Excluded_WL <- rbind(Excluded_WL, c(1368, 1499))
  Excluded_WL <- rbind(Excluded_WL, c(1779, 2055))
  Excluded_WL <- rbind(Excluded_WL, c(2400, 2501))

  pca_output_rds_file_path <- file.path(Output_Dir,
                                        rectified_image_file_name,
                                        TypePCA,
                                        'PCA',
                                        'PCA_Output.rds')

  if (Perform_PCA) {
    ################################################################################
    ##                  Perform PCA & Dimensionality reduction                    ##
    ## https://jbferet.github.io/biodivMapR/articles/biodivMapR_4.html            ##
    ################################################################################
    print("PERFORM DIMENSIONALITY REDUCTION")
    #debug(perform_PCA)
    PCA_Output <- biodivMapR::perform_PCA(
      Input_Image_File = Input_Image_File,
      Input_Mask_File = Input_Mask_File,
      Output_Dir = Output_Dir,
      Continuum_Removal = Continuum_Removal,
      TypePCA = TypePCA,
      NbPCs_To_Keep = 30,
      FilterPCA = FilterPCA,
      Excluded_WL = Excluded_WL,
      nb_partitions = nb_partitions,
      nbCPU = NbCPU,
      MaxRAM = MaxRAM
    )


    # Save the list as an RDS file
    saveRDS(PCA_Output, file = pca_output_rds_file_path)
  } else {
    # Later, load the list back into R
    if(!External_PCA){
      PCA_Output <- readRDS(pca_output_rds_file_path)
    }
  }

  # path for the updated mask
  Input_Mask_File <- PCA_Output$MaskPath


  # # Write these numbers to a text file, one per line
  selected_components_file_path <- file.path(Output_Dir,
                                             rectified_image_file_name,
                                             TypePCA,
                                             'PCA',
                                             'Selected_Components.txt')

  # GDAL translate command to extract the specified bands
  pca_output_envi_file_path <- file.path(Output_Dir,
                                         rectified_image_file_name,
                                         TypePCA,
                                         'PCA',
                                         'OutputPCA_30_PCs')

   pca_selection_file_path <- paste0(pca_output_envi_file_path, '_selection.tif')

  # Read the numbers from the file
  selected_components <- scan(selected_components_file_path, what = integer(), quiet = TRUE)

  # Convert the numbers into the required format
  bandselection_pca <- paste(sprintf("-b %d", selected_components), collapse = " ")

  # Print the result
  print(bandselection_pca)

  gdal_translate_command <- sprintf(
    "gdal_translate %s -of GTiff %s %s",
    bandselection_pca,
    pca_output_envi_file_path,
    pca_selection_file_path
  )

  # Execute the GDAL edit command
  system(gdal_translate_command)

  # Select the components that explain 98% of the variance
  # selected_components <- pca_model$x[, 1:num_components]

  # Select components from the PCA/SPCA/MNF raster
  # Sel_PC = path of the file where selected components are stored
  # Sel_PC <- biodivMapR::select_PCA_components(Input_Image_File = Input_Image_File,
  #                                             Output_Dir = Output_Dir,
  #                                             PCA_Files = PCA_Output$PCA_Files,
  #                                             TypePCA = PCA_Output$TypePCA,
  #                                             File_Open = TRUE)

  ################################################################################
  ##                  Perform Spectral species mapping                          ##
  ## https://jbferet.github.io/biodivMapR/articles/biodivMapR_5.html            ##
  ################################################################################
  if (Map_Species) {
    print("MAP SPECTRAL SPECIES")
    Kmeans_info <- biodivMapR::map_spectral_species(
      Input_Image_File = Input_Image_File,
      Input_Mask_File = PCA_Output$MaskPath,
      Output_Dir = Output_Dir,
      SpectralSpace_Output = PCA_Output,
      nbclusters = NBbclusters,
      nbCPU = NbCPU,
      MaxRAM = MaxRAM,
      progressbar = TRUE
    )
  }


  ################################################################################
  ##                Perform alpha and beta diversity mapping                    ##
  ## https://jbferet.github.io/biodivMapR/articles/biodivMapR_6.html            ##
  ################################################################################
  if (Map_Alpha) {
    print("MAP ALPHA DIVERSITY")
    Index_Alpha   = c('Shannon', 'Simpson')
    #Index_Alpha <- c('Shannon')
    biodivMapR::map_alpha_div(
      Input_Image_File = Input_Image_File,
      Output_Dir = Output_Dir,
      TypePCA = TypePCA,
      window_size = Window_size,
      nbCPU = NbCPU,
      MaxRAM = MaxRAM,
      Index_Alpha = Index_Alpha,
      nbclusters = NBbclusters,
      FullRes = TRUE
    )
  }

  if (MAP_Beta) {
    print("MAP BETA DIVERSITY")
    biodivMapR::map_beta_div(
      Input_Image_File = Input_Image_File,
      Output_Dir = Output_Dir,
      TypePCA = TypePCA,
      window_size = Window_size,
      nbCPU = NbCPU,
      MaxRAM = MaxRAM,
      nbclusters = NBbclusters,
      Nb_Units_Ordin = 4000,
      scaling = 'PCO',
      dimMDS = 1,
      FullRes = TRUE
    )
  }

  # print("MAP FUNCTIONAL DIVERSITY")
  # Selected_Features <- read.table(selected_components_file_path)[[1]]
  # biodivMapR::map_functional_div(Original_Image_File = Input_Image_File,
  #                    Functional_File = PCA_Output$PCA_Files,
  #                    Selected_Features = Selected_Features,
  #                    Output_Dir = Output_Dir,
  #                    window_size = Window_size,
  #                    nbCPU = NbCPU,
  #                    MaxRAM = MaxRAM,
  #                    TypePCA = TypePCA)


  return(pca_selection_file_path)
}

#debug(analyse_biodiversity)
#path_name <- analyse_biodiversity('ang20180729t212542rfl/data/rectified/ang20180729t212542_rfl_v2r2_img_rectified','ang20180729t212542rfl/mask/ang20180729t212542_rfl_v2r2_img_rectified_savi_mask_02')
