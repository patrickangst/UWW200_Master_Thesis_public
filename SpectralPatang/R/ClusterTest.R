#' Perform cluster testing for kmeans clustering
#'
#' This function is a wrapper for the biodiversity calculations using the
#' package BiodivmapR.
#' @param Main_Folder_Path character. Path of the main folder
#' @param NbCPU numeric. Number of CPUs to use in parallel.
#' @param MaxRAM numeric. MaxRAM maximum size of chunk in GB to limit RAM allocation when reading image file.
#' @param PCA_Threshold number. Percentage explained by PCs for selecting PCs.
#'
#' @return Returns the full file name
#' @export
#'

test_cluster <- function(Main_Folder_Path,
                         NbCPU = 4,
                         MaxRAM = 8,
                         PCA_Threshold = 99) {
  get_esri_file_path <- function(file_path) {
    # List all files in the folder
    files <- list.files(file_path, full.names = TRUE)

    # Filter files without an extension
    file_without_ext_path <- files[!grepl("\\.[a-zA-Z0-9]+$", basename(files))]
    # Check if exactly one file without extension exists
    if (length(file_without_ext_path) != 1) {
      stop("Either no or multiple files without extensions found in the folder.")
    }

    return(file_without_ext_path)
  }

  get_shapefile_path <- function(file_path) {
    # List all files in the folder
    files <- list.files(file_path, pattern = "\\.shp$", full.names = TRUE)

    # Check if exactly one .shp file exists
    if (length(files) != 1) {
      stop("Either no or multiple .shp files found in the folder.")
    }

    return(files)
  }

  cut_image <- function(input_image_file_path,
                        output_image_file_path,
                        shp_file_path,
                        file_format) {
    gdal_cut_command <- sprintf(
      "gdalwarp -cutline %s -crop_to_cutline -of %s %s %s",
      shp_file_path,
      file_format,
      input_image_file_path,
      output_image_file_path
    )

    # # Execute the command in R
    system(gdal_cut_command)

  }

  input_image_folder_path <- file.path(Main_Folder_Path, 'data', 'hs_raw_image')
  cluster_image_folder_path <- file.path(Main_Folder_Path, 'clustertest', 'data', 'hs_raw_image')
  rectified_image_folder_path <- file.path(Main_Folder_Path, 'data', 'rectified')
  cluster_rectified_image_folder_path <- file.path(Main_Folder_Path, 'clustertest', 'data', 'rectified')
  cluster_rgb_image_folder_path <- file.path(Main_Folder_Path, 'clustertest', 'data', 'rgb')
  cluster_plotlocations_shp_folder_path <- file.path(Main_Folder_Path, 'clustertest', 'data', 'plotlocations')
  cluster_species_analysis_folder_path <- file.path(Main_Folder_Path, 'clustertest', 'data', 'species_analysis')
  cluster_cutfile_shp_folder_path <- file.path(Main_Folder_Path, 'clustertest', 'data', 'cutfile')
  cluster_mask_folder_path <- file.path(Main_Folder_Path, 'clustertest', 'mask')
  cluster_result_folder_path <- file.path(Main_Folder_Path, 'clustertest', 'result')
  result_folder_path <- file.path(Main_Folder_Path, 'result')
  mask_folder_path <- file.path(Main_Folder_Path, 'mask')

  # get full raw image file path and name
  raw_image_file_path <- get_esri_file_path(input_image_folder_path)
  raw_image_file_name <- basename(raw_image_file_path)

  # create cluster raw image file credentials
  cluster_raw_image_file_name <- paste0(raw_image_file_name, '_clustertest')
  cluster_raw_image_file_path <- file.path(cluster_image_folder_path, cluster_raw_image_file_name)


  # get full rectified image file path and name
  rectified_image_file_path <- get_esri_file_path(rectified_image_folder_path)
  rectified_image_file_name <- basename(rectified_image_file_path)

  # create cluster rectified image file credentials
  cluster_rectified_image_file_name <- paste0(rectified_image_file_name, '_clustertest')
  cluster_rectified_image_file_path <- file.path(cluster_rectified_image_folder_path,
                                                 cluster_rectified_image_file_name)

  # get plot location shape file path and name
  cluster_plotlocations_shp_file_path <- get_shapefile_path(cluster_plotlocations_shp_folder_path)
  cluster_plotlocations_shp_file_name <- basename(cluster_plotlocations_shp_file_path)

  # create cutfile shape from plot locations
  cluster_cutfile_shp_file_path <- SpectralPatang::create_rectangular_shapefile(cluster_plotlocations_shp_file_path,
                                                                                cluster_cutfile_shp_folder_path,
                                                                                20)

  # cut full input image
  cut_image(
    raw_image_file_path,
    cluster_raw_image_file_path,
    cluster_cutfile_shp_file_path,
    'ENVI'
  )

  # cut rectified image
  cut_image(
    rectified_image_file_path,
    cluster_rectified_image_file_path,
    cluster_cutfile_shp_file_path,
    'ENVI'
  )

  # create mask file
  cluster_savi_mask <- SpectralPatang::create_SAVI_mask(cluster_rectified_image_folder_path,
                                                        cluster_mask_folder_path)

  savi_mask <- SpectralPatang::create_SAVI_mask(rectified_image_folder_path, mask_folder_path)

  num_cores <- parallel::detectCores()

  SpectralPatang::analyse_biodiversity(
    rectified_image_file_path,
    savi_mask,
    NBbclusters = 5,
    Window_size = 10,
    NbCPU = num_cores,
    MaxRAM = 8,
    Perform_PCA = FALSE,
    Map_Species = FALSE,
    Map_Alpha = FALSE,
    MAP_Beta = FALSE,
    PCA_Threshold = 99
  )

  # create file path and name for selected principle components file for cluster analysis
  cluster_selected_pc_file_path <- file.path(
    cluster_result_folder_path,
    cluster_rectified_image_file_name,
    'SPCA',
    'PCA',
    'OutputPCA_30_PCs_selection.tif'
  )

  selected_pc_file_path <- file.path(
    result_folder_path,
    rectified_image_file_name,
    'SPCA',
    'PCA',
    'OutputPCA_30_PCs_selection.tif'
  )

  selected_pc_file_name <- basename(selected_pc_file_path)
  selected_pc_file_name_without_ext <- sub("\\.tif$", "", selected_pc_file_name)
  selected_pc_cut_file_name <- paste0(selected_pc_file_name_without_ext, '_cut.tif')
  selected_pc_cut_file_path <- file.path(
    result_folder_path,
    rectified_image_file_name,
    'SPCA',
    'PCA',
    selected_pc_cut_file_name
  )

  # cut rectified image
  cut_image(
    selected_pc_file_path,
    selected_pc_cut_file_path,
    cluster_cutfile_shp_file_path,
    'GTiff'
  )

  optimal_cluster_number_cut <- get_optimal_cluster_number(selected_pc_cut_file_path,
                                                           Min_Cluster = 2,
                                                           Max_Cluster = 30)

  cat(
    "Optimal number of optimal_cluster_number_cut (Elbow Method):",
    optimal_cluster_number_cut,
    "\n"
  )

  optimal_cluster_number <- get_optimal_cluster_number(selected_pc_file_path,
                                                       Min_Cluster = 2,
                                                       Max_Cluster = 30)

  cat(
    "Optimal number of optimal_cluster_number_cut (Elbow Method):",
    optimal_cluster_number_cut,
    "\n"
  )

  # SpectralPatang::analyse_biodiversity(
  #   rectified_image_file_path,
  #   savi_mask,
  #   NBbclusters = optimal_cluster_number,
  #   Window_size = 10,
  #   NbCPU = num_cores,
  #   MaxRAM = 8,
  #   Perform_PCA = FALSE,
  #   Map_Species = TRUE,
  #   Map_Alpha = TRUE,
  #   MAP_Beta = TRUE,
  #   PCA_Threshold = 99
  # )

  return(rectified_image_file_name)
}

debug(test_cluster)
path_name <- test_cluster(
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl'
)
