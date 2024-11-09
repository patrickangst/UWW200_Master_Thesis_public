# clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

# load biodivMapR and useful libraries
library(biodivMapR)
library(labdsv)
library(tools)
library(ggplot2)
library(gridExtra)
library(terra)

# ===============================================================================
# set important variables
base_path <- getwd()

# input image folder path rectified
Datadir <- paste0(base_path,'/data/rectified')
NameRaster <- 'ang20190712t231624_rfl_v2v2_img_rectified_v2'
destfile <- file.path(Datadir,NameRaster)
destfile_HDR <- get_HDR_name(destfile,showWarnings = FALSE)


dir.create(path = Datadir,recursive = T,showWarnings = F)



# ===============================================================================
# name your raster HDR with the same name as the binary raster, with .hdr extension


# ===============================================================================
# url for the vector files corresponding to different vegetation types


################################################################################
##                      Set parameters for biodivMapR                         ##
## https://jbferet.github.io/biodivMapR/articles/biodivMapR_2.html            ##
################################################################################
# Define path for image file to be processed
Input_Image_File <- file.path(Datadir,NameRaster)
# Define path for corresponding mask file
# Set to FALSE if no mask available
#Input_Mask_File <- FALSE
Input_Mask_File <- '~/Documents/GitHub/UWW200_Master_Thesis_public/MasterThesisRCode/mask/savi_mask'
# Define path for master output directory where files produced during the process are saved

Output_Dir <- paste0('~/Documents/GitHub/UWW200_Master_Thesis_public/MasterThesisRCode/result/',NameRaster)
dir.create(path = Output_Dir,recursive = T,showWarnings = F)

dir.create(path = Output_Dir,recursive = T,showWarnings = F)
# Define levels for radiometric filtering
NDVI_Thresh <- 0.3
Blue_Thresh <- 500
NIR_Thresh <- 1500
# Apply normalization with continuum removal?
Continuum_Removal <- FALSE
# Type of dimensionality reduction
TypePCA <- 'SPCA'
# PCA FILTERING:        Set to TRUE if you want second filtering based on PCA outliers to be processed.
# Slower process
# Automatically set to FALSE if TypePCA     = 'MNF'
FilterPCA <- FALSE
# window size forcomputation of spectral diversity
window_size <- 10
# computational parameters
nbCPU <- 6
MaxRAM <- 8
# number of clusters (spectral species)
nbclusters <- 20



# set excluding wavelengths --> either "FALS" or some wavelengths

#Excluded_WL = FALSE

# bblist[0:14] = 0
# bblist[189:225] = 0
# bblist[281:336] = 0
# bblist[405:] = 0
#

Excluded_WL <- c(0, 442)
Excluded_WL <- rbind(Excluded_WL, c(1368, 1499))
Excluded_WL <- rbind(Excluded_WL, c(1779, 2055))
Excluded_WL <- rbind(Excluded_WL, c(2400, 2501))


# if (file.exists(Input_Image_File)) {
#   print("Input_Image_File File found.")
# } else {
#   print("Input_Image_File File not found. Check the path and working directory.")
# }
#
# if (file.exists(Input_Mask_File)) {
#   print("Input_Mask_File File found.")
# } else {
#   print("Input_Mask_File File not found. Check the path and working directory.")
# }

################################################################################
##                      Perform radiometric filtering                         ##
## https://jbferet.github.io/biodivMapR/articles/biodivMapR_3.html            ##
################################################################################
# print("PERFORM RADIOMETRIC FILTERING")
# Input_Mask_File <- perform_radiometric_filtering(Image_Path = Input_Image_File,
#                                                  Mask_Path = Input_Mask_File,
#                                                  Output_Dir = Output_Dir,
#                                                  TypePCA = TypePCA,
#                                                  NDVI_Thresh = NDVI_Thresh,
#                                                  Blue_Thresh = Blue_Thresh,
#                                                  NIR_Thresh = NIR_Thresh)

################################################################################
##                  Perform PCA & Dimensionality reduction                    ##
## https://jbferet.github.io/biodivMapR/articles/biodivMapR_4.html            ##
################################################################################
print("PERFORM DIMENSIONALITY REDUCTION")
#debug(perform_PCA)
PCA_Output <- perform_PCA(Input_Image_File = Input_Image_File,
                          Input_Mask_File = Input_Mask_File,
                          Output_Dir = Output_Dir,
                          TypePCA = TypePCA,
                          FilterPCA = FilterPCA,
                          Excluded_WL = Excluded_WL,
                          nbCPU = nbCPU,
                          MaxRAM = MaxRAM,
                          Continuum_Removal = Continuum_Removal)

# path for the updated mask
Input_Mask_File <- PCA_Output$MaskPath

pca_output_image_file_path = paste0(Output_Dir,TypePCA,"/PCA/","OutputPCA_30_PCs")
pca_output_image <- rast(pca_output_image_file_path)
plot(pca_output_image, main = "Principal Components", nc = 5, maxnl = 30)  # maxnl allows all 30 layers to be shown


# Loop through each component, plot and save
output_plots <- list()
for (i in 1:nlyr(pca_output_image)) {
  pc_layer <- pca_output_image[[i]]  # Extract each PC as a separate layer

  # Plot each PC layer with the variance explained in the title
  plot(pc_layer,
       main = paste0("Principal Component ", i),
       col = terrain.colors(100))  # Customize colors if needed

  # Optionally, save each plot to a file


  output_file <-paste0(Output_Dir,TypePCA,"/PCA/PC_Plots/PCA_PC_", i, ".png")
  png(output_file, width = 800, height = 600)
  plot(pc_layer, main = paste0("PC ", i), col = terrain.colors(100))
  dev.off()

  # Store the plot output paths
  output_plots[[i]] <- output_file
}



# Select components from the PCA/SPCA/MNF raster
# Sel_PC = path of the file where selected components are stored
Sel_PC <- select_PCA_components(Input_Image_File = Input_Image_File,
                                Output_Dir = Output_Dir,
                                PCA_Files = PCA_Output$PCA_Files,
                                TypePCA = PCA_Output$TypePCA,
                                File_Open = TRUE)

################################################################################
##                  Perform Spectral species mapping                          ##
## https://jbferet.github.io/biodivMapR/articles/biodivMapR_5.html            ##
################################################################################
print("MAP SPECTRAL SPECIES")
Kmeans_info <- map_spectral_species(Input_Image_File = Input_Image_File,
                                    Input_Mask_File = PCA_Output$MaskPath,
                                    Output_Dir = Output_Dir,
                                    SpectralSpace_Output = PCA_Output,
                                    nbclusters = nbclusters,
                                    nbCPU = nbCPU, MaxRAM = MaxRAM)

################################################################################
##                Perform alpha and beta diversity mapping                    ##
## https://jbferet.github.io/biodivMapR/articles/biodivMapR_6.html            ##
################################################################################
print("MAP ALPHA DIVERSITY")
Index_Alpha   = c('Shannon','Simpson')
#Index_Alpha <- c('Shannon')
map_alpha_div(Input_Image_File = Input_Image_File,
              Output_Dir = Output_Dir,
              TypePCA = TypePCA,
              window_size = window_size,
              nbCPU = nbCPU,
              MaxRAM = MaxRAM,
              Index_Alpha = Index_Alpha,
              nbclusters = nbclusters)

print("MAP BETA DIVERSITY")
map_beta_div(Input_Image_File = Input_Image_File,
             Output_Dir = Output_Dir,
             TypePCA = TypePCA,
             window_size = window_size,
             nbCPU = nbCPU,
             MaxRAM = MaxRAM,
             nbclusters = nbclusters)

################################################################################
##                  Perform Functional Diversity mapping                      ##
## https://jbferet.github.io/biodivMapR/articles/biodivMapR_7.html            ##
##          (Villeger et al, 2008 https://doi.org/10.1890/07-1206.1)          ##
################################################################################
## read selected features from dimensionality reduction
Selected_Features <- read.table(Sel_PC)[[1]]
## path for selected components
map_functional_div(Original_Image_File = Input_Image_File,
                   Functional_File = PCA_Output$PCA_Files,
                   Selected_Features = Selected_Features,
                   Output_Dir = Output_Dir,
                   window_size = window_size,
                   nbCPU = nbCPU,
                   MaxRAM = MaxRAM,
                   TypePCA = TypePCA)

