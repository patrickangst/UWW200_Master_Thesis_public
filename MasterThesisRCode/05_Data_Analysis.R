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

# Define parameter script
source('00_Project_Parameter.R')

# input image folder path rectified
Datadir <- paste0(base_path,'/data/rectified')
# name of the image file
NameRaster <- file_name_rectified
Input_Image_File <- file.path(Datadir,NameRaster)
Input_HDR_File <- get_HDR_name(Input_Image_File,showWarnings = FALSE)


dir.create(path = Datadir,recursive = T,showWarnings = F)

################################################################################
##                      Set parameters for biodivMapR                         ##
## https://jbferet.github.io/biodivMapR/articles/biodivMapR_2.html            ##
################################################################################
# Define path for image file to be processed

# Define path for corresponding mask file
# Set to FALSE if no mask available
#Input_Mask_File <- FALSE
Input_Mask_File <- paste0(base_path,'/mask/',mask_name)
# Define path for master output directory where files produced during the process are saved

# Master output directory (remove unnecessary line break)
Output_Dir <- paste0(base_path,'/result')
dir.create(path = Output_Dir, recursive = TRUE, showWarnings = FALSE)

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
nbCPU <- 4
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


# Save the list as an RDS file
pca_output_rds_file_path = paste0(Output_Dir,"/",NameRaster,"/",TypePCA,"/PCA/","PCA_Output.rds")
saveRDS(PCA_Output, file = pca_output_rds_file_path)

# Later, load the list back into R
PCA_Output <- readRDS(pca_output_rds_file_path)


# path for the updated mask
Input_Mask_File <- PCA_Output$MaskPath

var_exp <- (PCA_Output$PCA_model$sdev^2/sum(PCA_Output$PCA_model$sdev^2))*100
barplot(var_exp, names.arg = colnames(PCA_Output$PCA_model$x))

pca_output_image_file_path = paste0(Output_Dir,"/",NameRaster,"/",TypePCA,"/PCA/","OutputPCA_30_PCs")
print(pca_output_image_file_path)
pca_output_image <- rast(pca_output_image_file_path)
plot(pca_output_image, main = "Principal Components", nc = 5, maxnl = 30)  # maxnl allows all 30 layers to be shown


# Define the path for PCA plots
# Define the path for individual PCA component plots
pca_plots_file_path <- paste0(Output_Dir, "/", NameRaster, "/", TypePCA, "/PCA/PCA_Plots")

# Create the directory if it doesn't exist
if (!dir.exists(pca_plots_file_path)) {
  dir.create(pca_plots_file_path, recursive = TRUE, showWarnings = FALSE)
}


# Initialize a list to store output plot file paths
output_plots <- list()

# Loop through each layer in the PCA output image
for (i in 1:nlyr(pca_output_image)) {
  # Extract each PC layer as a separate raster
  pc_layer <- pca_output_image[[i]]

  # Define the output file path for the plot
  output_file <- paste0(pca_plots_file_path, "/PCA_PC_", i, ".png")

  # Save the plot as a PNG
  png(filename = output_file, width = 800, height = 600)
  plot(pc_layer,
       main = paste0("Principal Component ", i),
       col = terrain.colors(100))  # Customize colors if needed
  dev.off()  # Close the PNG device

  # Store the output file path
  output_plots[[i]] <- output_file
}

# Optional: print confirmation
print("PCA plots saved to:")
print(pca_plots_file_path)



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
# Selected_Features <- read.table(Sel_PC)[[1]]
# ## path for selected components
# map_functional_div(Original_Image_File = Input_Image_File,
#                    Functional_File = PCA_Output$PCA_Files,
#                    Selected_Features = Selected_Features,
#                    Output_Dir = Output_Dir,
#                    window_size = window_size,
#                    nbCPU = nbCPU,
#                    MaxRAM = MaxRAM,
#                    TypePCA = TypePCA)

################################################################################
##            Perform validation based on a vectorized plot network           ##
## https://jbferet.github.io/biodivMapR/articles/biodivMapR_8.html            ##
################################################################################
# location of the directory where shapefiles used for validation are saved
# VectorDir <- destunz
# # list vector data
# Path_Vector <- list_shp(VectorDir)
# Name_Vector <- tools::file_path_sans_ext(basename(Path_Vector))
# location of the spectral species raster needed for validation
# Path_SpectralSpecies <- Kmeans_info$SpectralSpecies
# # get diversity indicators corresponding to shapefiles (no partitioning of spectral dibversity based on field plots so far...)
# Biodiv_Indicators <- diversity_from_plots(Raster_SpectralSpecies = Path_SpectralSpecies,
#                                           Plots = Path_Vector,
#                                           nbclusters = nbclusters,
#                                           Raster_Functional = PCA_Output$PCA_Files,
#                                           Selected_Features = Selected_Features)
#
# Shannon_RS <- c(Biodiv_Indicators$Shannon)[[1]]
# FRic <- c(Biodiv_Indicators$FunctionalDiversity$FRic)
# FEve <- c(Biodiv_Indicators$FunctionalDiversity$FEve)
# FDiv <- c(Biodiv_Indicators$FunctionalDiversity$FDiv)
# # if no name for plots
# Biodiv_Indicators$Name_Plot = seq(1,length(Biodiv_Indicators$Shannon[[1]]),by = 1)
#
# # write a table for Shannon index
# Path_Results <- file.path(Output_Dir,NameRaster,TypePCA,'VALIDATION')
# dir.create(Path_Results, showWarnings = FALSE,recursive = TRUE)
# write.table(Shannon_RS, file = file.path(Path_Results,"ShannonIndex.csv"),
#             sep="\t", dec=".", na=" ", row.names = Biodiv_Indicators$Name_Plot, col.names= F,quote=FALSE)
#
# # write a table for all spectral diversity indices corresponding to alpha diversity
# Results <- data.frame(Name_Vector, Biodiv_Indicators$Richness, Biodiv_Indicators$Fisher,
#                       Biodiv_Indicators$Shannon, Biodiv_Indicators$Simpson,
#                       Biodiv_Indicators$FunctionalDiversity$FRic,
#                       Biodiv_Indicators$FunctionalDiversity$FEve,
#                       Biodiv_Indicators$FunctionalDiversity$FDiv)
# names(Results)  = c("ID_Plot", "Species_Richness", "Fisher", "Shannon", "Simpson", "FRic", "FEve", "FDiv")
# write.table(Results, file = paste(Path_Results,"AlphaDiversity.csv",sep=''),
#             sep="\t", dec=".", na=" ", row.names = F, col.names= T,quote=FALSE)
#
# # write a table for Bray Curtis dissimilarity
# BC_mean <- Biodiv_Indicators$BCdiss
# colnames(BC_mean) <- rownames(BC_mean) <- Biodiv_Indicators$Name_Plot
# write.table(BC_mean, file = paste(Path_Results,"BrayCurtis.csv",sep=''),
#             sep="\t", dec=".", na=" ", row.names = F, col.names= T,quote=FALSE)
#
# # apply ordination using PCoA (same as done for map_beta_div)
# MatBCdist <- as.dist(BC_mean, diag = FALSE, upper = FALSE)
# BetaPCO <- labdsv::pco(MatBCdist, k = 3)
#
# # assign a type of vegetation to each plot, assuming that the type of vegetation
# # is defined by the name of the shapefile
# nbSamples <- shpName <- c()
# for (i in 1:length(Path_Vector)){
#   shp <- Path_Vector[i]
#   nbSamples[i] <- nrow(sf::read_sf(shp))
#   shpName[i] <- tools::file_path_sans_ext(basename(shp))
# }
#
# Type_Vegetation = c()
# for (i in 1: length(nbSamples)){
#   for (j in 1:nbSamples[i]){
#     Type_Vegetation = c(Type_Vegetation,shpName[i])
#   }
# }
#
# # create data frame including a selection of alpha diversity metrics and beta diversity expressed as coordinates in the PCoA space
# Results <- data.frame('vgtype'=Type_Vegetation,'pco1'= BetaPCO$points[,1],'pco2'= BetaPCO$points[,2],'pco3' = BetaPCO$points[,3],
#                       'shannon'=Shannon_RS,'FRic' = FRic, 'FEve' = FEve, 'FDiv' = FDiv)
#
# # plot field data in the PCoA space, with size corresponding to shannon index
# g1 <-ggplot (Results, aes (x=pco1, y=pco2, color=vgtype,size=shannon)) +
#   geom_point(alpha=0.6) +
#   scale_color_manual(values=c("#e6140a", "#e6d214", "#e68214", "#145ae6"))
#
# g2 <-ggplot (Results, aes (x=pco1, y=pco3, color=vgtype,size=shannon)) +
#   geom_point(alpha=0.6) +
#   scale_color_manual(values=c("#e6140a", "#e6d214", "#e68214", "#145ae6"))
#
# g3 <-ggplot (Results, aes (x=pco2, y=pco3, color=vgtype,size=shannon)) +
#   geom_point(alpha=0.6) +
#   scale_color_manual(values=c("#e6140a", "#e6d214", "#e68214", "#145ae6"))
#
# #extract legend
# #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
# get_legend <- function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)
# }
#
# legend <- get_legend(g3)
# gAll <- grid.arrange(arrangeGrob(g1 + theme(legend.position="none"),
#                                  g2 + theme(legend.position="none"),
#                                  g3 + theme(legend.position="none"),
#                                  nrow=1),legend,nrow=2,heights=c(5, 4))
#
# filename <- file.path(Path_Results,'BetaDiversity_PcoA1_vs_PcoA2_vs_PcoA3.png')
# ggsave(filename, plot = gAll, device = 'png', path = NULL,
#        scale = 1, width = 12, height = 7, units = "in",
#        dpi = 600, limitsize = TRUE)
