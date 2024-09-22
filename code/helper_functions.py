from osgeo import gdal,ogr,osr
import os

# get and return te crs of the hyperspectral image
def return_hsi_projection(img_open):
    # Get the projection
    projection = img_open.GetProjection()

    # Create a spatial reference object
    srs = osr.SpatialReference()
    srs.ImportFromWkt(projection)

    # Print the projection in various formats
    #print("WKT Projection:")
    #print(srs.ExportToWkt())

    #print("Proj4 Projection:")
    #print(srs.ExportToProj4())

    srs.AutoIdentifyEPSG()
    hs_crs = srs.GetAuthorityCode(None)
    
    return hs_crs


def get_subzone_objects(source_folder_path,subzone):

    hsi_source_folder_path = os.path.join(source_folder_path,subzone,"hsi")
    plots_source_folder_path = os.path.join(source_folder_path,subzone,"plots")
    
    print(f"HSI Source folder path: {hsi_source_folder_path}")
    print(f"Plots Source folder path: {plots_source_folder_path}")
    # List only directories within the target folder
    hsi_toplevel_folders = [name for name in os.listdir(hsi_source_folder_path) if os.path.isdir(os.path.join(hsi_source_folder_path, name))]

    # Initialize an empty dictionary to store folder names and their corresponding GDAL objects
    hsi_img_object = {}

    # Iterate over each subfolder
    for folder in hsi_toplevel_folders:
        folder_path = os.path.join(hsi_source_folder_path, folder)
        
        # Initialize variables to store file paths
        file_without_extension = None
        hdr_file = None

        # Iterate over all files in the folder
        for file_name in os.listdir(folder_path):
            file_path = os.path.join(folder_path, file_name)

            # Ensure it's a file and not a directory
            if os.path.isfile(file_path):
                
                # Check if the file has no extension
                if '.' not in file_name:
                    file_without_extension = file_path
                    #print(f"Found file without extension: {file_without_extension}")
                
                # Check if the file has a .hdr extension
                elif file_name.endswith('.hdr'):
                    hdr_file = file_path
                    #print(f"Found .hdr file: {hdr_file}")

        # If both files are found, open the file without extension using GDAL
        if file_without_extension and hdr_file:
            try:
                # Open the file without extension using GDAL
                img_open = gdal.Open(file_without_extension)
                
                # Store the folder name as key and the GDAL object as value in the dictionary
                hsi_img_object[folder] = img_open
                #print(f"Opened GDAL object for {file_without_extension} and stored as '{folder}'")
            
            except Exception as e:
                print(f"Error opening file {file_without_extension}: {str(e)}")
        else:
            print(f"Required files not found in folder: {folder}")

    # define plot site file path and read as a geopandas df
    plot_site_file = os.path.join(plots_source_folder_path,f"{subzone}_plots.shp")

    # open with ogr
    driver = ogr.GetDriverByName("ESRI Shapefile")
    plot_sites = driver.Open(plot_site_file, 0)
    
    return hsi_toplevel_folders, hsi_img_object, plot_sites
    # Now you have a dictionary 'hsi_img_object' with folder names and opened files
    #print(f"Total GDAL objects opened: {len(hsi_img_object)}")


# Dictionary containing band descriptions
band_dictionary = {
    "visible-violet": {'lower': 375, 'upper': 450, 'color': 'violet'},
    "visible-blue": {'lower': 450, 'upper': 485, 'color': 'blue'},
    "visible-cyan": {'lower': 485, 'upper': 500, 'color': 'cyan'},
    "visible-green": {'lower': 500, 'upper': 565, 'color': 'green'},
    "visible-yellow": {'lower': 565, 'upper': 590, 'color': 'yellow'},
    "visible-orange": {'lower': 590, 'upper': 625, 'color': 'orange'},
    "visible-red": {'lower': 625, 'upper': 740, 'color': 'red'},
    "near-infrared": {'lower': 740, 'upper': 1100, 'color': 'gray'},
    "shortwave-infrared": {'lower': 1100, 'upper': 2500, 'color': 'white'}
}

# Function to classify bands based on their wavelength
def classify_band(wavelength):
    for region, limits in band_dictionary.items():
        if limits['lower'] < wavelength <= limits['upper']:
            return region
    return None