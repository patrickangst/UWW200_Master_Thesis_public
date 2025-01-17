import os
from osgeo import gdal,ogr,osr
import numpy as np
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import skbio.diversity.alpha as alpha
import subprocess
import pickle
from sklearn.cluster import KMeans
import gc
from scipy.ndimage import generic_filter
from joblib import Parallel, delayed


#input_dir = "/Volumes/T9/new_pca_test/e_ang20180722t212222rfl/data/hs_raw_image" 
#file = os.path.join(input_dir, "ang20180722t212222_rfl_v2r2_img")
input_dir = "../aviris_data/e_done/e_ang20180722t212222rfl/data/hs_raw_image" 
file = os.path.join(input_dir, "ang20180722t212222_rfl_v2r2_img")
hdr_file = f"{file}.hdr"

def save_array(output_path, array, num_bands, gdal_dataset, gdal_format = "GTiff", NoDataValue = -9999, data_type=gdal.GDT_Float32):
    """
    Saves a NumPy array as to a file.

    Args:
        output_path (str): The path to save the GTiff or ENVI file.
        array (numpy.ndarray): The NumPy array to save.
        num_bands (integer): Number of bands.
        gdal_dataset (gdal.Dataset): The GDAL dataset to use for geospatial information.
        gdal_format (str, optional): The GDAL format to use (default: "GTiff").
        NoDataValue (str or number): No data value. Usually -9999, 0 or np.nan.
        data_type (gdal.Datatype): GDT_Float32 or GDT_Byte 
    """
    driver = gdal.GetDriverByName(gdal_format)

    out_raster = driver.Create(
        output_path,
        gdal_dataset.RasterXSize,
        gdal_dataset.RasterYSize,
        num_bands,
        data_type   
    )
    out_raster.SetGeoTransform(gdal_dataset.GetGeoTransform())
    out_raster.SetProjection(gdal_dataset.GetProjection())

    # Write the array band by band
    for i in range(num_bands):
        out_band = out_raster.GetRasterBand(i + 1)
        if num_bands == 1:
            out_band.WriteArray(array)
        else:
            #out_band.WriteArray(array[:, :, i])
            out_band.WriteArray(array[i])
        out_band.SetNoDataValue(NoDataValue)  # Or use 0 if appropriate
    out_raster.FlushCache()

# Function to calculate the mean for a range of bands
def calculate_band_average(img, band_range):
    band_arrays = []
    for band_index in band_range:
        band_data = img.GetRasterBand(band_index).ReadAsArray()
        band_data = np.where(band_data == -9999, np.nan, band_data)
        band_arrays.append(band_data)
    # Stack bands and calculate the mean along the 0th axis
    return np.nanmean(np.stack(band_arrays, axis=0), axis=0)

def calculate_shannon_diversity(window):
    """
    Calculate Shannon diversity index for a given window of cluster values.
    """
    # Remove no-data values
    valid_values = window[window != -9999]
    if len(valid_values) == 0:
        return -9999

    # Compute the frequency of each cluster
    unique, counts = np.unique(valid_values, return_counts=True)
    probabilities = counts / counts.sum()

    # Calculate Shannon diversity index
    shannon_diversity = -np.sum(probabilities * np.log(probabilities))
    return shannon_diversity


def create_shannon_diversity_map(input_file, output_file, window_size):
    """
    Generate a Shannon diversity map from a GeoTIFF containing cluster data.

    Args:
        input_file (str): Path to the input GeoTIFF file.
        output_file (str): Path to save the output GeoTIFF file.
        window_size (int): Window size for computing Shannon diversity.
    """
    # Open the input GeoTIFF
    dataset = gdal.Open(input_file)
    band = dataset.GetRasterBand(1)
    input_array = band.ReadAsArray()

    # Get geo-information from the input dataset
    geotransform = dataset.GetGeoTransform()
    projection = dataset.GetProjection()
    no_data_value = band.GetNoDataValue()

    # Apply the Shannon diversity calculation using a sliding window
    diversity_map = generic_filter(
        input_array,
        function=calculate_shannon_diversity,
        size=(window_size, window_size),
        mode='constant',
        cval=no_data_value
    )

    # Save the Shannon diversity map as a GeoTIFF
    driver = gdal.GetDriverByName("GTiff")
    out_dataset = driver.Create(
        output_file,
        dataset.RasterXSize,
        dataset.RasterYSize,
        1,
        gdal.GDT_Float32
    )
    out_dataset.SetGeoTransform(geotransform)
    out_dataset.SetProjection(projection)

    # Write the Shannon diversity data and set no-data value
    out_band = out_dataset.GetRasterBand(1)
    out_band.WriteArray(diversity_map)
    out_band.SetNoDataValue(-9999)

    # Save and close datasets
    out_band.FlushCache()
    out_dataset = None
    dataset = None
    print(f"Shannon diversity map saved to {output_file}")