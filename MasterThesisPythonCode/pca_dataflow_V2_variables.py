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
import pca_dataflow_V2_variables as vb
from scipy.ndimage import generic_filter
from joblib import Parallel, delayed

input_dir = "data" 
file = os.path.join(input_dir, "ang20180729t212542_rfl_v2r2_img")
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