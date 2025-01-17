from spectral import *
import spectral.io.envi as envi
import os
import matplotlib.pyplot as plt
import numpy as np


input_dir = "data/e_ang20180722t212222rfl" 
file = os.path.join(input_dir, "ang20180722t212222_rfl_v2r2_img")
hdr_file = f"{file}.hdr"

# Check if files exist
if os.path.exists(file):
    print(f"File exists: {file}")
else:
    print(f"File not found: {file}")

if os.path.exists(hdr_file):
    print(f"HDR File exists: {hdr_file}")
else:
    print(f"HDR File not found: {hdr_file}")

img = envi.open(hdr_file, file)

img_rgb = get_rgb(img,(19,34,57))

view_cube(img_rgb)