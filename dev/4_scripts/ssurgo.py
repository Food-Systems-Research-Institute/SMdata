import pandas as pd
import geopandas as gpd
import rasterio
import matplotlib as plt

raster_path='1_raw/ssurgo/gSSURGO_VT.gdb/MURASTER_10m'
raster_path

# # Use rasterio to list all subdatasets
# with rasterio.open(raster_path) as src:
#     print(src.subdatasets)
    
with rasterio.open(raster_path) as raster:
    data = raster.read(1)
    plt.imshow(data, cmap='terrain')
    plt.colorbar()
    plt.title('Raster from GDB')
    plt.show()


raster = gpd.read_file('1_raw/ssurgo/gSSURGO_VT.gdb/', layer='MURASTER_10m')
raster = gpd.read_file('1_raw/ssurgo/gSSURGO_VT.gdb/')
raster = gpd.read_file('1_raw/ssurgo/ssurgo_raster.tif')


print(raster)
raster.head()


import rasterio

# Path to your raster
raster_path = '1_raw/ssurgo/ssurgo_raster.tif'

# Open the raster
with rasterio.open(raster_path) as src:
    print("CRS:", src.crs)           # Coordinate Reference System
    print("Bounds:", src.bounds)     # Spatial extent
    print("Width, Height:", src.width, src.height)
    print("Number of bands:", src.count)

    # Read the first band
    band1 = src.read(1)


# `band1` is a NumPy array
print(band1.shape)

raster.plot()

print(raster)

# This is the same thing we already had - just identifiers
