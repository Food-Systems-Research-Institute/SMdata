import rasterstats
import geopandas as gpd
import rasterio
import pandas as pd

raster_path = '1_raw/spatial/prism//PRISM_ppt_stable_4kmM3_2024_bil/PRISM_ppt_stable_4kmM3_2024_bil.bil'

with rasterio.open(raster_path) as src:
    raster_crs = src.crs
    print(raster_crs)
