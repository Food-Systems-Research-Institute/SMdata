import pandas as pd
import geopandas as gpd
import rasterstats
import rasterio

def cat_zonal_stats(raster_path, polygon_path, missing=None):
  '''
  Get a DataFrame of LULC category cell counts for each polygon. If the CRS of 
  each file do not match, the polygons will be projected into the CRS of the 
  raster.
  
  Args:
    raster_path (str): File path to a raster file (.tif)
    polygon_path (str): File path to a polygon (.gpkg, .shp)
  
  Returns:
    DataFrame
  '''
  
  # Load polygon file
  polygons = gpd.read_file(polygon_path)
  
  # Save CRS from raster as object without loading raster
  with rasterio.open(raster_path) as src:
    raster_crs = src.crs

  # If CRS is different, transform polygons to same crs as raster
  if polygons.crs != raster_crs:
    print(f'Transforming polygon CRS from {polygons.crs} to {raster_crs}')
    polygons = polygons.to_crs(raster_crs)

  # Calculate frequency of each LULC class within each polygon
  # Note that we use polygon file, but only raster path
  out = rasterstats.zonal_stats(
    polygons,
    raster_path,
    stats=None,
    categorical=True,
    nodata=missing
  )
  
  # Convert to dataframe, put fips column back in
  df = pd.DataFrame(out)
  df['fips'] = polygons['fips']
  return df
