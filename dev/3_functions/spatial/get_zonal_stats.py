import rasterstats
import geopandas as gpd
import rasterio
import pandas as pd

## Function to aggregate raster data by polygons
def get_zonal_stats(raster_path, polygon_path, stat, new_name, missing=None):
    '''
    Run zonal_stats given a path to a raster and path to a polygon. 
    
    Args:
      raster_path (str): File path to a raster file (.tif)
      polygon_path (str): File path to a polygon (.gpkg, .shp)
      stat (str): Type of statistic to run (min, max, sum, median, mean)
      missing (str): Code for missing values to be removed.
    
    Returns:
      DataFrame
    '''
      
    # Get raster crs
    with rasterio.open(raster_path) as src:
        raster_crs = src.crs
        
    # Load polygons shapefile
    polygons = gpd.read_file(polygon_path)

    # If CRS do not match, convert polygons to CRS of raster
    if polygons.crs != raster_crs:
        polygons = polygons.to_crs(raster_crs)
    
    # Calculate stat of raster value within each polygon
    out = rasterstats.zonal_stats(
      polygons, 
      raster_path, 
      stats=stat,
      geojson_out=True,
      nodata=missing
    )
    
    # Return a data frame
    df = gpd.GeoDataFrame.from_features(out)
    df.set_crs(polygons.crs, inplace=True)
    df = pd.DataFrame(df)
    df = df[['fips', stat]]
    df.rename(columns={stat: new_name}, inplace=True)
    return df
