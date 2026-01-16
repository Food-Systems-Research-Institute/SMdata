import rasterstats
import geopandas as gpd
import rasterio
import pandas as pd

## Function to aggregate raster data by polygons
def raster_mean_by_polygon(polygon_path, raster_path, new_name):
    
    # Get raster crs
    with rasterio.open(raster_path) as src:
        raster_crs = src.crs
    
    # Load polygons shapefile
    polygons = gpd.read_file(polygon_path)

    # If CRS do not match, convert polygons to CRS of raster
    if polygons.crs != raster_crs:
        polygons = polygons.to_crs(raster_crs)
    
    # Calculate mean of raster value within each polygon
    out = rasterstats.zonal_stats(
      polygons, 
      raster_path, 
      stats='mean',
      geojson_out=True
    )
    
    # Return a data frame
    df = gpd.GeoDataFrame.from_features(out)
    df.set_crs(polygons.crs, inplace=True)
    df = pd.DataFrame(df)
    df = df[['fips', 'mean']]
    df.rename(columns={'mean': new_name}, inplace=True)
    return df
