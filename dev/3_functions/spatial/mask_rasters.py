import os
import glob
import geopandas as gpd
import rasterio
from rasterio.mask import mask

# # Path to directory with input rasters
# input_dir = "1_raw\\spatial\\mrlc_lulc\\conus"
# output_dir = "1_raw\\spatial\\mrlc_lulc\\neast"
# os.makedirs(output_dir, exist_ok=True)

def mask_rasters(input_dir, output_dir, aoi_path = '2_clean/spatial/neast_mask.gpkg'):
    '''
    Reduces folder of rasters to Northeast states using mask file and saves to
    a new folder.
    
    Args:
      input_dir (str): File path to where large conus rasters are kept
      output_dir (str): File path to where smaller Northeast rasters should go
      aoi_path (str): File path to a mask object of the Northeast
    
    Returns:
      Nothing. Just saves the rasters of the Northeast in the output folder.
    '''
    
    # Load area of interest
    aoi = gpd.read_file(aoi_path)
    aoi = aoi.to_crs("EPSG:4326")  # project to wgs 84 just in case

    # Convert geometries to GeoJSON-like dicts for rasterio.mask
    geoms = aoi.geometry.values
    geoms = [geom.__geo_interface__ for geom in geoms]

    # Get a list of all the tif files, print to check
    tif_files = glob.glob(os.path.join(input_dir, "*.tif"))
    print(tif_files)

    # Loop through all tif files, mask, save
    for tif in tif_files:
        with rasterio.open(tif) as src:

            # Reproject shapes just in case
            if src.crs != aoi.crs:
                aoi_proj = aoi.to_crs(src.crs)
                geoms_proj = [geom.__geo_interface__ for geom in aoi_proj.geometry]
            else:
                geoms_proj = geoms

            # Mask raster with AOI
            out_image, out_transform = mask(src, geoms_proj, crop=True)
            out_meta = src.meta.copy()

            # Update metadata
            out_meta.update({
                "driver": "GTiff",
                "height": out_image.shape[1],
                "width": out_image.shape[2],
                "transform": out_transform
            })

            # Output file path
            out_name = os.path.basename(tif).replace(".tif", "_masked.tif")
            out_path = os.path.join(output_dir, out_name)

            # Save the masked raster
            with rasterio.open(out_path, "w", **out_meta) as dest:
                dest.write(out_image)

    print("Masking complete. Northeast rasters saved in:", output_dir)
