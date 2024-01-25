import geopandas as gpd
from shapely import Point

shapefilePath = "../dzielnice_Warszawy/dzielnice_Warszawy.shp"
# Read shapefile
districts = gpd.read_file(shapefilePath)

# Transform to WGS84 coordinate system
districts_wgs84 = districts.to_crs(epsg=4326)


# Function to find district based on coordinates
def find_district(lat, lng):
    point = Point(lng, lat)
    for _, district in districts_wgs84.iterrows():
        if district['geometry'].contains(point):
            return district['nazwa_dzie']  # Replace 'nazwa_dzie' with the actual district name field
    return "No district found"



