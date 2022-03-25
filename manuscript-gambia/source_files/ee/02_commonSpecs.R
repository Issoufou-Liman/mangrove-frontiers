# Common scale for aggregation
AgScale <- 30;

# Temporal scale for composite
nDays <- 30;

# Lags days dependency
lagDays <- nDays + 1;
# print(lagDays)

# The number of cycle per year
harmonics <- 1; 

# The number of classes for unsupervised class
nClass <- 14;

# The of points for stratified sampling 
numPoints = 1000;

# # Define a palette for the 18 distinct land cover classes.
myPalette = c(
  '6495ed', #  1: water (assume standard wetlands colour) -> cornflowerblue
  'aec3d4', #  2: sea water (assume standard water colour)
  '152106', #  3: closed forest
  '225129', #  4: opened forest
  '369b47', #  5: woody sannava
  '387242', #  6: savanna
  '6a2325', #  7: closed shrubland
  'b76031', #  8: opened shrubland
  '91af40', #  9: grassland
  '111149', # 10: wetlands
  'cdb33b', # 11: croplands
  'cc0013', # 12: urban
  '33280d', # 13: crop mosaic
  '808080', # 14: barren -> gray
  'ffc0cb', # 15: mangroves -> pink
  '14566f' # 16: riperian vegetation
)

myPalette <- paste0('#', myPalette)

className <- c(
  'Fresh waters',
  'Salty waters',
  'Closed forests',
  'Open forests',
  'Woody savannas',
  'Savannas',
  'Closed shrublands',
  'Opened shrublands',
  'Grasslands',
  'Wetlands',
  'Croplands',
  'Built-up areas',
  'Mosaics',
  'Barren',
  'Mangroves',
  'Riperian vegetation'
)

maxpixels =   5100000

## Change direction ####

dpi = 300

# magnitudeClassName <- c(
#   'Increase',
#   'Decrease',
#   'Stable'
# )
# myMagnitudePalette <- c(
#   'green', # Icrease 
#   'red',   # Decrease
#   'blue'   # Stable
# )

#### Commons specs ####

checkPointGambia = ee$Geometry$Point(list(-15.676307243695224,13.36767864351738));

roiGambia = ee$Geometry$Polygon(
  list(list(list(-16.817361363458062, 13.826892060370968),
            list(-16.817361363458062, 13.064653321558035),
            list(-13.79092939346374, 13.064653321558035),
            list(-13.79092939346374, 13.826892060370968))), NULL, FALSE);
# print(roiGambia)

# Export$table$toDrive(
#   collection = ee$FeatureCollection(list(ee$Feature(roiGambia))), 
#   description = 'roiGambia', 
#   folder = 'Gambia',
#   fileFormat = 'SHP', 
# )

# ee_drive_to_local (
#   ee_collection = ee$FeatureCollection(list(ee$Feature(roiGambia))),
#   drive_description = "roiGambia",
#   drive_folder = "Gambia",
#   local_folder = 'ee_output',
#   overwrite = overwrite_file)
lapply(list('.shp', '.shx', 'prj', 'dbf', 'cpg'), function(i){
  ee_to_drive_to_local (
    ee_object = ee$FeatureCollection(list(ee$Feature(roiGambia))),
    drive_description = '~/Gambia_Manuscript/roiGambia', 
    drive_folder = 'Gambia_Manuscript', 
    region = roiGambia, 
    scale = AgScale,
    maxPixels = 1e13, 
    local_folder  = 'ee_output/roiGambia',
    overwrite = overwrite_file,
    ee_type = 'table',
    file_extension = i
  )
})
## center map to the region of interest
Map$centerObject(roiGambia, 9.2)
