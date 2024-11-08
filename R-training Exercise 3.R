# COORDINATE REFERENCE SYSTSEMS

# Load relevant libraries
library(sf)
library(here)
library(tidyverse)

# Load geopackage and look inside it
st_layers(here("R training", "Exercise 3", "gadm41_AUS.gpkg"))

# Read in Australia layer
Australia <- st_read(here("R training", "Exercise 3", "gadm41_AUS.gpkg"), layer = 'ADM_ADM_0')

# Check CRS of data (2 ways)
# 1: 
print(Australia) 

# or 2: can be used to assign a CRS to a file that doesn't have one provided
st_crs(Australia)$proj4string 

# Manually Set Spatial Reference system code (ESPG)
Australia <- Australia %>%
st_set_crs(., 4326)

# Can re-project/ transform vector data into a different CRS
AustraliaProjected <- Australia %>%
  st_transform(.,3112)
print(AustraliaProjected)

# Transform sf objects to sp
AustraliaSP <- Australia %>%
  as(., "Spatial")

# Transform sp objects into sf
AustraliaSF <- AustraliaSP %>%
  st_as_sf()

# RASTER DATA

# Load in raster data
library(raster)
library(terra)

jantemperature <- terra::rast(here("R training", "Exercise 3","Climate data ","wc2.1_5m_tavg_01.tif"))

# View the raster layer
jantemperature

# Plot the raster data
plot(jantemperature)

# Reproject raster data into new CRS system (Mollweide projection)
rasterProjected <- terra::project(jantemperature, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")
plot(rasterProjected)

# Reproject back into WGS84 CRS
rasterProjected <- rasterProjected %>%
  terra::project(., "+proj=longlat +datum=WGS84 +no_defs +type=crs")
plot(rasterProjected)

# LOADING AND RENAMING RASTER DATA

# List all files stored within a stated directory
library(fs)
dir_info("R training/Exercise 3/Climate data /")

# select all tif files within the directory and extract them
listfiles <- dir_info("R training/Exercise 3/Climate data /") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path) %>% # using :: ensures select function comes from the right package (dplyr)
  pull()

# Look at the file names
listfiles
  
# Load all the extracted files into a 'SpatRaster' = a collection of raster layers 
# with the same spatial extent and resolution

WorldAverageTemp <- listfiles %>%
  terra::rast()
WorldAverageTemp

# Access a single layer within the stack (January)
WorldAverageTemp[[1]]

# Rename raster stack layers ('rename' from dplyr package is not available to use for raster data)
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(WorldAverageTemp) <- months

# Extract data for January
WorldAverageTemp$Jan

# Make a data frame of sample Australian sites
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )

# State their longitude and latitude position on the earth
long <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
          138.6, 153.43, 149.13, 151.78, 150.89, 153.12)

lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)

# Put all this information into one dataframe table
samplesites <- data.frame(site, long, lat, row.names="site")

# Extract the average temperature data from the raster stack for all points
AUcitytemp <- terra::extract(WorldAverageTemp, samplesites)

# Add the city names to the rows in AUcitytemp

AUcitytemp <- AUcitytemp %>%
  as_tibble() %>%
  add_column(Site=site, .before = "Jan")
  
          