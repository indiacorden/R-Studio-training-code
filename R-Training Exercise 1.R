# READING SHAPEFILES, CSVs, AND GEOPACKAGES

# install necessary packages for project
# install.packages(c("sf", "tmap", "tmaptools", "RSQLite", "tidyverse"),repos = "https://www.stats.bris.ac.uk/R/")
 
# Clear the workspace
rm(list_ls())


# Load sf package so we can read our shapefile in
library(sf)

# Set file path to read London Borough Shapefile
shape <- st_read("/Users/indiacorden/Documents/R-Studio training/R training/Exercise 1/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")

#get a summary of the data held within the shapefile data (attribute table)
summary(shape)

# have a look what the shapefile looks like
plot(shape)

# to just look at the geometry/shape outline
shape %>% 
  st_geometry() %>%
  plot()

# load tidyverse package so we can load in .csv file
library(tidyverse)

# set file path to read Fly Tipping Data, and skip 1 header row in file
mycsv <- read_csv("/Users/indiacorden/Documents/R-Studio training/R training/Exercise 1/fly_tipping_pivot.csv", skip = 1)

# to just view csv table:
mycsv

# To join the csv file with the shapefile, and replace Row labels with GSS_CODE
shape <- shape %>%
  merge(.,
        mycsv,
        by.x="GSS_CODE", 
        by.y="Row Labels")

# show the top 10 rows of dataset (to check they merged correctly)
shape %>%
  head(., n=10)

# load tmap package and plot merged fly tipping per borough data
library (tmap)
tmap_mode("plot") # Set tmap to plot

# have a look at the map 
shape %>%
  qtm(.,fill = "2011-12") +
  tm_layout(title = "Fly Tipping by Borough (2011-2012)", legend.position = c(1,0)) + # Add map title and move legend to bottom right
  tm_scale_bar(position=c("left","bottom")) # add scale bar and move to bottom left

# Export shape as a GeoPackage, with name of choice
shape %>%
  st_write(.,"/Users/indiacorden/Documents/R-Studio training/R training/Exercise 1/Fly_tipping_by_borough_r.gpkg",
           "Fly Tipping by Borough (2011-2012)",
           delete_layer=TRUE)

library(readr)
library(RSQLite)

# Connect to the .gpkg
con <- dbConnect(RSQLite::SQLite(),dbname="/Users/indiacorden/Documents/R-Studio training/R training/Exercise 1/Fly_tipping_by_borough_r.gpkg")

# Add the original .csv
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)

# List what is in the GeoPackage
con %>%
     dbListTables()

# Disconnect from the .gpkg
con %>% 
  dbDisconnect()

