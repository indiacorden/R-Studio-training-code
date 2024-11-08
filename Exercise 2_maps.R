# CREATING THEMATIC MAPS USING TMAP

# Install relevant mapping packages
# install.packages("tmap")
# install.packages(c("tmaptools", "sf"))
# install.packages("OpenStreetMap")

# Load packages
library(tmap)
library(tmaptools)
library(sf)
library(dplyr)
library(tidyverse)
library(janitor)
library(here)
library(stringr)
# library(Rcpp)
# library(OpenStreetMap)

# Read in shapefile layer directly
Boroughshapefile <- st_read("/Users/indiacorden/Documents/R-Studio training/R training/Exercise 2/Local Authority Boundary data/LAD_Dec_2015_FCB_GB.shp")

# Read in shapefile layer using here
Boroughshapefile1 <-st_read(here::here("R training", "Exercise 2", "Local Authority Boundary data", "LAD_Dec_2015_FCB_GB.shp"))

# Extract London Boroughs from data set
LondonMap <- Boroughshapefile %>%
  filter(str_detect(lad15cd, "^E09"))

# Plot Map of London Boroughs
qtm(LondonMap)

# Join attribute data to London Borough shapefile based on common column
# 1. Using merge function
LondonData <- clean_names(LondonData) # Clean up data names
BoroughDataMap <- Boroughshapefile %>%
  clean_names() %>%
  filter(str_detect(lad15cd, "^E09")) %>% # Filter to only borough data
  merge(., LondonData, by.x="lad15cd", by.y="new_code", no.dups=TRUE) %>% # Merge data tables based on common E09 column
  distinct(.,lad15cd, .keep_all = TRUE) # Keep all other columns 
  
# 2. Using join function
BoroughDataMap2 <- Boroughshapefile %>%
  clean_names() %>%
  filter(str_detect(lad15cd, "^E09")) %>%
  left_join(., LondonData, by= c("lad15cd" = "new_code")) %>% # Left join adds data to table 1
  distinct(., lad15cd, .keep_all = TRUE)

# Create a simple choropleth map (using colour to represent data distribution)
tmap_mode("plot") # static
qtm(BoroughDataMap, fill="rate_of_job_seekers_allowance_jsa_claimants_2015")

# Extract an OSM basemap for london map
# tmaplondon <- BoroughDataMap %>%
#  st_bbox(.) %>% # create a bounding box around London
#  tmaptools::read_osm(., type ="osm", zoom = NULL)

# Plot static job seeker per borough map
tmap_mode("plot")
  tm_shape(BoroughDataMap) +
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015",
              style = "jenks",
              palette = "Blues",
              midpoint = NA,
              title = "Rate per 1,000 people")+
    tm_compass(position = c("left", "bottom"), type = "arrow") +
    tm_scale_bar(position = c("left", "bottom")) +
    tm_layout(title = "Job seekers Allowance Claimants",
              legend.position = c("right", "bottom"))
  
# Plot interactive job seeker per borough map
tmap_mode("view")
tm_shape(BoroughDataMap) +
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015",
              style = "jenks", n = 6,
              palette = "Blues",
              midpoint = NA,
              title = "Rate per 1,000 people",
              alpha = 0.5) +
  tm_compass(position = c("left", "bottom"), type = "arrow") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "job seekers', Allowance Claimants", legend.position = c("right", "bottom"))

# Join life expectancy data with borough shapefile
Life_expectancy4map <- Boroughshapefile %>%
  inner_join(., Life_expectancy4, by = c("lad15cd" = "NewCode")) %>%
  distinct(., lad15cd, .keep_all = TRUE)
  
# Plot Life expectancy per borough static map
tmap_mode("plot")
  tm_shape(Life_expectancy4map) + 
  tm_polygons("UKdifference", 
              style= "pretty", # rounds data to whole numbers and evenly spaces
              palette= "Blues",
              midpoint= NA,
              title= "Number of years") + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in life expectancy than UK average", legend.position = c("right", "bottom"))




