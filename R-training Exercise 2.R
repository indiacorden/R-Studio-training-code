# INTRODUCTION TO R: MANIPULATING DATA TABLES

# Set working directory
setwd("/Users/indiacorden/Documents/R-Studio training")

A <- 1
B <- 2
C <- A+B

# install and load relevant packages
library(readr)
library(tidyverse)
library(RSQLite)
library(dplyr)

# Read in csv file
flytipping <- read_csv("~/Documents/R-Studio training/R training/Exercise 1/fly-tipping-borough.csv")  

# Read on csv file and force columns to be the appropriate data types
flytipping1 <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv",
                        col_types = cols(
                          code = col_character(),
                          total_incidents = col_number(),
                          total_action_taken = col_number(),
                          warning_letters = col_number(),
                          fixed_penalty_notices = col_number(),
                          statutory_notices = col_number(),
                          formal_cautions = col_number(),
                          injunctions = col_number(),
                          prosecutions = col_number()))
                        
# create some datasets, first a vector of 1-100 and 101-200
Data1 <- c(1:100) # c() concatenates a string of numbers together into a vector
Data2 <- c(101:200) # produce the integers between and including 101 - 200
# Plot the data
plot(Data1, Data2, col="red") # plots 2 data objects in a given colour

# create some normally distributed vectors of 100 numbers
Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean = 64, sd=14)
#plot
plot(Data3, Data4, col="blue")

# Alternatively put data into a data frame (table) and plot
df <- data.frame(Data1, Data2)
plot(df, col="green")

# Can select elements/subsets of a dataframe using data.frame[row,column ]

df[1:10, 1] # returns 1st to 10th value in data1 column
df[5:15,] # returns 5 to 15 in data1 and data2 columns
df[c(2,3,6),2] # returns concatenated vector of the 2, 3, and 6 value in data2 column
df[,1] # returns all values in data1 column

# change the data column names to differ from original assigned objects
df <- df %>%
  dplyr::rename(column1 = Data1, column2 = Data2)

# Select or refer to columns directly by name
df %>% 
  dplyr::select(column1) # using :: ensures select function comes from the right package (dplyr)

# use either of these other methods when changing name of raster data
# df$column1 
# df[["column1"]] 


# Read in London ward data using old school method, data is already cleaned via excel
LondonData<- read.csv("~/Documents/R-Studio training/R training/Exercise 2/London_ward_data.csv", 
                         header = TRUE, # First row of the file contains header information
                         sep = ",",  # values in the file are separated with commas
                         encoding = "latin1") # Encoding Latin1 means each character is 1 byte long. The standard for R.

# Read in files a more straightforward way, avoiding use of /
install.packages("here")
library(here)
LondonData2<- read.csv(here::here("R training", "Exercise 2", "London_ward_data.csv"), 
                         header = TRUE, sep = ",",  
                         encoding = "latin1")

# Read in uncleaned London ward data straight from web, skipping over N/A entries as it goes
LondonData3 <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")

# Check what data type your data is 
class(LondonData3)

# check that our data has been read in correctly and that, 
# for example, numeric data haven’t been read in as text or other variables
Datatypelist <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

# Quickly and easily summarise the data frame
summary(df) # will generate a min, max, mean, median, quantiles
summary(LondonData)

# To just look at the first 5 headers of the data frame
LondonData%>%
  colnames()%>%
  head()

# To look at the last 5 rows
LondonData %>%
  tail()

# SELECTING ROWS

# Select London Borough rows from data frame by specifying range
LondonBoroughs <- LondonData[626:658,]

# Or, select London boroughs by taking a 'slice' out of the dataset
LondonBoroughs2<-LondonData%>%
  dplyr::slice(626:658)

# Or, select London boroughs by filtering data frame based on condition
install.packages("stringr")
library(stringr) # as London borough codes are in String format
LondonBoroughs3<- LondonData %>% 
  filter(str_detect(`New.code`, "^E09"))

# Extract only unique rows using
LondonBoroughs<-LondonBoroughs %>%
  distinct()


# Check ward name column to check Boroughs were picked out
LondonBoroughs3$Ward.name

# or 

LondonBoroughs %>% 
  dplyr::select(`Ward.name`) %>%
  print()

# Extract all wards where female life expectancy is greater than 90
Femalelifeexp<- LondonData %>% 
  filter(`Female.life.expectancy..2009.13`>90)

# SELECTING COLUMNS

# select columns 1,19,20 and 21
LondonBoroughs_manualcols<-LondonBoroughs[,c(1,19,20,21)]

LondonBoroughs_dplyrcols<-LondonBoroughs %>%
  dplyr::select(c(1,19,20,21))

# Or select columns that contain certain words, e.g:
LondonBoroughs_contains<-LondonBoroughs %>% 
  dplyr::select(contains("expectancy"), 
                contains("obese...2011.12.to.2013.14"),
                contains("Ward.name")) 

# Rename the wards column to boroughs
library(janitor) # Removes all capitals and uses _ instead of space

LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough='Ward.name')%>%
  clean_names(.,case="big_camel") # Big camel capitalises words

LondonBoroughs1 <- LondonBoroughs1 %>%
  dplyr::rename(Borough='Ward.name')
  
# Determine the average of male and female life expectancy together
# by creating a new variable from existing ones

# First Check column types 
library(dplyr)
LondonBoroughs %>%
  summarize_all(is.character)

# And change characters to string of desired columns
LondonBoroughs <- LondonBoroughs %>%
  mutate(across(c(FemaleLifeExpectancy2009_13, MaleLifeExpectancy2009_13), as.integer))

# ^ use "Across" within mutate() or summarise() to apply functions to a selection of columns

Life_expectancy <- LondonBoroughs %>%
# Create new column with average life expectancy
mutate(average_life_expectancy=(FemaleLifeExpectancy2009_13 + MaleLifeExpectancy2009_13)/2) %>%

# Create new column with normalised life expectancy
mutate(normalised_life_expectancy=average_life_expectancy/mean(average_life_expectancy)) %>%

# Select only the columns we want
dplyr::select(Borough, NewCode,average_life_expectancy, normalised_life_expectancy) %>%

# Arrange normalised life expectancy in descending order
arrange(desc(normalised_life_expectancy))

# Show the first 5 rows 
slice_head(Life_expectancy, n=5)

# Show the last 5 rows
slice_tail(Life_expectancy,n=5)

# DPLYR CAPABILITIES

# Compare London borough life expectancy data with UK average
Life_expectancy2 <- Life_expectancy %>%
  mutate(UKcomparison=case_when(average_life_expectancy>81.16 ~ "Above UK average", TRUE ~ "Below UK average"))
Life_expectancy2

# Calculate the range of London borough life expectancies above the UK average
Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdifference = average_life_expectancy-81.16) %>%
  group_by(UKcomparison)%>%
  summarise(Range = max(UKdifference) - min(UKdifference), Count=n(), Average=mean(UKdifference))
Life_expectancy2_group

# Calculate the distribution of Boroughs compared to the national average
Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdifference = average_life_expectancy-81.16) %>%
  mutate(across(where(is.numeric), round, 3))%>%
  mutate(across(UKdifference, round, 0))%>%
  mutate(UKcomparison = case_when(average_life_expectancy >= 81 ~ # Create a new column
                                    str_c("Equal to or above UK average by", # join 2 or more vector elements into a single character vector
                                          UKdifference, "years", sep = " "), # sep determines how the vectors are seperated
                                  TRUE ~ str_c("Below UK average by",
                                               UKdifference, "years", sep = " ")))%>%
  group_by(UKcomparison)%>%
  summarise(count=n())

# Calculate the difference between average life expectancy of each London Boroughc compared to UK avergae
Life_expectancy4 <- Life_expectancy %>%
  mutate(UKdifference = average_life_expectancy-81.16) %>%
  mutate(across(where(is.numeric), round, 3))%>%
  mutate(across(UKdifference, round, 0))

# Plot this data ($ means plot certain column within given dataframe)
plot(LondonBoroughs1$Male.life.expectancy..2009.13, 
     LondonBoroughs1$X..children.in.reception.year.who.are.obese...2011.12.to.2013.14)
  
# Improve graph appearance using plotly package
install.packages("plotly")
library(plotly)
  
plot_ly(LondonBoroughs1, x = ~Male.life.expectancy..2009.13, 
        y = ~X..children.in.reception.year.who.are.obese...2011.12.to.2013.14,
        text = ~Borough, type = "scatter", mode = "markers") %>%
  # Set a plot title and change the x and y axis labels
  layout(title = 'Male life expectancy v child obesity', xaxis = list(title = 'Male life expectancy 2009-13'), 
         yaxis = list(title = 'Child obesity 2011/12 - 2020'))












