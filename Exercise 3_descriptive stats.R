# DESCRIPTIVE STATISTICS
library(tidyverse)

# Take a subset of data using row name
Perthtemp <- AUcitytemp %>%
  filter(site=="Perth")

# Or by selecting row location
 Perthtemp <- AUcitytemp[3,]
 
# HISTOGRAM
# See the frequency of distribution of data
 
# Make a histogram of Perth's temperature, and Change the tibble to be numeric
hist(as.numeric(Perthtemp)) # Contains unwanted columns

# Remove unwanted 'ID' and site name columns by selecting desired column range
Perthtemp <- AUcitytemp %>%
  filter(site=="Perth")
timeframe <- Perthtemp %>%
  dplyr::select(Jan:Dec)

# Define bin widths/breaks in the histogram
userbreak <- c(8,10,12,14,16,18,20,22,24,26)

hist((as.numeric(timeframe)),
     breaks=userbreak,
     col="red",
     main="Histogram of Perth Temperature",
     xlab="Temperature",
     ylab="Frequency")

histinfo <- as.numeric(timeframe) %>%
  as.numeric() %>%
  hist(.)
histinfo

# Plot the outline geometry of Australia
plot(Australia$geom)

# Can also Simplify a shapefile to make it load faster
AustraliaSIMPLE <- Australia %>%
  st_simplify(., dTolerance = 1000) %>% # controls the level of generalisation in map units
  st_geometry() %>%
  plot()

# CLIP DATA TO MAP EXTENT

# First, check both layers are in the same CRS
print(Australia)
crs(WorldAverageTemp)

# Crop the temp data to shapefile extent
Austemp <- Australia %>%
  terra::crop(WorldAverageTemp,.)
plot(Austemp)

# Perfectly clip raster data to the Australia outline
ExactAustemp <- terra::mask(Austemp, Australia)
plot(ExactAustemp)

# Create a histogram of temperature in Aus in March
hist(ExactAustemp[[3]], col="red", main= "March Temperature")

# HISTOGRAM USING GGPLOT

# Raster data must be in data.frame format to be compatible with ggplot
ExactAusTempDF <- ExactAustemp %>%
  as.data.frame()

# plot a basic histogram using ggplot
library(ggplot2)
gghistogram <- ggplot(ExactAusTempDF, aes(x=Mar)) +
  geom_histogram(color="black",
                 fill="white") +
  labs(title="ggplot histogram of Australia March Temperatures",
       x="Temperature",
       y="Frequency")

# plot a vertical line showing mean March temperature
gghistogram + geom_vline(aes(xintercept=mean(Mar, na.rm=TRUE)),
                         colour = "blue",
                         linetype = "dashed",
                         linewidth = 1) +
  theme(plot.title = element_text(hjust = 0.5))

# Change data format to plot multiple months on histogram
squishdata <- ExactAusTempDF %>%
  pivot_longer(cols = 1:12,
               names_to = "Month",
               values_to = "Temp")

# Subset data, selecting months of choice
JanJundata <- squishdata %>%
  filter(., Month=="Jan" | Month=="Jun")

# Take the mean for each selected month
meanJanJun <- JanJundata %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))
meanJanJun
  
# Create a ggplot histogram of Australian Jan and June temps
ggplot(JanJundata, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram(position="identity", # Identity places bars on top of eachother
                 alpha=0.5) + # Slight transparency of bars
  geom_vline(data=meanJanJun,
             aes(xintercept=mean,
                 color=Month),
             linetype="dashed") +
  labs(title="GGplot histogran of Australian Jan and June temps",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5)) # Positions the title horizontally to the graph

# Create a faceted ggplot for all months (multiple subplots with the same set of axis)
AllMonthsFacetedPlot <- squishdata %>%
  drop_na() %>% # Remove all non-finite values from dataset
  mutate(Month = factor(Month,levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
ggplot(AllMonthsFacetedPlot, aes(x=Temp, na.rm=TRUE)) + # Ignore NA values to improve calculation accuracy
  geom_histogram(color="black", binwidth = 5) +
  labs(title="ggplot faceted histogram of Australian Temperatures",
       x="Temperature",
       y="Frequency") +
  facet_grid(Month ~ .) +
  theme(plot.title = element_text(hjust = 0.5))

# Create an interactive histogram of Jan/ Jun temps using plotly
library(plotly)

# Subset and assign data into selected months
jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")

jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")

# Set the axis titles
x <- list(title = "Temperature")
y <- list(title = "Frequency")

# Set the bin width
setbinwidth <- list(start=0, end=40, size=2.5) 

# Plot the histogram calling the set variables
interactivehist <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=setbinwidth, name = "January") %>%
  add_histogram(x = jun$Temp,
                xbins=setbinwidth, name = "June") %>%
  layout(barmode = "overlay", xaxis=x, yaxis=y)

interactivehist
# is good practice to pre-set variables and recall them when creating multiple plots with the same settings/aesthetics


# SIMPLE SUMMARY STATISTICS

# Calculate mean per month
meanPerMonth <- squishdata %>%
  group_by(Month) %>%
  summarise(mean = mean(Temp, na.rm=TRUE))
head(meanPerMonth, n=1) # show first result

# Calculate standard deviation per month
StandevALL <- squishdata %>%
  group_by(Month) %>%
  summarise(sd = sd(Temp, na.rm=TRUE))

# Calculate the maximum per month
Maxpermonth <- squishdata %>%
  group_by(Month) %>%
  summarise(max=max (Temp, na.rm=TRUE))

# Calculate minimum per month
Minpermonth <- squishdata %>%
  group_by(Month) %>%
  summarise(min=min (Temp, na.rm=TRUE))

# Calculate the interquartile range per month
IQRpermonth <- squishdata %>%
  group_by(Month) %>%
  summarise(IQR=IQR (Temp, na.rm=TRUE))

# Store multiple outputs in one list
statslist <- squishdata %>%
  group_by(Month) %>%
  summarise(IQR=IQR(Temp, na.rm=TRUE),
            max=max(Temp, na.rm=TRUE),
            min=min(Temp, na.rm=TRUE),
            mean=mean(Temp, na.rm=TRUE))

# Calculate mean (or any stat) for the whole year
meanhwoleyear <- squishdata %>%
  summarise(meanyear = mean(Temp, na.rm=TRUE))











