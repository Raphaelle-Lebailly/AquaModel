setwd("C:/Users/User/Desktop/Internship/Data")


# Packages ----------------------------------------------------------------
library(tidyverse)
# library(tiff)
library(raster)
library(CoordinateCleaner) # Cleaning the data
library(sf) 
library(cowplot)
library(terra)
library(geodata)
library(maps)
library(rgbif)
library(ggplot2)
library(RColorBrewer)
library(letsR)
# path <-  "C:/Users/User/Desktop/Internship/Data/Climate"


# Data importation --------------------------------------------------------
# Use Geodata package to import data

# Geographical data
borders.vnm <- gadm(country = "VNM", level = 0, path=tempdir()) # Borders
# Climate data
temp.min <- worldclim_country("Vietnam", var = "tmin", res = 2.5, path=tempdir()) # Min temperature

## Per seasons
vnm.temp.spring <- mean(temp.min[[3]], temp.min[[4]], temp.min[[5]])
vnm.temp.summer <- mean(temp.min[[6]], temp.min[[7]], temp.min[[8]])
vnm.temp.autumn <- mean(temp.min[[9]], temp.min[[10]],temp.min[[11]])
vnm.temp.winter <- mean(temp.min[[12]], temp.min[[1]], temp.min[[2]])

seas <- list(vnm.temp.spring, vnm.temp.summer, vnm.temp.autumn, vnm.temp.winter)

# Species data
mola <- occ_data(scientificName = "Amblypharyngodon mola") # Test with 'mola' species
# Meta and data
mola.df <- mola$data


# Overlay -----------------------------------------------------------------
name.seas <- c("Spring", "Summer", "Autumn", "Winter")

# Temperature range
range.temp <- as.data.frame(minmax(temp.min))
min.temp <- min(range.temp)
max.temp <- max(range.temp)

# Plot
x11()
par(mfrow=c(2,2))
for (i in seq_along(seas)) {
  ov <- mask(seas[[i]], borders.vnm)
  plot(ov, zlim=c(min.temp,max.temp),
       main = paste("Average min temperature in Vietnam in", name.seas[i]))
  map("world", add=TRUE)
}


# Dataframe ---------------------------------------------------------------

# All data
temp.min.df <- as.data.frame(temp.min, xy = TRUE)

# Per season
vnm.temp.spring.df <- as.data.frame(vnm.temp.spring, xy = TRUE)
names(vnm.temp.spring.df)[3] <- "value"
vnm.temp.summer.df <- as.data.frame(vnm.temp.summer, xy = TRUE)
names(vnm.temp.summer.df)[3] <- "value"
vnm.temp.autumn.df <- as.data.frame(vnm.temp.autumn, xy = TRUE)
names(vnm.temp.autumn.df)[3] <- "value"
vnm.temp.winter.df <- as.data.frame(vnm.temp.winter, xy = TRUE)
names(vnm.temp.winter.df)[3] <- "value"

list.seas.df <- list(vnm.temp.spring.df, vnm.temp.summer.df, vnm.temp.autumn.df, vnm.temp.winter.df)


# Snap to grid ------------------------------------------------------------
ext.temp <- ext(temp.min) # Extent for data (terra object)
xmin <- ext.temp$xmin
ymin <- ext.temp$ymin

# [(x-xmin)/delta(x) +0.5]

# Snap to grid function
SnapToGrid <- function(layer){
  # Resolution
  DimLayer <- dim(layer)
  ResX <- DimLayer[1]
  ResY <- DimLayer[2]
  
  # Extent
  ext.layer <- ext(layer) # Extent for data (terra object)
  xmin <- ext.layer$xmin
  ymin <- ext.layer$ymin
  xmax <- ext.layer$xmax
  ymax <- ext.layer$ymax
  
  #Delta
  deltaX <- (xmax - xmin)/ResX
  deltaY <- (ymax - ymin)/ResY
  
  # Create grid
  coordGrid <-list()
  listX <- c()
  listY <- c()
  for(x in 1:ResX){
    Xgrid <- as.integer(((x-xmin)/deltaX) + 0.5)
    listX[x] <- c(listX[x-1],Xgrid)
    for(y in 1:ResY){
      Ygrid <- as.integer(((y-ymin)/deltaY) + 0.5)
      listY[y] <- c(listY[y-1],Ygrid)
    }
    coordGrid <-list(listX,listY)
  } 
  return(coordGrid)
}

# Test the function
SnapToGrid(temp.min)  

# Data cleaning -----------------------------------------------------------
# With CoordinateCleaning package --> Standardized cleaning
is.spatialvalid(mola$data) # Check for valid coordinates

