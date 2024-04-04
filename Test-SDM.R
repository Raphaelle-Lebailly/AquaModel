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

# The goal is to build the grid scaffold from the environmental data layers to have a base
# and chose the adequate resolution. After that, the species data can be projected within the
# different grid cells (presence/absence) and filter the data rightfully.

# [(x-xmin)/delta(x) +0.5]

# Snap to grid function
SnapToGrid <- function(layer){
  df <- as.data.frame(layer,xy=T) # Use the xy dataframe and append the (x,y) values of each cell + index value
  # Resolution
  DimLayer <- dim(layer) 
  ResX <- DimLayer[1] # Resolution for x
  ResY <- DimLayer[2] # Resolution for y 
  
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
  df$snapX <- as.integer(((df$x-xmin)/deltaX) + 0.5)
  df$snapY <- as.integer(((df$y-ymin)/deltaY) + 0.5)
  len <- dim(df)
  df$index <- seq(1,len[1])
  return(df)
}


# Test the function
Grid <- SnapToGrid(temp.min) 
View(Grid)
# plot(Grid$snapX, Grid$snapY)
# range(Grid$snapY)


# Adjustment to cells -----------------------------------------------------

# If multiple data by cell, calculate the mean value and assign it to the cell
# What if NAs?

# Avoid loops and optimize my code

# Data cleaning -----------------------------------------------------------
# With CoordinateCleaning package --> Standardized cleaning

clean_coordinates(mola.df)

is.spatialvalid(mola$data) # Check for valid coordinates
is.spatialvalid(vnm.temp.autumn.df) 
