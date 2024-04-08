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
library(rgbif) # Graph
library(ggplot2) # Graph
library(RColorBrewer) # Graph
library(letsR)
library(conflicted) # Look for conflicts for functions in different packages

# path <-  "C:/Users/User/Desktop/Internship/Data/Climate"


# Data importation --------------------------------------------------------
# Use Geodata package to import data

# Geographical data
borders.vnm <- gadm(country = "VNM", level = 0, path=tempdir()) # Borders, SPATVECTOR
# Climate data
temp.min <- worldclim_country("Vietnam", var = "tmin", res = 2.5, path=tempdir()) # Min temperature, SPATRASTER

# Species data
mola <- occ_data(scientificName = "Amblypharyngodon mola") # Test with 'mola' species, GBIF_DATA
# Meta and data
mola.df <- mola$data # Class "tbl_df"     "tbl"        "data.frame"



# Dataframe ---------------------------------------------------------------

# All data
temp.min.df <- as.data.frame(temp.min, xy = TRUE) # DATAFRAME


# Snap to grid ------------------------------------------------------------

# The goal is to build the grid scaffold from the environmental data layers to have a base
# and chose the adequate resolution. After that, the species data can be projected within the
# different grid cells (presence/absence) and filter the data rightfully.

# [(x-xmin)/delta(x) +0.5]

# Snap to grid function (manually)
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
temp.min.grid<- SnapToGrid(temp.min) 
View(temp.min.grid)
# plot(Grid$snapX, Grid$snapY)
# range(Grid$snapY)



# Subset Mean value ---------------------------------------------------------------------------

mean.df <- function(layer, arg){
  name.col <- paste0("mean_", arg)
  # Calculate mean value per row 
  sub.df.mean1 <-  layer %>%
    mutate(name.col =  rowMeans(dplyr::select(., contains(arg)), na.rm = FALSE)) 
  # Subset without raw data
  sub.df.mean2 <- sub.df.mean1 %>%
    dplyr::select(., -contains(arg))
  # Rename column
  names(sub.df.mean2)[names(sub.df.mean2) == 'name.col'] <- toString(name.col)
  # Final df
  return(sub.df.mean2)
}


# Test with min temperature raster
temp.min.mean <- mean.df(temp.min.grid, "tmin")
View(temp.min.mean) # OK 

# Overlay -----------------------------------------------------------------

# Function to visualize the raster layer 
Mapplot <- function(layer){
  # Variable range 
  range.var <- as.data.frame(minmax(layer))
  min.var <- min(range.layer)
  max.var <- max(range.layer)
  
  # Plot
  x11()
  # par(mfrow=c(1,1))
  for (i in val_mean) {
    ov <- mask(seas[[i]], borders.vnm)
    plot(ov, zlim=c(min.var,max.var),
         main = paste(c("Average","in" ), name.seas[i]))
    map("world", add=TRUE)
  }
  
}

# Adjustment to cells -----------------------------------------------------

# If multiple data by cell, calculate the mean value and assign it to the cell
# What if NAs?

# Example with another raster with another resolution
temp.max <- worldclim_country("Vietnam", var = "tmax", res = 0.5, path=tempdir()) # Min temperature, SPATRASTER
wind <- worldclim_country("Vietnam", var = "wind", res = 10, path=tempdir())









# Data cleaning -----------------------------------------------------------
# With CoordinateCleaning package --> Standardized cleaning

clean_coordinates(mola.df)

is.spatialvalid(mola$data) # Check for valid coordinates
is.spatialvalid(vnm.temp.autumn.df) 
