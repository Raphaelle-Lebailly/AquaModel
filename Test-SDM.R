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

# Define a base raster that defines the scaffold for the grid
SnapToBase <- function(layer){
  df <- as.data.frame(layer,xy=T) # Use the xy dataframe and append the (x,y) values of each cell + index value
  # Resolution
  Dim <- dim(layer) 
  ResX <- Dim[1] # Resolution for x
  ResY <- Dim[2] # Resolution for y 
  DimLayer <- list(ResX, ResY)
  # Extent
  ext <- ext(layer) # Extent for data (terra object)
  xmin <- ext$xmin
  ymin <- ext$ymin
  xmax <- ext$xmax
  ymax <- ext$ymax
  ext.layer <- list(xmin, ymin, xmax, ymax)
  #Delta
  deltaX <- (xmax - xmin)/ResX
  deltaY <- (ymax - ymin)/ResY 
  # Create grid
  df$snapX <- as.integer(((df$x-xmin)/deltaX) + 0.5)
  df$snapY <- as.integer(((df$y-ymin)/deltaY) + 0.5)
  len <- dim(df)
  df$index <- seq(1,len[1])
  # Return
  list(df, DimLayer, ext.layer)
}


# Test the function
temp.min.grid <- SnapToBase(temp.min) 
View(temp.min.grid)

# Retrieve the data
temp.min.grid <- temp.min.grid[[1]]
temp.min.res <- temp.min.grid[[2]]
temp.min.ext <- temp.min.grid[[3]]

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

# # Overlay -----------------------------------------------------------------
# 
# # Function to visualize the raster layer 
# Mapplot <- function(layer, borders){
#   # Variable range 
#   range.layer <- as.data.frame(minmax(layer))
#   min.var <- min(range.layer)
#   max.var <- max(range.layer)
#   # Plot
#   # x11()
#   # par(mfrow=c(1,1))
#   col <- layer %>% 
#     dplyr::select(., contains("mean"))
#   ov <- mask(col, borders)
#   plot(ov, zlim=c(min.var,max.var),
#     main = paste(c("Average value","in" )))
#     map("world", add=TRUE)
# }
# 
# Mapplot(temp.min.mean, borders.vnm)
# 
# col <-  temp.min.mean %>% 
#   dplyr::select(., contains("mean"))
# colname <-colnames(col)
# head(col)
# 
# Ne fonctionne pas, a voir plus tard (juste de la visualisation en soi)

# Adjustment to cells -----------------------------------------------------

# Use tapply function

# Example with another raster with another resolution
temp.max <- worldclim_country("Vietnam", var = "tmax", res = 0.5, path=tempdir()) # Min temperature, SPATRASTER
wind <- worldclim_country("Vietnam", var = "wind", res = 10, path=tempdir())


# Index = (Sx -1).ny + Sy
# Base$NewVector[v2[names[v]]]


# Snaptogrid secon layer
# Function Snaptogrid to change
# Parameters for base raster
base.res <- temp.min.res
base.ext <- temp.min.ext

SnapToGrid <- function(layer, base){
  df <- as.data.frame(layer,xy=T) # Use the xy dataframe and append the (x,y) values of each cell + index value
  # Resolution
  Dim <- dim(base) 
  ResX <- Dim[1] # Resolution for x
  ResY <- Dim[2] # Resolution for y 
  DimLayer <- list(ResX, ResY)
  # Extent
  ext <- ext(base) # Extent for data (terra object)
  xmin <- ext$xmin
  ymin <- ext$ymin
  xmax <- ext$xmax
  ymax <- ext$ymax
  ext.layer <- list(xmin, ymin, xmax, ymax)
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

tapply()

# Data cleaning -----------------------------------------------------------
# With CoordinateCleaning package --> Standardized cleaning

clean_coordinates(mola.df)
is.spatialvalid(mola$data) # Check for valid coordinates
is.spatialvalid(vnm.temp.autumn.df) 
