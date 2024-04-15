# setwd("C:/Users/User/Desktop/Internship/Data")

# PACKAGES ----------------------------------------------------------------
# Data Management
library(tidyverse)
library(conflicted) 
library(letsR)
# Plot Maps
library(plotly)
library(maps)
library(rgbif) 
library(ggplot2) 
library(RColorBrewer) 
library(cowplot)
# Spatial data
#library(raster) # Obsolete
library(terra)
library(tidyterra)
library(geodata) # Import environmental data
library(sf) 
library(leaflet) # Web mapping
library(CoordinateCleaner) 


# DATA IMPORTATION --------------------------------------------------------

# Geographical data
# borders.vnm <- gadm(country = "VNM", level = 0, path=tempdir()) # Borders, SPATVECTOR
# Climate data
temp.min <- worldclim_country("Vietnam", var = "tmin", res = 2.5, path=tempdir()) # Min temperature, SPATRASTER
temp.max <- worldclim_country("Vietnam", var = "tmax", res = 2.5, path=tempdir()) # Min temperature, SPATRASTER
wind <- worldclim_country("Vietnam", var = "wind", res = 10, path=tempdir())
# Species data
mola <- occ_data(scientificName = "Amblypharyngodon mola") # Test with 'mola' species, GBIF_DATA
# rtilapia <- occ_data(scientificName = "Oreochromis sp.")
# Meta and data
# mola.df <- mola$data # Class "tbl_df"     "tbl"        "data.frame" ; for later


# FUNCTIONS ---------------------------------------------------------------

### Snap to grid ------------------------------------------------------------
# The goal is to build the grid scaffold from the environmental data layers to have a base
# and chose the adequate resolution. After that, the species data can be projected within the
# different grid cells (presence/absence) and filter the data rightfully.

# [(x-xmin)/delta(x) +0.5]

# Snap to grid function (manually)
# Define a base raster that defines the scaffold for the grid
SnapToGrid <- function(layer){
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
  return(df)
  # list(df, DimLayer, ext.layer)
}


### Subset Mean value ---------------------------------------------------------------------------
mean.df <- function(layer, arg){
  df <- as.data.frame(layer, xy =T)
  name.col <- paste0("mean_", arg)
  # Calculate mean value per row 
  sub.df1 <-  df %>%
    mutate(name.col =  rowMeans(dplyr::select(., contains(arg)), na.rm = FALSE)) 
  # Subset without raw data
  sub.df2 <- sub.df1 %>%
    dplyr::select(., -contains(arg))
  # Rename column
  names(sub.df2)[names(sub.df2) == 'name.col'] <- toString(name.col)
  # Retransform as spatraster
  layer2 <- as_spatraster(sub.df2, crs = "EPSG:4326")
  # Final df
  return(layer2)
}


### Overlay -----------------------------------------------------------------
# Function to visualize the raster layer
Mapplot <- function(layer, ISO){ # borders = ISOCODE => importer le spatvector en fonction
  if( (typeof(layer) != "S4")| (typeof(layer) != "S3")){
    layer <- as_spatraster(layer, crs = "EPSG:4326")
  }
  # Geographical data
  borders <- gadm(country = ISO, level = 0, path=tempdir()) # Borders, SPATVECTOR
  # Variable range
  range.layer <- as.data.frame(minmax(layer))
  min.var <- min(range.layer)
  max.var <- max(range.layer)
  # Plot
  x11()
  par(mfrow=c(1,1))
  ov <- mask(layer, borders)
  plot(ov, zlim=c(min.var,max.var),
    main = paste(c(names(layer), ISO)))
    # map("world", add=TRUE)
}


# TESTING -----------------------------------------------------------------

# Min temperature
temp.min # The SpatRaster layer
tmin <- as.data.frame(temp.min, xy = TRUE) # The first dataframe
tmin.mean <- mean.df(tmin, "tmin") # Compute the mean value and re-transform into a SpatRaster object
Mapplot(tmin.mean, "VNM") # Plot the layer

# Wind 
wind # The spatraster layer
wind.rs <- resample(wind, temp.min, "bilinear") # Reshape so that it fits the base layer geometry (tmin here)
df <- as.data.frame(wind.rs, xy = T)
wind.mean <- mean.df(wind.rs, "wind") # Compute the mean
Mapplot(wind.mean, "VNM") # Plot the layer (in Thailand here)


# Add both layers into the same dataframe
test1 <- tmin.df %>% 
  left_join(tmax.df)

which(is.na(test1)) # Pas de NA, normal car tout colle normalement


### For the species data
mola.data <- mola$data
mola.df <- as.data.frame(mola.data, xy = TRUE)
x11()
Mapplot(mola, "VNM")

# Data cleaning -----------------------------------------------------------
# With CoordinateCleaning package --> Standardized cleaning

clean_coordinates(mola.df)
is.spatialvalid(mola$data) # Check for valid coordinates
is.spatialvalid(vnm.temp.autumn.df) 
