# setwd("C:/Users/User/Desktop/Internship/Data")

# Packages ----------------------------------------------------------------
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
library(geodata) # Import environmental data
library(sf) 
library(CoordinateCleaner) 


# Data importation --------------------------------------------------------

# Geographical data
borders.vnm <- gadm(country = "VNM", level = 0, path=tempdir()) # Borders, SPATVECTOR
# Climate data
temp.min <- worldclim_country("Vietnam", var = "tmin", res = 2.5, path=tempdir()) # Min temperature, SPATRASTER
wind <- worldclim_country("Thailand", var = "wind", res = 10, path=tempdir())
# Species data
mola <- occ_data(scientificName = "Amblypharyngodon mola") # Test with 'mola' species, GBIF_DATA
# Meta and data
# mola.df <- mola$data # Class "tbl_df"     "tbl"        "data.frame" ; for later


# Functions ---------------------------------------------------------------

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
  #Ret
  list(df, DimLayer, ext.layer)
}

### Subset Mean value ---------------------------------------------------------------------------

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


### Overlay -----------------------------------------------------------------

# Function to visualize the raster layer
Mapplot <- function(layer, borders){
  # Variable range
  range.layer <- as.data.frame(minmax(layer))
  min.var <- min(range.layer)
  max.var <- max(range.layer)
  # Plot
  # x11()
  # par(mfrow=c(1,1))
  col <- layer %>%
    dplyr::select(., contains("mean"))
  ov <- mask(col, borders)
  plot(ov, zlim=c(min.var,max.var),
    main = paste(c("Average value","in" )))
    map("world", add=TRUE)
}

Mapplot(temp.min.mean, borders.vnm)

col <-  temp.min.mean %>%
  dplyr::select(., contains("mean"))
colname <-colnames(col)
head(col)

# Ne fonctionne pas, a voir plus tard (juste de la visualisation en soi)


# Resample the rasters  ----------------------------------------------------------------
# Use the resample function of the 'terra' package. Adapts the geometry of one raster to another.
# Has to be a SpatRaster format.
# Chose the adapted method for the resampling.
wd.df <- as.data.frame(wind, xy = T)
wd.df.mean <- mean.df(wd.df, "wind")
View(wd.df.mean)

# Reshape the geometry of the wind layer (with temp.min as geometry reference)
wind.rsp <- terra::resample(wind, temp.min, method = "bilinear") 
# Need to verify if the method is adapted to our case.





# # Test the function
# temp.min.grid <- SnapToGrid(temp.min) 
# View(temp.min.grid)
# 
# # Retrieve the data
# temp.min.grid <- temp.min.grid[[1]]
# temp.min.res <- temp.min.grid[[2]]
# temp.min.ext <- temp.min.grid[[3]]



# Test with min temperature raster
temp.min.mean <- mean.df(temp.min.grid, "tmin")
View(temp.min.mean) # OK



# EXAMPLE:
r <- rast(nrows=3, ncols=3, xmin=0, xmax=10, ymin=0, ymax=10)
values(r) <- 1:ncell(r)
s <- rast(nrows=25, ncols=30, xmin=1, xmax=11, ymin=-1, ymax=11)
values(s) <- 1:ncell(s)
x <- resample(r, s, method="bilinear")

# Adaptation d
par(mfrow=c(1,3))
plot(r, main = "raster to convert")
plot(s, main = "raster structure")
plot(x, main = "Result")

# On essaye de les faire correspondre. On prend le rater 1 comme base.







# Data cleaning -----------------------------------------------------------
# With CoordinateCleaning package --> Standardized cleaning

clean_coordinates(mola.df)
is.spatialvalid(mola$data) # Check for valid coordinates
is.spatialvalid(vnm.temp.autumn.df) 
