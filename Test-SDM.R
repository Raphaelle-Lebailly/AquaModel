setwd("C:/Users/User/Desktop/Internship/Data")


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
library(geodata)
library(sf) 
# library(raster)
library(CoordinateCleaner) 


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


# Test the function
temp.min.grid <- SnapToGrid(temp.min) 
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

# Ne fonctionne pas, a voir plus tard (juste de la visualisation en soi)

# Adjustment to cells -----------------------------------------------------

# Use tapply function

# Example with another raster with another resolution
# temp.max <- worldclim_country("Vietnam", var = "tmax", res = 0.5, path=tempdir()) # Min temperature, SPATRASTER
# countries <- c("Vietnam", "Thailand")
wind <- worldclim_country("Thailand", var = "wind", res = 10, path=tempdir())

wd.df <- as.data.frame(wind, xy = T)
wd.df.mean <- mean.df(wd.df, "wind")
View(wd.df.mean)

# Bidouillage
range.layer <- as.data.frame(minmax(wind))
min.var <- min(range.layer)
max.var <- max(range.layer)
col <- wd.df.mean %>%
  dplyr::select(., contains("mean"))
borders.tha <- gadm(country = "THA", level = 0, path=tempdir()) # Borders, SPATVECTOR
ov <- terra::mask(wind, borders.tha) # Use SpetRaster object and borders layer
# Calculer la moyenne une fois que le masque est pose
ov.df <- as.data.frame(ov, xy = TRUE)
wd.mean.mask <- mean.df(ov.df, "wind")
x11()
plot(ov)
# , zlim=c(min.var,max.var)
map("world", add=TRUE)


--------------------------------------------------------------------

# Index = (Sx -1).ny + Sy
# Base$NewVector[v2[names[v]]]


# Snaptogrid second layer
# Function Snaptogrid to change
# Parameters for base raster
base.res <- temp.min.res
base.ext <- temp.min.ext

Gridify <- function(layer, base){
  lay <- as.data.frame(layer,xy=T) # Use the xy dataframe and append the (x,y) values of each cell + index value
  base <- as.data.frame(base,xy=T)
  
  ## BASE PARAMETERS
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

SnapToGrid()

tapply()


# Bidouillage standardisation en grille -----------------------------------------
x1 <- runif(10,1,20)
y1 <- runif(10,50,80)
val1 <- sample(1:50, 10)
df1 <- data.frame(x1, y1, val1)
df1
x2 <- sample(1:100, 20)
y2 <- sample(200:300, 20)
val2 <- sample(1:50, 20)
df2 <- data.frame(x2, y2, val2)
df2

dffinal <- terra::resample(temp.min, wind, method = "average")

df <- as.data.frame(dffinal, xy = TRUE)
View(df)

# 2 rasters pas dans les memes conditions (longeur, unites, range etc.)


# On essaye de les faire correspondre. On prend le rater 1 comme base.


# Data cleaning -----------------------------------------------------------
# With CoordinateCleaning package --> Standardized cleaning

clean_coordinates(mola.df)
is.spatialvalid(mola$data) # Check for valid coordinates
is.spatialvalid(vnm.temp.autumn.df) 
