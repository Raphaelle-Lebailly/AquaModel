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
borders.vnm <- gadm(country = "VNM", level = 0, path=tempdir()) # Borders, SPATVECTOR
# Climate data
temp.min <- worldclim_country("Vietnam", var = "tmin", res = 2.5, path=tempdir()) # Min temperature, SPATRASTER

# ## Per seasons
# vnm.temp.spring <- mean(temp.min[[3]], temp.min[[4]], temp.min[[5]])
# vnm.temp.summer <- mean(temp.min[[6]], temp.min[[7]], temp.min[[8]])
# vnm.temp.autumn <- mean(temp.min[[9]], temp.min[[10]],temp.min[[11]])
# vnm.temp.winter <- mean(temp.min[[12]], temp.min[[1]], temp.min[[2]])
# 
# seas <- list(vnm.temp.spring, vnm.temp.summer, vnm.temp.autumn, vnm.temp.winter)

# Species data
mola <- occ_data(scientificName = "Amblypharyngodon mola") # Test with 'mola' species, GBIF_DATA
# Meta and data
mola.df <- mola$data # Class "tbl_df"     "tbl"        "data.frame"


# Overlay -----------------------------------------------------------------
# name.seas <- c("Spring", "Summer", "Autumn", "Winter")
# 
# # Temperature range
# range.temp <- as.data.frame(minmax(temp.min))
# min.temp <- min(range.temp)
# max.temp <- max(range.temp)
# 
# # Plot
# x11()
# par(mfrow=c(2,2))
# for (i in seq_along(seas)) {
#   ov <- mask(seas[[i]], borders.vnm)
#   plot(ov, zlim=c(min.temp,max.temp),
#        main = paste("Average min temperature in Vietnam in", name.seas[i]))
#   map("world", add=TRUE)
# }


# Dataframe ---------------------------------------------------------------

# All data
temp.min.df <- as.data.frame(temp.min, xy = TRUE) # DATAFRAME

# # Per season
# vnm.temp.spring.df <- as.data.frame(vnm.temp.spring, xy = TRUE)
# names(vnm.temp.spring.df)[3] <- "value"
# vnm.temp.summer.df <- as.data.frame(vnm.temp.summer, xy = TRUE)
# names(vnm.temp.summer.df)[3] <- "value"
# vnm.temp.autumn.df <- as.data.frame(vnm.temp.autumn, xy = TRUE)
# names(vnm.temp.autumn.df)[3] <- "value"
# vnm.temp.winter.df <- as.data.frame(vnm.temp.winter, xy = TRUE)
# names(vnm.temp.winter.df)[3] <- "value"
# 
# list.seas.df <- list(vnm.temp.spring.df, vnm.temp.summer.df, vnm.temp.autumn.df, vnm.temp.winter.df)


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


# Adjustment to cells -----------------------------------------------------

# If multiple data by cell, calculate the mean value and assign it to the cell
# What if NAs?

# Example with another raster with another resolution
temp.max <- worldclim_country("Vietnam", var = "tmax", res = 0.5, path=tempdir()) # Min temperature, SPATRASTER
wind <- worldclim_country("Vietnam", var = "wind", res = 0.5, path=tempdir())

# --------------------------------------------------------------------------------

colns <- c('VNM_wc2.1_30s_tmin_1', 'VNM_wc2.1_30s_tmin_2')

temp.min.grid2 <- temp.min.grid %>%
  rowwise() %>%
  mutate(tmin_mean = mean(c_across(colns), na.rm = T)) %>%
  select(- colns)

# indx <- grepl('tmin', colnames(temp.min.grid))
# indx
# temp.min.grid$tmin_mean <- apply(temp.min.grid[indx], mean)
# 
# # Goal is to add the variables in the same df
# # Calculate mean per variable and transform it into the name of the variable
# 
# cols_tmin <- grep("tmin", names(temp.min.grid), value = TRUE)
# temp.min.grid$tmin_mean <- rowMeans(dataset[cols_tmin], na.rm = TRUE)
# 
# dataset <- temp.min.grid %>%
#   rowwise() %>%
#   mutate(tmin_mean = rowMeans(select(., all_of(cols_tmin)), na.rm = TRUE))
# 


# Data cleaning -----------------------------------------------------------
# With CoordinateCleaning package --> Standardized cleaning

clean_coordinates(mola.df)

is.spatialvalid(mola$data) # Check for valid coordinates
is.spatialvalid(vnm.temp.autumn.df) 
