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
library(magick)
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
Hg <- occ_data(scientificName = "Hemibagrus guttatus") # Vietnamese species

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

### Subset Mean value ---------------------------------------------------------------------------
mean.df <- function(layer, arg, type){
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
  # Final df
  if(type == "df"){
    return(sub.df2)
  } else if(type == "raster"){
    # Retransform as spatraster
    layer2 <- as_spatraster(sub.df2, crs = "EPSG:4326")
    return(layer2)
  }
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

### Clean species data ------------------------------------------------------
Sprast <- function(sp, raw){
  # Data
  sp.data <- sp$data
  sp.df <- as.data.frame(sp.data, xy = TRUE)
  # Filter relevant data
  sp.df <- sp.df %>%
    dplyr::select(species, decimalLongitude, 
                  decimalLatitude, countryCode, individualCount,
                  gbifID, family, taxonRank, coordinateUncertaintyInMeters,
                  year, basisOfRecord, institutionCode, datasetName)
  
  # remove records without coordinates
  sp.df <- sp.df %>%
    tidyterra::filter(!is.na(decimalLongitude)) %>%
    tidyterra::filter(!is.na(decimalLatitude))
  # Cleaned df
  name <- sp.df$species[1]
  df <- data.frame(x = sp.df$decimalLongitude, y = sp.df$decimalLatitude, species = toString(name))
  # New raster
  # r <- as_spatraster(df)
  if(raw == "yes"){
    return(sp.df)
  } else if (raw == "no"){
    return(df)
  } else {
    print("Write valid argument 'yes' or 'no' for raw data")
  }
}


# TESTING -----------------------------------------------------------------

# Min temperature
temp.min # The SpatRaster layer
tmin <- as.data.frame(temp.min, xy = TRUE) # The first dataframe
tmin.mean <- mean.df(tmin, "tmin", "df") # Compute the mean value and re-transform into a SpatRaster object
tmin.mn <- mean.df(tmin, "tmin", "raster")
Mapplot(tmin.mean, "VNM") # Plot the layer

# Wind 
wind # The spatraster layer
wind.rs <- resample(wind, temp.min, "bilinear") # Reshape so that it fits the base layer geometry (tmin here)
df <- as.data.frame(wind.rs, xy = T)
wind.mn <- mean.df(wind.rs, "wind", "df") # Compute the mean
Mapplot(wind.mn, "VNM") # Plot the layer (in Thailand here)


# Add both layers into the same dataframe
test1 <- tmin.mean %>% 
  left_join(wind.mn)
layer <- as_spatraster(test1, crs = "EPSG:4326")
Mapplot(layer = layer, ISO = "VNM") # OK


# Prepare species data ----------------------------------------------------
### For the species data
mola.data <- mola$data
mola.df <- as.data.frame(mola.data, xy = TRUE)
plot(mola.df$decimalLatitude, mola.df$decimalLongitude) # Just an idea

# Visualization of the data with rgbif
# x11()
# map_fetch()
# typeof(mola.df)

# # Interactive map
# prefix = 'https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?'
# style = 'style=purpleYellow.point'
# tile = paste0(prefix,style)
# leaflet() %>%
#   setView(lng = 20, lat = 20, zoom = 01) %>%
#   addTiles() %>%  
#   addTiles(urlTemplate=tile)
# 
# 
# # create style raster layer
# projection = '3857' # projection code
# style = 'style=osm-bright' # map style
# tileRaster = paste0('https://tile.gbif.org/',projection,'/omt/{z}/{x}/{y}@1x.png?',style)
# # create our polygons layer
# prefix = 'https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?'
# polygons = 'style=fire.point' # ploygon styles
# speciesKey = 'speciesKey=2361130' # speciesKey of mola 
# country = 'country=VD'
# tilePolygons = paste0(prefix,polygons,'&',speciesKey)
# # plot the styled map
# leaflet() %>%
#   setView(lng = 5.4265362, lat = 43.4200248, zoom = 01) %>%
#   addTiles(urlTemplate=tileRaster) %>%
#   addTiles(urlTemplate=tilePolygons)


# Add multiple layers in a map
# See for later use!!!


# Data cleaning -----------------------------------------------------------
# With CoordinateCleaning package (Standardized cleaning)
mola.df2 <- Sprast(mola, "no") # Only coordinates + presence
mola.df3 <- Sprast(mola, "yes") # Cleaned df
mola.flags <- clean_coordinates(x = mola.df3, 
                                lon = "decimalLongitude", 
                                lat = "decimalLatitude",
                                countries = "countryCode",
                                species = "species",
                                tests = c("capitals", "centroids",
                                          "equal", "zeros", "countries"))
summary(mola.flags)
# Problem with the country flags.... 100% wrong according to cc but not true...!
# Omit this for the moment


# Add coordinates to common df
Hg.data <- Hg$data
Hg.df <- as.data.frame(Hg.data, xy = TRUE)
Hg.df2 <- Sprast(Hg, "no")
sp <- mola.df2 %>%
  full_join(Hg.df2)


# Adapt the raster to the geometry of the base raster
test1.r <- as_spatraster(test1, crs = "EPSG:4326")
Mapplot(test1, "VNM")
plot(test1.r) # Normal que ca ne dessine pas le vietnam car pas fonction overlay, just to check

rastersp <- function(df, layerbase){
  coord <- matrix(c(df$x,df$y), ncol = 2)
  r <- rasterize(coord, layerbase)
  return(r)
}

coord <- matrix(c(sp$x, sp$y), ncol = 2) # Coordinates from species df
coord
# sprast <- rasterize(x = coord, y = temp.min, value = sp$species) # Rasterize coord sp with base layer
d <- as.data.frame(sprast, xy=TRUE)
x11()
plot(sp$x, sp$y)

s <- cellFromXY(temp.min, xy = coord)
s
replace(s, 1:length(s), 1)

sp$species

# Create df 3
df3 <- 
# Add species name in final dataframe (env var)
test1$species <- 0
test1$species[df3$index,]==df3$species




# Rasteriser avant le df espece, l'adapter au raster base et ensuite extraire dataset et 
summary(test2)



