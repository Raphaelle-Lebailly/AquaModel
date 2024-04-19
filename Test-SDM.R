# SDM FOR AQUACULTURE CANDIDATE SPECIES

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

# FUNCTIONS ---------------------------------------------------------------
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


### Add species name in final dataframe ----------------------------------
Final.df <- function(final, sp){
  coord <- matrix(c(sp$x, sp$y), ncol = 2) # Coordinates from species df
  s <- cellFromXY(temp.min, xy = coord)
  final$species <- NA
  for (i in 1:length(s)) {
    if (!is.na(s[i])) {
      final$species[s[i]] <- sp$species[i]
    }
  }
  return(final)
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


# Assemble final data frame with environmental values + species
finaldf <- Final.df(test1, sp)
# Convert to raster
finalr <- as_spatraster(finaldf, crs = "EPSG:4326")
finalr
Mapplot(finalr, "VNM")


### Visualization of the data with rgbif ---------------------------------









