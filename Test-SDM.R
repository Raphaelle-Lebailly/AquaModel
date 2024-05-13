# SDM FOR AQUACULTURE CANDIDATE SPECIES
setwd("C:/Users/User/Desktop/Internship/Data")
# PACKAGES ----------------------------------------------------------------
# Data Management
library(tidyverse)
library(conflicted) 
library(letsR)
library(foreach)
library(doParallel)
library(DescTools)
library(rfishbase)
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
# Alpha countries data
data("d.countries") # Data with codes a2 and a3 to convert for flags
head(d.countries)
countcode <- d.countries %>%
  select(a2, a3)

# Species for SDM 
aquaspecies_df <- read_rds("aquaspecies_df.rds")

# Species for background
# Background data 
setwd("C:/Users/User/Desktop/Internship/Data")

# First element is the same as the last one from the previous batch (my bad)
distrifish2 <- distrifish2[-1]
distrifish3 <- distrifish3[-1]
distrifish4 <- distrifish4[-1]
distrifish5 <- distrifish5[-1]

# Combine all of the data
# , distrifish3, distrifish4, distrifish5
distrifish_all <- rbind(distrifish1, distrifish2) # Apparently not working
# saveRDS(distrifish_all, "background_data.rds")

# FUNCTIONS ---------------------------------------------------------------
### Get Data ---------------------------------------------------------------
# Environmental data

# See if it's useful for future code optimization
# GetEnvData <- function(variable, country, resolution){
#   data <- worldclim_country(country, var = variable, res = resolution, path=tempdir())
#   return(data)
# }
# Species data
# GetSpData <- function(species){
#   data <- rgbif::occ_data(scientificName = species)
#   return(data)
# }

### Subset Mean value ---------------------------------------------------------------------------
GetMeanDf <- function(layer, arg, type){
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
GetMap <- function(layer, ISO){ # borders = ISOCODE => importer le spatvector en fonction
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
# individualCount deleted from select() function because does not systematically appear, same with datasetName and coordinateUncertaintyInMeters
# Don't think we recquire them for the moment, add conditions later if that's the case. 
# Sometimes, the data object is NULL, add a condition so that it does not count 

GetClean <- function(sp, raw){
  # Data
  #sp.data <- sp$data
  sp.df <- as.data.frame(sp, xy = TRUE)
  # Filter relevant data
  sp.df <- sp.df %>%
    dplyr::select(species, decimalLongitude, 
                  decimalLatitude, countryCode, 
                  gbifID, family, taxonRank,
                  year, basisOfRecord, institutionCode)
  
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

### Species data flagging ------------------------------------------------------
# Allows to get the flags and remove the rows flagged
GetFlags <- function(datadf) { 
  # Replace alpha-2 with alpha-3
  indices <- match(datadf$countryCode, countcode$a2)
  datadf$countryCode <- countcode$a3[indices]
  # Flags
  flags <- clean_coordinates(x = datadf, 
                             lon = "decimalLongitude", 
                             lat = "decimalLatitude",
                             countries = "countryCode",
                             species = "species",
                             tests = c("countries"), verbose = FALSE) # Choice to have the details of the flags or not 
  # Remove flags
  flags <- subset(flags, .summary == TRUE)
  df <- data.frame(x = flags$decimalLongitude, y = flags$decimalLatitude, species = flags$species)
  return(df)
}

### Get species dataframe ------------------------------------------------------

GetSpDf <- function(dataGBIF){
  df_list <- list()
  for(i in 1:length(dataGBIF)){
    if (!is.null(dataGBIF[[i]]) && !is.null(dataGBIF[[i]][["data"]])) {
    # Extract the data from the raster
    df1 <- as.data.frame(dataGBIF[[i]][["data"]], xy = TRUE)
    # Check if we have the recquired columns 
    required_cols <- c("species", "decimalLongitude", "decimalLatitude", "countryCode")
    if(all(required_cols %in% colnames(df1))) {
      # Get the flags
      df2 <- GetClean(df1, raw = "yes") # First cleaning step (delete unecessary columns etc.)
      df3 <- GetFlags(df2) # Get the flagged rows and delete them
    
      df_list[[i]] <- df3
    } else {
      warning(paste("Element", i, "in dataGBIF is missing required columns. Skipping."))
    }
    print(i)
    } else {
      warning(paste("Element", i, "in dataGBIF is NULL or data is missing. Skipping."))
    }
  }
  final_df <- do.call(rbind, df_list)
  return(final_df)
} # Takes a lot of time to run (size data and loop)


### Manip to tidy up the data species into 1 dataframe:
# listedf <- GetSpDf(dist_aqua) # Takes some time to run... (352 species)
# 3 species ignored (NULL data or absence of recquired columns --> defined in the function)
# saveRDS(listedf, "aquaspecies_df.rds") # Save the file as .rds


### Add species name in final dataframe ----------------------------------
GetCombinedDf <- function(final, sp, base){
  coord <- matrix(c(sp$x, sp$y), ncol = 2) # Coordinates from species df
  s_sp <- cellFromXY(base, xy = coord)
  coord2 <- matrix(c(final$x, final$y), ncol = 2) # Coordinates from env df
  s_env <- cellFromXY(base, xy = coord2)
  
  # Target missmatches between the two dataframes
  pos <- which(! s_sp %in% s_env)
  s_sp <- s_sp[-pos]
  sp <- sp[-pos,]
  p <- which(!is.na(s_sp))
  
  final$species <- NA
  
  index <- tapply(1:length(s_env),s_env,function(x){return(x)})
  rn <- index[as.character(s_sp[p])]
  
  final$species[rn] <- sp$species[p]
  return(final)
}



# Get combined dataframe for all species and environmental data
Combined_df1 <- GetCombinedDf(env, aquaspecies_df, tmin)
length(which(!is.na(Combined_df1$species)))

View(Combined_df1)






# PROTOCOLE ---------------------------------------------------------------
# Test if every function is working well and giving generalizable results.

### Import raster data ---------------------------------------------------------------
## Environmental data
# tmin <- GetEnvData("tmin", "Vietnam", 0.5)
tmin <- worldclim_country("Vietnam", var = "tmin", res = 0.5, path=tempdir())
# wind <- GetEnvData("wind", "Vietnam", 0.5)
wind <- worldclim_country("Vietnam", var = "wind", res = 0.5, path=tempdir())
## Species data
mola <- occ_data(scientificName = "Amblypharyngodon mola")
Hg <- occ_data(scientificName = "Hemibagrus guttatus")

### Set base  ----------------------------------------------------------
## Layer
BASE <- tmin
## Country
COUNTRY <- c(name = "Vietnam", ISO = "VNM")

### Data  cleaning ---------------------------------------------------------------
## Resample
wind.rs <- resample(wind, BASE, "bilinear") # Adapt one raster's geometry to the base layer

## Subset mean value (optional)
# Minimal temperature
tmin.df <- as.data.frame(tmin, xy = TRUE) # The first dataframe
tmin.mn <- GetMeanDf(tmin, "tmin", "df") # # Compute the mean value and keep as a dataframe (useful?)
tmin.mnr <- GetMeanDf(layer = tmin, arg = "tmin", type = "raster") # Compute the mean value and re-transform into a SpatRaster object
Mapplot(tmin.mn, "VNM") # Plot the layer (works with df or raster)

# Wind
wind.df <- as.data.frame(wind.rs, xy = T)
wind.mn <- GetMeanDf(wind.rs, "wind", "df") # Compute the mean
Mapplot(wind.mn, "VNM") 

# Merge environmental variables
# Add both layers into the same dataframe 
env <- tmin.mn %>%
  left_join(wind.mn)
env2 <- cbind(tmin.mn, mean_wind = wind.mn$mean_wind) # Method 2

# Convert back to a raster with all the environmental variables
env.rast <- as_spatraster(env, crs = "EPSG:4326")
Mapplot(layer = env.rast, ISO = "VNM") # Check if it works

## Species data
# Prepare species data ----------------------------------------------------
### For the species data
mola.data <- mola$data
mola.df <- as.data.frame(mola.data, xy = TRUE)
plot(mola.df$decimalLatitude, mola.df$decimalLongitude) # Check distribution quickly

# Get simplified species dataset
mola.df2 <- GetClean(mola, "no") # Only coordinates + presence
mola.cc <- GetClean(mola, "yes") # Cleaned df but with details (used for flags)
Hg.df2 <- GetClean(Hg, "no")
Hg.cc <- GetClean(Hg, "yes")

# Check flags
mola.flags <- GetFlags(mola) # 8 countries flagged
Hg.flags <- Getflag(Hg.cc) # 0 flag



# Merge all species in one df
# sp <- mola.df2 %>%
#   full_join(Hg.df2)
sp <- rbind(mola.df2, Hg.df2)

# Assemble final data frame with environmental values + species
finaldf <- GetCombinedDf(env, sp, BASE) 
# Convert to raster
finalr <- as_spatraster(finaldf, crs = "EPSG:4326")
finalr
Mapplot(finalr, "VNM")


# TESTING -----------------------------------------------------------------

##### Checking the coordinates and row numbers
# c = matrix(c(finaldf2$x[ps], finaldf2$y[ps]), ncol = 2)
# cellFromXY(temp.min, c)

### Visualization of the data with rgbif ---------------------------------


# Background data ---------------------------------------------------------
# Use the rfishbase package to import all the fish data (30 000 sp of fishes)
# species of fish, their biology, ecology, morphology, and more. This package also
# supports experimental access to 'SeaLifeBase' data, which contains
# nearly 200,000 species records for all types of aquatic life not covered by
# 'FishBase.'


# Penser a enlever les lignes qui se repetent (premieres lignes ou dernieres lignes)
# Fit all of this in a grid



# SDM test ----------------------------------------------------------------

# bg <- fb_tbl("species")
# bg_aqua <- bg %>%
#   tidyterra::filter(UsedforAquaculture == "commercial")
# dim(bg_aqua) # 358 entries
# # # Add column with both genus and species name
# bg_aqua$namesp <- paste(bg_aqua$Genus, bg_aqua$Species, sep = " ")
# # unique(bg_aqua$namesp)
# # # Retrieve this as a vector
# aq_sp <- bg_aqua$namesp
# # Import distribution data for this list of species (GBIF or FishBase?)
# dist_aqua <- rgbif::occ_data(scientificName = aq_sp)
# saveRDS(dist_aqua, file = "aquafish.rds")



