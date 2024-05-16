# Function for the aquamodel

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
# individualCount deleted from select() function because does not systematically appear, same with datasetName,
# coordinateUncertaintyInMeters, institutionCode, basisOfRecord
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
                  year)
  
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
      required_cols <- c("species", "decimalLongitude", "decimalLatitude", "countryCode", "year")
      if(all(required_cols %in% colnames(df1))) {
        # Get the flags
        df2 <- GetClean(df1, raw = "yes") # First cleaning step (delete unecessary columns etc.)
        df3 <- GetFlags(df2) # Get the flagged rows and delete them
        
        df_list[[i]] <- df3
      } else {
        warning(paste("Element", i, "in dataGBIF is missing required columns. Skipping."))
      }
      # print(i)
    } else {
      warning(paste("Element", i, "in dataGBIF is NULL or data is missing. Skipping."))
    }
  }
  final_df <- do.call(rbind, df_list)
  final_df <- distinct(final_df) # To make sure to delete the redundant rows
  return(final_df)
} # Takes a lot of time to run (size data and loop)


### Add species name in final dataframe ----------------------------------
GetCombinedDf <- function(final, sp, base){
  coord <- matrix(c(sp$x, sp$y), ncol = 2) # Coordinates from species df
  s_sp <- cellFromXY(base, xy = coord) 
  coord2 <- matrix(c(final$x, final$y), ncol = 2) # Coordinates from env df
  s_env <- cellFromXY(base, xy = coord2)
  # Target missmatches between the two dataframes
  pos <- which(! s_sp %in% s_env) # Check if there are still data outside range 
  if(length(pos)>0) {
    s_sp <- s_sp[-pos] 
    sp <- sp[-pos,]
  }
  p <- which(!is.na(s_sp)) # select only non NA data
  final$species <- NA # Create new species column
  index <- tapply(1:length(s_env), s_env, function(x){return(x)})
  rn <- index[as.character(s_sp[p])]
  # Get final dataframe
  final$species[rn] <- sp$species[p]
  return(final)
}

