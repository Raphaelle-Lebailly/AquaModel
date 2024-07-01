# Function for the aquamodel

# PACKAGES ----------------------------------------------------------------
# Data Management
library(tidyverse)
library(lessR)
library(conflicted) 
library(letsR)
library(foreach)
library(doParallel)
library(DescTools)
library(rfishbase)
library(rlang)
# Plot Maps
library(plotly)
library(maps)
library(rnaturalearth) 
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
library(biooracler) # Marine data


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


# Resample multiple variables ---------------------------------------------

GetDataResample <- function(listraster, baseraster){
  newname <- paste0(arg, "_rs")
  for(i in 1:length(listraster)){
    
  }
}


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
} # Takes a lot of time to run (size data and loop), one time thing


### Add species name in final dataframe ----------------------------------
# GetCombinedDf <- function(final, sp, base){
#   coord <- matrix(c(sp$x, sp$y), ncol = 2) # Coordinates from species df
#   s_sp <- cellFromXY(base, xy = coord) 
#   coord2 <- matrix(c(final$x, final$y), ncol = 2) # Coordinates from env df
#   s_env <- cellFromXY(base, xy = coord2)
#   # Target missmatches between the two dataframes
#   pos <- which(! s_sp %in% s_env) # Check if there are still data outside range 
#   if(length(pos)>0) {
#     s_sp <- s_sp[-pos] 
#     sp <- sp[-pos,]
#   }
#   p <- which(!is.na(s_sp)) # select only non NA data
#   final$species <- NA ; final$PA <- NA # Create new species column
#   index <- tapply(1:length(s_env), s_env, function(x){return(x)})
#   rn <- index[as.character(s_sp[p])]
#   # Get final dataframe
#   final$species[rn] <- sp$species[p] # Add species column
#   final$PA[rn] <- sp$PA[p] # Add presence/absence column
#   
#   # final$country[rn] <- sp$country[p] # Add country column
#   # final$Aquaculture_status[rn] <- sp$Aquaculture_status[p] # Add aquaculture status column
#   return(final)
# }

GetCombinedDf <- function(final, sp, base) {
  coord <- matrix(c(sp$x, sp$y), ncol = 2) # Coordinates from species df
  s_sp <- cellFromXY(base, xy = coord) 
  coord2 <- matrix(c(final$x, final$y), ncol = 2) # Coordinates from env df
  s_env <- cellFromXY(base, xy = coord2)
  
  # Target mismatches between the two dataframes
  pos <- which(!s_sp %in% s_env) # Check if there are still data outside range 
  if(length(pos) > 0) {
    s_sp <- s_sp[-pos] 
    sp <- sp[-pos,]
  }
  
  p <- which(!is.na(s_sp)) # Select only non NA data
  final$species <- NA
  final$PA <- NA
  final$country <- NA
  final$Aquaculture_status <- NA # Create new columns in final dataframe
  
  index <- tapply(1:length(s_env), s_env, function(x) {return(x)})
  rn <- index[as.character(s_sp[p])]
  
  # Get final dataframe
  final$species[rn] <- sp$species[p] # Add species column
  final$PA[rn] <- sp$PA[p] # Add presence/absence column
  
  # Add country column, ensuring NA values are retained
  final$country[rn] <- sp$country[p]
  
  # Add aquaculture status column, ensuring NA values are retained
  final$Aquaculture_status[rn] <- sp$Aquaculture_status[p]
  
  return(final)
}


### Get the dataframe to generate the model ---------------------------------
# OBSOLETE
# GetModelData <- function(pseudoabs, pres, nb){
#   # Rename df and delete NA rows
#   df1 <- na.omit(pseudoabs)
#   df2 <- na.omit(pres)
#   # Add Presence/Absence column
#   df1$PA <- 0
#   df2$PA <- 1
#   # Select only the species with 20>= occurrences
#   df3 <- df2 %>%
#     group_by(species) %>%
#     tidyterra::filter(n() >= nb) %>% # Chose nb of occurrences depending on the 
#     ungroup()
#   # Merge dataframes
#   species_list <- split(df3, df3$species)
#   list_df <- lapply(species_list, function(df) {
#     rbind(df, df1)
#   })
#   return(list_df)
# }


# Get the cropped raster given the targeted area --------------------------
GetCroppedRaster <- function(list_raster, extent){
  # Get extent
  name_reg <- paste0(extent)
  region <- world_vect[world_vect$name == name_reg, ]
  
  # Draw polygon from coastline
  intersect <- terra::intersect(coastline_vect, region)
  
  # Crop coastline in targeted area
  crop <- crop(region, intersect)
  
  # Add a buffer everywhere following the border
  buffer <- buffer(crop, width = 22000) # Apply 22km buffer on all coasts
  
  # Combine Geometries
  combined <- terra::union(region, buffer)
  
  # Crop raster
  rast_ext <- list()
  for (i in seq_along(list_raster)) {
    rast_ext[[i]] <- crop(list_raster[[i]], combined, mask = TRUE) # use mask to respect boundaries and not extent
  }
  
  return(rast_ext)
  
  # VERSION APPLY DU CI DESSUS
  # lapply(X = list_raster, FUN = function(x){
  #   crop(x, reg_ext)
  # })
  
}




### Get subset of background data -------------------------------------------
# First function to get the background species given an extent
GetSubBg <-function(bg_df, extent){
  # Get extent
  name_reg <- paste0(extent)
  region <- world_vect[world_vect$name == name_reg, ]
  ext <- ext(region)
  # Select data inside given extend
  bg_ext <- bg_df %>%
    tidyterra::filter(x >= ext$xmin & x <= ext$xmax & 
                        y >= ext$ymin & y <= ext$ymax)
  # Select random 10,000 values
  if(length(bg_ext) > 10000){
    sub_bg <- bg_ext[.(random(10000)),]
  } else {
    sub_bg <- bg_ext
    print(paste("Length <10,000 species:",length(sub_bg$species)))
  }
  return(sub_bg)
} 


#### ADVANCED METHOD ------------------------------------------------------------------------

#### Get species organisation ####
#### Get a list of species names per country (one time thing) 
GetSpCount<-function(country, df){ 
  region <- world_vect[world_vect$name == country, ]
  e <- ext(region)
  rn=which(df$x >= e$xmin & df$x <= e$xmax & df$y>=e$ymin & df$y <=e$ymax)
  if(length(rn)>0)
  {
    u=unique(df$species[rn])
  return(u)
  }else{
    return("")
  }
}

## For the aquaculture species (aquaspecies_df)
# t=list()
# cnt=0
# for(i in regions){
#   cnt=cnt+1
#   print(cnt)
#   t[[i]] <- get_sp_country(i, aquaspecies_df)
#   
# }
## Save the object
# setwd("C:/Users/User/Desktop/Internship/Data")
# saveRDS(t,"species_per_country.rds")


### Same for background species data
GetBg <-function(countries, df){
  u_comb=NULL
  for(i in countries){
    region <- world_vect[world_vect$name == countries[i], ]
    e <- ext(region)
    rn=which(df$x >= e$xmin & df$x <= e$xmax & df$y>=e$ymin & df$y <=e$ymax)
    if(length(rn)>0){
    u_comb=rbind(df[rn,])
    }
  }
  return(u_comb)
}

# t2 <- list()
# cnt=0
# for(i in regions){
#   cnt=cnt+1
#   print(cnt)
#   t2[[i]] <- get_bg(i, bg_df)
# }
# t2 <- do.call(get_bg, list(regions, bg_df), quote = TRUE )
# saveRDS(t2,"background_per_country.rds")


### Matrix of presence/absence of species per country 
u=unique(aquaspecies_df$species) # List of all of the species names
rn_sp=tapply(1:length(u),u,function(x){return(x)})
df_spc=matrix(0,nrow=length(unique(aquaspecies_df$species)),ncol=length(regions))
rownames(df_spc)=u
colnames(df_spc)=regions
for(i in 1:length(regions)){
  df_spc[rn_sp[t[[i]]],i]=1
}
# saveRDS(df_spc,"presence_sp_per_count.rds")
# df_spc is the dataframe of presences of species per country



### Render countries where species are observed and add pseudoabsences
# Modify get sub bg function

#### Get background data per country ####
GetSubBg_count <-function(bg_df, extent){
  # Get extent
  name_reg <- paste0(extent)
  region <- world_vect[world_vect$name == name_reg, ]
  ext <- ext(region)
  # Select data inside given extend
  bg_ext <- bg_df %>%
    tidyterra::filter(x >= ext$xmin & x <= ext$xmax & 
                        y >= ext$ymin & y <= ext$ymax)
  # Select random 10,000 values
  if(length(bg_ext) > 10000){
    sub_bg <- bg_ext[.(random(10000)),]
  } else {
    sub_bg <- bg_ext
    print(paste("Length <10,000 species:",length(sub_bg$species)))
  }
  return(sub_bg)
} 

# Do things PER SPECIES instead of per country
# spc_df is the dataframe for the species per country presence
# sp_name is the string of the name of the species of interest

GetSubBg_sp <-function(bg_df, spc_df, sp_name){
  # Get extent given the species names and where they occurr
  # Line with the species of interest
  spl <- spc_df[sp_name,] # Depends on how the dataframe is built (header = true or not)
  for(i in spl){
    spl %>%
      tidyterra::filter(1)
  }
  count <- list()
  # Select data inside given extent
  bg_ext <- bg_df %>%
    tidyterra::filter(x >= ext$xmin & x <= ext$xmax &
                        y >= ext$ymin & y <= ext$ymax)
  # Select random 10,000 values
  if(length(bg_ext) > 10000){
    sub_bg <- bg_ext[.(random(10000)),]
  } else {
    sub_bg <- bg_ext
    print(paste("Length <10,000 species:",length(sub_bg$species)))
  }
  return(sub_bg)
}

sp_names <- unique(aquaspecies_df$species)
# List of dataframes with presences and pseudoabsences
GetSubBg_sp(bg_df,df_spc, sp_names)

GetSubBg <-function(bg_df, extent){
  # Get extent
  name_reg <- paste0(extent)
  region <- world_vect[world_vect$name == name_reg, ]
  ext <- ext(region)
  # Select data inside given extend
  bg_ext <- bg_df %>%
    tidyterra::filter(x >= ext$xmin & x <= ext$xmax & 
                        y >= ext$ymin & y <= ext$ymax)
  # Select random 10,000 values
  if(length(bg_ext) > 10000){
    sub_bg <- bg_ext[.(random(10000)),]
  } else {
    sub_bg <- bg_ext
    print(paste("Length <10,000 species:",length(sub_bg$species)))
  }
  return(sub_bg)
} 

# Group fusion for big dataframes -----------------------------------------

GetMerged <- function(df_list, group_size = 10) {
  merged_list <- list()
  num_groups <- ceiling(length(df_list) / group_size)
  
  for (i in seq_len(num_groups)) {
    start_index <- (i - 1) * group_size + 1
    end_index <- min(i * group_size, length(df_list))
    group <- df_list[start_index:end_index]
    merged_group <- reduce(group, ~ full_join(.x, .y, by = c("x", "y")))
    merged_list[[i]] <- merged_group
  }

  # Fusionner tous les groupes ensemble
  final_merged <- reduce(merged_list, ~ full_join(.x, .y, by = c("x", "y")))
  
  return(final_merged)
}




# Get background data -----------------------------------------------------
# Sample the background species according to a target species (for aquaculture in the future SDM)
sample_background <- function(bg_df,  
                              sp){
  
  # Extract countries where targeted species occur
  countries_filtered <- bg_df$country[bg_df$species == sp]
  
  # Filter SP background
  df.temp <- bg_df[bg_df$species != sp & countries_filtered %in% countries_filtered,]  
  row.temp <- sample(x = 1:nrow(df.temp), size = 10000, replace = F)  # random coordinates sample, size 10,000
  
  return(sub_df = df.temp[row.temp,])
}

