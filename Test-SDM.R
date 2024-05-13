# SDM FOR AQUACULTURE CANDIDATE SPECIES
setwd("C:/Users/User/Desktop/Internship/Data") # Download and load data (!!! LOCAL ADRESS)

# Source Function
source("C:/Users/User/Documents/GitHub/AquaModel/Functions.R") # Allows to access the function (!!! LOCAL ADRESS)

# # PACKAGES ----------------------------------------------------------------
### LOADED FROM SOURCE CODE FOR THE FUNCTIONS

# DATA IMPORTATION --------------------------------------------------------
# Alpha countries data
data("d.countries") # Data with codes a2 and a3 to convert for flags
# head(d.countries)
countcode <- d.countries %>%
  select(a2, a3)

# Species for SDM 
aquaspecies_df <- read_rds("aquaspecies_df.rds")

# Background species data 
bg <- read_rds("background_data.rds")



# FUNCTIONS ---------------------------------------------------------------
### LOADED FROM SOURCE CODE FOR THE FUNCTIONS


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
# tmin.df <- as.data.frame(tmin, xy = TRUE) # The first dataframe
tmin.mn <- GetMeanDf(tmin, "tmin", "df") # # Compute the mean value and keep as a dataframe (useful?)
# tmin.mnr <- GetMeanDf(layer = tmin, arg = "tmin", type = "raster") # Compute the mean value and re-transform into a SpatRaster object
# Mapplot(tmin.mn, "VNM") # Plot the layer (works with df or raster)

# Wind
# wind.df <- as.data.frame(wind.rs, xy = T)
wind.mn <- GetMeanDf(wind.rs, "wind", "df") # Compute the mean
# Mapplot(wind.mn, "VNM") 

# Merge environmental variables
# Add both layers into the same dataframe 
env <- tmin.mn %>%
  left_join(wind.mn)
env2 <- cbind(tmin.mn, mean_wind = wind.mn$mean_wind) # Method 2

# Convert back to a raster with all the environmental variables
# env.rast <- as_spatraster(env, crs = "EPSG:4326")
# Mapplot(layer = env.rast, ISO = "VNM") # Check if it works

## Species data
# Prepare species data ----------------------------------------------------
### For the species data
mola.data <- mola$data
mola.df <- as.data.frame(mola.data, xy = TRUE)
# plot(mola.df$decimalLatitude, mola.df$decimalLongitude) # Check distribution quickly

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

# Get combined dataframe for all species and environmental data
Combined_df1 <- GetCombinedDf(env, aquaspecies_df, tmin)


