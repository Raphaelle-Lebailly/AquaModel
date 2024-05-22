# Run SDM

setwd("C:/Users/User/Desktop/Internship/Data")# Download and load data (!!! LOCAL ADRESS)
# Source Function
source("C:/Users/User/Documents/GitHub/AquaModel/Functions.R") # Allows to access the functions (!!! LOCAL ADRESS)
# source("C:/Users/User/Documents/GitHub/AquaModel/Data.R") # Allows to access the data (!!! LOCAL ADRESS)

# # PACKAGES ----------------------------------------------------------------
### LOADED FROM SOURCE CODE FOR THE FUNCTIONS

# DATA IMPORTATION --------------------------------------------------------
# Alpha countries data
# data("d.countries") # Data with codes a2 and a3 to convert for flags
# countcode <- d.countries %>%
#   select(a2, a3)
# Species for SDM 
aquaspecies_df <- read_rds("aquaspecies_df.rds")
# Background species data 
bg_df <- read_rds("background_data_clean.rds")

# Environmental data
## Download
pathtmin <- "C:/Users/User/Desktop/Internship/Data/Climate/tmin" # Manually put the data inside separate files (otherwise, same file)
pathtmax <- "C:/Users/User/Desktop/Internship/Data/Climate/tmax"
# tmin <- worldclim_global(var = "tmin", res = 0.5, path = pathtmin)
# tmax <- worldclim_global(var = "tmax", res = 0.5, path = pathtmax)

## Import data 
# tmin
path_tmin <- paste0(pathtmin,"/wc2.1_30s") # Make a loop in the future for the different files
raster_tmin <- list.files(path_tmin, pattern = "\\.tif$", full.names = TRUE)
tmin <- rast(raster_tmin)


# tmax
path_tmax <- paste0(pathtmax,"/wc2.1_30s") # Make a loop in the future for the different files
raster_tmax <- list.files(path_tmax, pattern = "\\.tif$", full.names = TRUE)
tmax <- rast(raster_tmax)


#### Select subsets of the rasters (so that we can work with it)
vietnam_extent <- ext(102, 110, 8, 24)
tminVNM <- crop(tmin, vietnam_extent)
tmaxVNM <- crop(tmax, vietnam_extent)
rm(tmin)
rm(tmax)
gc()

# FUNCTIONS ---------------------------------------------------------------
### LOADED FROM SOURCE CODE FOR THE FUNCTIONS

# PROTOCOLE ---------------------------------------------------------------
# Test if every function is working well and giving generalizable results.

#### Environmental data management
# 1. Set base
BASE <- tminVNM

# 2. Adjust geometry of the other rasters
tmax.rs <- resample(tmaxVNM, BASE, "bilinear")

# 3. Get mean value
tmin.mn <- GetMeanDf(tminVNM, "tmin", "df")
tmax.mn <- GetMeanDf(tmax.rs, "tmax", "df") 

# 4. Merge dataframes (mean or not)
env <- tmin.mn %>%
  left_join(tmax.mn)

# 5. Prepare the species data
# Aquaspecies are the species of interest
# bg_df is the background species 
# These data have already been cleaned (delete the flags etc.)

# 6. Assemble df with species + environmental data = Presence dataframe
pres <- GetCombinedDf(env, aquaspecies_df, BASE)
# pres <- pres[,-3]

# 7. Get Pseudo-absences df
# Select random subset of the background dataset (n = 10,000)
set.seed(10)

vietnam_extent <- list(
  min_lon = 102,
  max_lon = 110,
  min_lat = 8,
  max_lat = 24
)
# Get the background subset
subbg <- GetSubBg(bg_df, vietnam_extent)
# Get the pseudoabsences dataframe
pseudoabs <- GetCombinedDf(env, subbg, BASE)

# Check for NAs
colSums(!is.na(pres))
colSums(!is.na(pseudoabs))

# 8. Get Presence/Absence dataframe





# Clean the environment
# rm(list = ls())
# gc()

# Get the data for the targeted country (on hold for now)
# VNM <- gadm(country = "VNM", level = 0, path=tempdir())
# bg_rast <- as_spatraster(bg_df, crs = "EPSG:4326")
# var.border <- mask(bg_rast, VNM)
# test <- st_as_sf(bg_df, coords = c("x", "y"), crs = "EPSG:4326")
# subset_sf <- st_intersection(test, VNM)
# 
# # Convertir le résultat de retour en dataframe si nécessaire
# subset_df <- as.data.frame(subset_sf)

# MODEL -------------------------------------------------------------------

# sim_func <- function(names_x, name_y,dat){
#   tmp_sdm<-gam(formula(paste(name_y,"~",paste(names_x,collapse='+'))),family = binomial, data=dat,select = TRUE, method="GCV.Cp")
#   return(tmp_sdm)
# }
# 
# names_x= c("Bio1","Bio2") # Then you can set your list of predictor variables
# names_x1=paste("s(",names_x,",k=5)",sep="") #and for all variables that you want to allow a non-linear fit
# sdm_obj=sim_func(names_x1,name_y, dat) #get back the sdm object
# predict.gam(sdm_obj, newdata=newdat) #new data would be the environments that you want to extrapolate the model to.


