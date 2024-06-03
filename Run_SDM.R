# Run SDM
library(mgcv)
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
# pathtmin <- "C:/Users/User/Desktop/Internship/Data/Climate/tmin" # Manually put the data inside separate files (otherwise, same file)
# pathtmax <- "C:/Users/User/Desktop/Internship/Data/Climate/tmax"
# pathtmean <- "C:/Users/User/Desktop/Internship/Data/Climate/tmean"
# pathsolrad <- "C:/Users/User/Desktop/Internship/Data/Climate/solar_rad"
pathbio <- "C:/Users/User/Desktop/Internship/Data/Climate/bio"
# tmin <- worldclim_global(var = "tmin", res = 0.5, path = pathtmin)
# tmax <- worldclim_global(var = "tmax", res = 0.5, path = pathtmax)


## Import data 
# # tmin
# path_tmin <- paste0(pathtmin,"/wc2.1_30s") # Make a loop in the future for the different files
# raster_tmin <- list.files(path_tmin, pattern = "\\.tif$", full.names = TRUE)
# tmin <- rast(raster_tmin)
# 
# # tmax
# path_tmax <- paste0(pathtmax,"/wc2.1_30s") # Make a loop in the future for the different files
# raster_tmax <- list.files(path_tmax, pattern = "\\.tif$", full.names = TRUE)
# tmax <- rast(raster_tmax)

# tmean
# path_tmean <- paste0(pathtmean,"/wc2.1_30s_tavg") # Make a loop in the future for the different files
# raster_tmean <- list.files(path_tmean, pattern = "\\.tif$", full.names = TRUE)
# tmean <- rast(raster_tmean)
# 
# # Solar radiation
# path_solrad <- paste0(pathsolrad,"/wc2.1_30s_srad") # Make a loop in the future for the different files
# raster_solrad <- list.files(path_solrad, pattern = "\\.tif$", full.names = TRUE)
# solrad <- rast(raster_solrad)

# Bioclimatic variables
pathbio <- "C:/Users/User/Desktop/Internship/Data/Climate/bio"
path_bio <- paste0(pathbio,"/wc2.1_30s_bio") # Make a loop in the future for the different files
raster_bio <- list.files(path_bio, pattern = "\\.tif$", full.names = TRUE) # Can't open this list of files ??
bio <- rast(raster_bio)
# 19 variables 

## Download every environmental variable for aquatic env
dir <- "C:/Users/User/Desktop/Internship/Data/Climate/aqua"
# layers <- download_layers(dataset_id, variables, constraints, fmt = "csv", directory = dir) # fmt is the format, can also be a raster
NO3 <- rast(paste0(dir,"/no3_baseline_2000_2018_depthsurf_8486_b388_df7c_U1716440129770.nc"))
PO4 <- rast(paste0(dir,"/po4_baseline_2000_2018_depthsurf_6006_d51b_00e9_U1716440256420.nc"))
SI <- rast(paste0(dir,"/si_baseline_2000_2018_depthsurf_395f_f84b_becc_U1716440390984.nc"))
bathy <- rast(paste0(dir,"/terrain_characteristics_bea1_f9a7_03c1_U1716440607679.nc"))
surftemp <- rast(paste0(dir,"/thetao_baseline_2000_2019_depthsurf_74ff_39fa_9adc_U1716440102349.nc"))
prim_prod <- rast(paste0(dir,"/phyc_baseline_2000_2020_depthsurf_7d39_02af_cdbd_U1716500021103.nc"))
# List of variables
env_var <- list(NO3, PO4, SI, bathy, surftemp, prim_prod)
bio_names <- c("tmean_ann", "diurn_mean_range", "isotherm","temp_seas", "tmax", "tmin", "tmean_ann_range",
               "tmin_wet_quart", "tmin_fry_quart", "tmin_warm_quart", "tmin_cold_quart", "prec_ann", "prec_wet",
               "prec_dry", "prec_var", "prec_wet_quart", "prec_dry_quart", "prec_warm_quart", "prec_cold_quart")

names(bio) <- bio_names
bio_list <- lapply(1:nlyr(bio), function(i) bio[[i]])
env_var <- c(env_var, bio_list) # Add the extracted and renamed layers
rm(bio_list)
gc()
# Retrieve data for countries extent
world <- ne_countries(scale = "medium", returnclass = "sf")
world_vect <- vect(world)
regions <- world$name

# Cropping to the desired extent
env_cropped <- GetCroppedRaster(env_var, 'India')
# rm(bio)
# rm(env_var)
gc()

# Check
# plot(env_cropped[[2]])


# FUNCTIONS ---------------------------------------------------------------
### LOADED FROM SOURCE CODE FOR THE FUNCTIONS

# PROTOCOLE ---------------------------------------------------------------
# Test if every function is working well and giving generalizable results.

#### Environmental data management
# 1. Set base (any variable from 'bio')
BASE <- env_cropped[[19]]


# 2. Adjust geometry of the other rasters
# tmax.rs <- resample(env_cropped2[[2]], BASE, "bilinear")
# Resample all of the environmental data given the base layer
env_rs <- list()
for (i in seq_along(env_cropped)) {
  env_rs[[i]] <- resample(env_cropped[[i]], BASE, "bilinear") # OK
}


# 3. Get mean value
# tmin.mn <- GetMeanDf(tminVNM, "tmin", "df")
# tmax.mn <- GetMeanDf(tmax.rs, "tmax", "df") 
# For the list of env data
# NO NEED FOR BIO-ORACLE DATA, ALREADY GOOD FORMAT (get the dataframes only)
env_mn <- list()
for (i in seq_along(env_rs)) {
  env_mn[[i]] <- as.data.frame(env_rs[[i]], xy = TRUE)
}

rm(bio)
rm(NO3, PO4, prim_prod,SI, surftemp, bathy)
gc()
# 4. Merge dataframes (mean or not)
# env <- tmin.mn %>%
#   left_join(tmax.mn)

# Try to find optimized function
env_mg <- GetMerged(env_mn, group_size = length(env_var))
rm(env_mn)
gc()
# 5. Prepare the species data
# Aquaspecies are the species of interest
# bg_df is the background species 
# These data have already been cleaned (delete the flags etc.)

# 6. Assemble df with species + environmental data = Presence dataframe

sp <- as.matrix(aquaspecies_df)
final <- as.matrix(env_mg)
base <- BASE
rm(aquaspecies_df)

# Changer pour que ce soit compatible avec des matrices (-lourd que dataframe)
GetCombinedDf <- function(final, sp, base){
  coord <- matrix(c(sp[,"x"], sp[,"y"]), ncol = 2) # Coordinates from species df
  s_sp <- cellFromXY(base, xy = coord) 
  coord2 <- matrix(c(final[,"x"], final[,"y"]), ncol = 2) # Coordinates from env df
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


pres <- GetCombinedDf(final, sp, BASE) # A bit long, could use some optimizing
world <- ne_countries(scale = "medium", returnclass = "sf") # No presence of France geometry



# 7. Get Pseudo-absences df
# Select random subset of the background dataset (n = 10,000)
set.seed(10)
# Get the background subset
subbg <- GetSubBg(bg_df, 'India')
# Get the pseudoabsences dataframe
pseudoabs <- GetCombinedDf(env_mg, subbg, BASE)

pseudoabs <- GetCombinedDf(env_mg, bg_df, BASE)

# Check for NAs
colSums(is.na(pres))
colSums(is.na(pseudoabs))
colSums(!is.na(pres))
colSums(!is.na(pseudoabs))

# 8. Get Presence/Absence dataframe
# Get the list of the combined dataframes

dat <- GetModelData(pseudoabs, pres) # Works but 20 occurrences might be too much, or try to counter balance with more aquaculture species
# Here, 5 occurrences to have at least some data in the list 


# Clean the environment
# rm(list = ls())
# gc()

# MODEL -------------------------------------------------------------------

# Faire un lapply pour le modele avec la liste de dataframes dat[[i]] acces dans la liste des dfs
sim_func <- function(names_x, name_y,dat){
  tmp_sdm <- gam(formula(paste(name_y,"~",paste(names_x,collapse='+'))),family = binomial, data=dat,select = TRUE, method="GCV.Cp")
  return(tmp_sdm)
}
name_y <- "PA"
col_names <- colnames(dat[[1]]) # Retrieve colnames to have variable names
selected_col_names <- col_names[3:(length(col_names) - 2)]
names_x <- selected_col_names # Then you can set your list of predictor variables
names_x1 <- paste("s(",names_x,",k=5)",sep="") # and for all variables that you want to allow a non-linear fit
# Do a loop to get all the predictions for the whole list of dataframes
sdm_obj <- list()
for (i in seq_along(dat)) {
  sdm_obj[[i]] <- sim_func(names_x1,name_y, dat[[i]])
}
sdm_obj2=sim_func(names_x1,name_y, dat[[1]]) # get back the sdm object
# Newdat is supposed to be the full coverage of the area of interest (rasters covering the whole area)
Newdat <- env_mg
# Loop for predictions
pred <- list()
Newpred <- rep(list(Newdat), length(dat))
for (i in seq_along(sdm_obj)) {
  pred[[i]] <- predict.gam(sdm_obj[[i]], newdata=Newdat) # Maybe not the quickest way
  Newpred[[i]]$predictions <- pred[[i]]
}

# pred2 <- predict.gam(sdm_obj[[1]], newdata=Newdat) # new data would be the environments that you want to extrapolate the model to.
Newdat$predictions <- pred # Add the predictions to the new data

# Plot the results
x11()
ggplot(Newpred[[2]], aes(x = x, y = y, fill = predictions)) + # So freaking long to display
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Probability of predicted presence of the species",
       x = "Latitude",
       y = "Longitude",
       fill = "Probability of presence")
gc()
