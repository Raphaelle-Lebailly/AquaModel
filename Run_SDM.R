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

aquaspecies_df <- read_rds("aquaspecies_df.rds") # Species for SDM 
bg_df <- read_rds("background_data_clean.rds") # Background species data 
sp_per_count <- read_rds("species_per_country.rds")
aquasp_per_count <- read_rds("presence_sp_per_count.rds")
spbg_per_count <- read_rds("background_per_country.rds")



# Environmental data
## Download
pathbio <- "C:/Users/User/Desktop/Internship/Data/Climate/bio"

## Import data 
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
rm(NO3, PO4, SI, bathy, surftemp, prim_prod)
names_x <- c("no3_mean", "po4_mean", "si_mean", "bathymetry_max", "thetao_mean", "phyc_mean",
             "tmean_ann", "diurn_mean_range", "isotherm","temp_seas", "tmax", "tmin", "tmean_ann_range",
             "tmin_wet_quart", "tmin_fry_quart", "tmin_warm_quart", "tmin_cold_quart", "prec_ann", "prec_wet",
             "prec_dry", "prec_var", "prec_wet_quart", "prec_dry_quart", "prec_warm_quart", "prec_cold_quart")

names(bio) <- names_x
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
rm(bio)
rm(env_var)
gc()

# Check
plot(env_cropped[[2]])


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

# 3. Get mean value
# tmin.mn <- GetMeanDf(tminVNM, "tmin", "df")
# tmax.mn <- GetMeanDf(tmax.rs, "tmax", "df") 
# For the list of env data
# NO NEED FOR BIO-ORACLE DATA, ALREADY GOOD FORMAT (get the dataframes only)
env_mn <- list()
for (i in seq_along(env_rs)) {
  env_mn[[i]] <- as.data.frame(env_rs[[i]], xy = TRUE)
}
rm(env_rs)
gc()
# 4. Merge dataframes (mean or not)
# env <- tmin.mn %>%
#   left_join(tmax.mn)
# Try to find optimized function

# Fusionner tous les groupes ensemble

env_mg <- GetMerged(env_mn, group_size = length(env_var))
# 5. Prepare the species data
# Aquaspecies are the species of interest
# bg_df is the background species 
# These data have already been cleaned (delete the flags etc.)
# 6. Assemble df with species + environmental data = Presence dataframe
rm(env_mn)
gc()
pres <- GetCombinedDf(env_mg, aquaspecies_df, BASE) # First function (primitive)
gc()
# 7. Get Pseudo-absences df
# Select random subset of the background dataset (n = 10,000)
set.seed(10)
# Get the background subset
subbg <- GetSubBg(bg_df, 'India') # First function (primitive)
# Get the pseudoabsences dataframe
pseudoabs <- GetCombinedDf(env_mg, subbg, BASE)
gc()
# Check for NAs
colSums(is.na(pres))
colSums(is.na(pseudoabs))
colSums(!is.na(pres))
colSums(!is.na(pseudoabs))
# 8. Get Presence/Absence dataframe
# Get the list of the combined dataframes
dat <- GetModelData(pseudoabs, pres, 3) # Works but 20 occurrences might be too much, or try to counter balance with more aquaculture species
# Here, 3 occurrences to have at least some data in the list 
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
names_x <- c("no3_mean", "po4_mean", "si_mean", "bathymetry_max", "thetao_mean", "phyc_mean",
             "tmean_ann", "diurn_mean_range", "isotherm","temp_seas", "tmax", "tmin", "tmean_ann_range",
             "tmin_wet_quart", "tmin_fry_quart", "tmin_warm_quart", "tmin_cold_quart", "prec_ann", "prec_wet",
             "prec_dry", "prec_var", "prec_wet_quart", "prec_dry_quart", "prec_warm_quart", "prec_cold_quart")
names_x1 <- paste("s(",names_x,",k=5)",sep="") # and for all variables that you want to allow a non-linear fit
gc()
# Do a loop to get all the predictions for the whole list of dataframes
sdm_obj <- list()
for (i in seq_along(dat)) {
  sdm_obj[[i]] <- sim_func(names_x1,name_y, dat[[i]])
}
sdm_obj2=sim_func(names_x1,name_y, dat[[3]]) # get back the sdm object
sdm_obj2
# Newdat is supposed to be the full coverage of the area of interest (rasters covering the whole area)
Newdat <- env_mg
# Loop for predictions
pred <- list()
Newpred <- rep(list(Newdat), length(dat))
for (i in seq_along(sdm_obj)) {
  pred[[i]] <- predict.gam(sdm_obj[[i]], newdata=Newdat, type = "response") # ADD type = 'response' to have results on 0-1 scale
  Newpred[[i]]$predictions <- pred[[i]]
}

# pred2 <- predict.gam(sdm_obj[[1]], newdata=Newdat) # new data would be the environments that you want to extrapolate the model to.
# Newdat$predictions <- pred # Add the predictions to the new data

#### Plot the results -----------------------------------------------------

# world_vect <- st_read(system.file("shape/nc.shp", package="sf"))
country_geom <- world[world$name == 'India', ]
reg_ext <- ext(country_geom)

# x11() # Trop Lourd
image <- ggplot(Newpred[[3]]) + # So freaking long to display
  geom_tile(aes(x = x, y = y, fill = predictions)) +
  geom_sf(data = country_geom, fill = NA, color = "black", size = 0.4) +
  scale_fill_viridis_c() +
  labs( x = "Latitude",
        y = "Longitude",
        fill = "Probability of presence")

# coord_sf(xlim = range(env_mg$x), ylim = range(env_mg$y), expand = FALSE) # Ajuster les limites  
# setwd("C:/Users/User/Desktop/Internship/Images")
ggsave("India_3.jpg", image, width = 10, height = 8)
gc()
