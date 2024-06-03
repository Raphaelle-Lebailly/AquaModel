# 

setwd("C:/Users/User/Desktop/Internship/Data")# Download and load data (!!! LOCAL ADRESS)
# Source Function
source("C:/Users/User/Documents/GitHub/AquaModel/Functions.R") # 

# Species data
aquaspecies_df <- read_rds("aquaspecies_df.rds")
bg_df <- read_rds("background_data_clean.rds")

# Environmental data
## Download
pathbio <- "C:/Users/User/Desktop/Internship/Data/Climate/bio"
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

# World data
world <- ne_countries(scale = "medium", returnclass = "sf")
world_vect <- vect(world)
regions <- world$name

# Set base object
BASE <- env_cropped[[19]]

# Not cropping before!!

# List of species per country
sp_per_count <- readRDS('species_per_country.rds')
bg_per_country <- readRDS('background_per_country.rds')

# We're trying to work with index within the raster file instead of the dataframe because 
# Very costly in terms of time and memory
coords <- matrix(c(aquaspecies_df$x, aquaspecies_df$y), ncol = 2) # Coordinates from aquaspecies df
cellnb <- cellFromXY(BASE, xy = coords)
x=colFromX(BASE,x=aquaspecies_df$x)
y=rowFromY(BASE,y=aquaspecies_df$y)
head(cellnb)
bg_df2$env1 = env[[1]][bg_df2$row,bg_df2$col,1]
