# Goal: implement the background data of all the countries containing the species of interest


# Load data, packages and functions ---------------------------------------

setwd("C:/Users/User/Desktop/Internship/Data") # Download and load data (!!! LOCAL ADRESS)
source("C:/Users/User/Documents/GitHub/AquaModel/Functions.R") # Load functions

### SPECIES DATA ###
bg_df <- read_rds("real_bg.rds") # All GBIF data (14,757 species)
plot(bg_df$x, bg_df$y)

# Access rfishbase data to retrieve fishbase aquaculture species names
library(rfishbase)
status <- c('commercial', 'experimental', 'likely future use') # Change if we want different status
sp_fb <- fb_tbl("species") %>% 
  mutate(species = paste(Genus, Species)) %>% 
  select(species, UsedforAquaculture) %>% 
  tidyterra::filter(UsedforAquaculture %in% status) %>% 
  tidyterra::rename(Aquaculture_status = UsedforAquaculture)
# 449 species names for different aquaculture status

# Join species occurrences with aquaculture status
bg_df <- bg_df %>% 
  left_join(sp_fb, by = "species")
rm(sp_fb) ; gc()
# Retrieve species with non NA aquaculture status
aquaspecies_df <- na.omit(bg_df)
plot(aquaspecies_df$x, aquaspecies_df$y) # Check data repartition

# We mostly have data in western and central Africa and eastern North America



# Visualize the data in the world (plot)
# data("World")
# img <- bg_df %>%
#   tidyterra::filter(species == "Abramis brama") %>%
#   ggplot(aes(x = x, y = y)) + 
#   # geom_sf(world) +
#   scale_fill_viridis_c() +
#   theme_minimal() +
#   theme(panel.background = element_rect(fill = 'light blue'))
# x11()
# img


### ENVIRONMENTAL DATA ###
## Download
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
rm(bio_list, bio)
gc()

# World data (COMPLICATED SO ON HOLD FOR THE MOMENT)
world <- ne_countries(scale = "medium", returnclass = "sf")
world_vect <- vect(world)
regions <- world$name

# Set base object # Not cropping before!!
# BASE <- env_var[[19]] # Fine grid (terrestrial raster)
BASE <- env_var[[1]] # Gross grid (aquatic raster)

# List of species per country
# sp_per_count <- readRDS('species_per_country.rds')
# spbg_per_count <- readRDS('background_per_country.rds')
# # Presence of species 
# sp2count <- readRDS("presence_sp_per_count.rds")



# We're trying to work with index within the raster file instead of the dataframe because 
# Very costly in terms of time and memory
# coords <- matrix(c(aquaspecies_df$x, aquaspecies_df$y), ncol = 2) # Coordinates from aquaspecies df
# cellnb <- cellFromXY(BASE, xy = coords)
# x=colFromX(BASE,x=aquaspecies_df$x)
# y=rowFromY(BASE,y=aquaspecies_df$y)
# head(cellnb)
# bg_df2$env1 = env[[1]][bg_df2$row,bg_df2$col,1]


# Data management ---------------------------------------------------------

### ENVIRONMENTAL DATA ###

# Here, we decide arbitrarily to target India in order to have an interesting example
COUNTRY <- 'India'

# Crop the raster to the extent wanted
env_crop <- GetCroppedRaster(list_raster = env_var, extent = COUNTRY)
rm(NO3, PO4, SI, bathy, surftemp, prim_prod, env_var) ; gc() # Remove unused raster

# Resample: adapt all raster geometry (resolution) to one reference raster
env_rs <- list()
for (i in seq_along(env_crop)) {
  env_rs[[i]] <- resample(env_crop[[i]], BASE, "bilinear") # OK
}
gc()
# Convert into dataframe
env_df <- list()
for (i in seq_along(env_rs)) {
  env_df[[i]] <- as.data.frame(env_rs[[i]], xy = TRUE)
}
rm(env_crop, env_rs) ; gc()

# Merge (+ cell ID)
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
env_mg <- GetMerged(df_list = env_df, group_size = length(env_df))
rm(env_df) ; gc()


### SPECIES DATA ###

# Test map
# Current geometry from rnaturalearth package but multiple issues to map the data
# See if there are pb also with cropping per country the data (missing data that are not inlands)
# world2 <- geodata::world(resolution = 5, level = 0, path = tempdir()) # Try to map with this
# plot(world2)
library(SeaVal) # Add countries names according to coordinates

# Add countries column in full bg dataframe....
bg_df2 <- tidyterra::rename(bg_df, lon = x, lat = y)
world <- ne_countries(scale = "medium", returnclass = "sf") # Problematic (missing values) (rnaturalearth)
bg_df2 <- as.data.table(bg_df2)
bg_df2 <- add_country_names(bg_df2, regions = world) # With SeaVal package
bg_df2$country <- gsub(":.*", "", bg_df2$country)  # Delete unused arguments (subregions)
bg_df2 <- tidyterra::rename(bg_df2, x = lon, y = lat) # Rename back the coordinates (useful for later)
# rm(bg_df) ; gc() # Clean memory (CM)

# OTHER METHOD (does not work yet)
# world2 <- getMap(resolution = "high") # Load world data (rworldmap)
# coordinates(bg_df2) <- ~ lon + lat
# proj4string(bg_df2) <- proj4string(world)
# countries <- over(bg_df2, world) # Geoloc inverse
# bg_df2 <- as.data.frame(bg_df2)
# bg_df2$country <- countries$ADMIN
# bg_df2 <- tidyterra::rename(bg_df2, x = lon, y = lat) # Rename back the coordinates (useful for later)

# ...AND aquaspecies dataframe 
# Get the list of species per country > threshold (occurrences)
# OCC <- 5 # Set threshold number of minimal occurrences 
# subdf_thrsh <- aquaspecies_df %>% # Get species occurrences for all countries (> threshold)
#   group_by(species) %>%
#   tidyterra::filter(n() >= OCC) %>% 
#   ungroup() # 348 sp for OCC = 5
# subdf_thrsh <- droplevels(subdf_thrsh)

# species_counts <- table(aquaspecies_df$species) # Useless??
# species_above_threshold <- names(species_counts[species_counts >= OCC])
# subdf_thrsh <- aquaspecies_df[aquaspecies_df$species %in% species_above_threshold, ]

# Add countries
aq_df2 <- tidyterra::rename(aquaspecies_df, lon = x, lat = y)
aq_df2 <- as.data.table(aq_df2)
aq_df2 <- add_country_names(aq_df2)
aq_df2$country <- gsub(":.*", "", aq_df2$country)  # Delete unused arguments (subregions)
aq_df2 <- tidyterra::rename(aq_df2, x = lon, y = lat) # Rename back the coordinates (useful for later)
# rm(aquaspecies_df, subdf_thrsh) ; gc() # CM


# Select subset of the targeted country for aquaculture species

# Filter to target
# Aquaculture species
subsp_count <- aq_df2 %>% # Filter by targeted country 
  tidyterra::filter(country == COUNTRY)

OCC <- 5 # Set threshold number of minimal occurrences 
aq_df_count <- subsp_count %>% # Get species occurrences for all countries (> threshold)
  group_by(species) %>%
  tidyterra::filter(n() >= OCC) %>% 
  ungroup()
occurences2 <- table(aq_df_count$species) # Check if threshold respected
print(occurences2) # OK

# Set target species (later on, vector of species found in the targeted area )
SPECIES <-  "Anabas testudineus"
plot(aq_df_count$x, aq_df_count$y)

# Get Country extent
name_reg <- paste0(COUNTRY)
region <- world_vect[world_vect$name == name_reg, ]
reg_ext <- ext(region)
reg_ext

# Get background species according to targeted species
spbg <- sample_background(bg_df = bg_df2, sp = SPECIES) # Select subdataframe of random background species where targeted aquaculture species occur



# Get a list of the countries where aquaculture species occur
# get_countries <- function(data) {
#   result <- list()
#   for (i in 1:nrow(data)) {
#     result[[rownames(data)[i]]] <- colnames(data)[data[i, ] == 1]
#   }
#   return(result)
# }
# # List of countries where the aquaculture species has been observed
# count_per_sp <- get_countries(sp2count)
# species <- unique(aquaspecies_df$species) # List of species names
# countries <- names(sp_per_count) # List of countries names

# Prepare data
spbg$PA <- 0 # Add presence absence column
subsp$PA <- 1 

# Merge the species dataframes (presence/absence df)
spPA <- rbind(subsp, spbg)

### MERGE ALL ###
dat <- GetCombinedDf(final = env_mg, sp = spPA, base = BASE)








# Generate per species the equation
get_species_df <- function(speciesname, unique_sp) {
  # Sélectionner le subset pour l'espèce donnée dans aquaspecies_df
  sub_sp <- aquaspecies_df %>%
    tidyterra::filter(species == speciesname)
  
  # Sélectionner le subset pour les espèces associées dans unique_sp
  species_list <- unique_sp[[speciesname]]
  sub_sp_bg <- bg_df %>%
    tidyterra::filter(species %in% species_list & species != speciesname)
  
  # Combinaison des deux subsets
  df <- bind_rows(sub_sp, sub_sp_bg)
  
  # Supprimer les doublons
  df <- distinct(df)
  
  return(df)
}

# PAS TOTALEMENT CORRECT CAR APPEL DES ESPECES BACKGROUND PAR NOM AU LIEU DE LOCATION
# CE QUI VEUT DIRE APPEL DE TOUTES LES LOCALISATIONS OU LES ESPECES BACKGROUND SONT 
# ET NON JUSTE CELLES D'INTERET (pays de l'espèce ciblee)

# Test
sp_comb_df <- get_species_df('Abramis brama', unique_species) # OK

# Call this function in a loop for species dataframes


# Get environmental data for areas targeted
get_crop_env <- function(species_names, count_per_sp){
  for(i in seq_along(species_names)){
    count <- count_per_sp[[species_names[i]]]
  }
  for(i in seq_along(count)){
    name_reg <- paste0(i)
    region <- world_vect[world_vect$name == name_reg, ]
    reg_ext <- ext(region)
    rast_ext <- list()
    for (i in seq_along(list_raster)) {
      rast_ext[[i]] <- crop(list_raster[[i]], reg_ext)
    }
  }
  return(rast_ext)
}








# SDM ---------------------------------------------------------------------


# FtestVirtual()# Faire un lapply pour le modele avec la liste de dataframes dat[[i]] acces dans la liste des dfs
sim_func <- function(names_x, name_y,dat){
  tmp_sdm <- gam(formula(paste(name_y,"~",paste(names_x,collapse='+'))),family = binomial, data=dat,select = TRUE, method="GCV.Cp")
  return(tmp_sdm)
}
name_y <- "PA"
# col_names <- colnames(dat[[1]]) # Retrieve colnames to have variable names
# selected_col_names <- col_names[3:(length(col_names) - 2)]
# List of selected variables
names_x <- c("no3_mean", "po4_mean", "si_mean", "bathymetry_max", "thetao_mean", "phyc_mean",
           "tmean_ann", "diurn_mean_range", "isotherm","temp_seas", "tmax", "tmin", "tmean_ann_range",
               "tmin_wet_quart", "tmin_fry_quart", "tmin_warm_quart", "tmin_cold_quart", "prec_ann", "prec_wet",
               "prec_dry", "prec_var", "prec_wet_quart", "prec_dry_quart", "prec_warm_quart", "prec_cold_quart")
names_x1 <- paste("s(",names_x,",k=5)",sep="") # and for all variables that you want to allow a non-linear fit
# Do a loop to get all the predictions for the whole list of dataframes



# --------

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
