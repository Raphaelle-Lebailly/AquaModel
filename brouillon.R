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


# World data (COMPLICATED SO ON HOLD FOR THE MOMENT)
world <- ne_countries(scale = "medium", returnclass = "sf")
world_vect <- vect(world)
regions <- world$name

# Set base object
BASE <- env_var[[19]]

# Not cropping before!!

# List of species per country
sp_per_count <- readRDS('species_per_country.rds')
spbg_per_count <- readRDS('background_per_country.rds')

# Presence of species 
sp2count <- readRDS("presence_sp_per_count.rds")

# Select the lines in 
# select_pres <- list()
# for(i in 1:dim(sp_per_count)[1]){
#   select_pres <- sp_per_count %>%
#     tidyterra::filter(sp_per_count[i] == 1)
# }



# We're trying to work with index within the raster file instead of the dataframe because 
# Very costly in terms of time and memory
coords <- matrix(c(aquaspecies_df$x, aquaspecies_df$y), ncol = 2) # Coordinates from aquaspecies df
cellnb <- cellFromXY(BASE, xy = coords)
x=colFromX(BASE,x=aquaspecies_df$x)
y=rowFromY(BASE,y=aquaspecies_df$y)
head(cellnb)
bg_df2$env1 = env[[1]][bg_df2$row,bg_df2$col,1]




# Test map
# Current geometry from rnaturalearth package but multiple issues to map the data
# See if there are pb also with cropping per country the data (missing data that are not inlands)
# world2 <- geodata::world(resolution = 5, level = 0, path = tempdir()) # Try to map with this
# plot(world2)





# Faire un lapply pour le modele avec la liste de dataframes dat[[i]] acces dans la liste des dfs
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


# Get a list of the countries where aquaculture species occur
get_countries <- function(data) {
  result <- list()
  for (i in 1:nrow(data)) {
    result[[rownames(data)[i]]] <- colnames(data)[data[i, ] == 1]
  }
  return(result)
}
# List of countries where the species has been observed
count_per_sp <- get_countries(sp2count)
species <- unique(aquaspecies_df$species) # List of species names
countries <- names(sp_per_count) # List of countries names

# Prepare data
bg_df$PA <- 0 # Add presence absence column
aquaspecies_df$PA <- 1 

# Get the background species associated with aquaculture species
get_unique_species <- function(species_names, count_per_sp, spbg_per_count) {
  data <- vector(mode='list', length= length(species_names))
  names(data) <- species_names
  
  for(i in seq_along(species_names)){
    count <- count_per_sp[[species_names[i]]] # Use species name to get countries
    spbg <- unname(unlist(spbg_per_count[count])) # Get the species per country and flatten the list
    t2 <- unique(spbg) # Remove the duplicates
    # Add the result in the list
    data[[i]] <- t2
  } 
  return(data)
}

# Get the list
unique_species <- get_unique_species(species, count_per_sp, sp_per_count)

# Generate per species the equation
# species <- unique(aquaspecies_df$species)

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
