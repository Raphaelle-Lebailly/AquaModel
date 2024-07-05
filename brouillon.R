# Goal: implement the background data of all the countries containing the species of interest
library(mgcv)


# Load data, packages and functions ---------------------------------------

setwd("C:/Users/User/Desktop/Internship/Data") # Download and load data (!!! LOCAL ADRESS)
source("C:/Users/User/Documents/GitHub/AquaModel/Functions.R") # Load functions

#### ENVIRONMENTAL DATA ####
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


# Set base object # Not cropping before!!
# BASE <- env_var[[19]] # Fine grid (terrestrial raster)
BASE <- env_var[[1]] # Coarser grid (aquatic raster)


# List of species per country (OBSOLETE FILES)
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


#### SPECIES DATA ####
## Download
bg_df <- read_rds("real_bg.rds") # All GBIF data (14,757 species)
# plot(bg_df$x, bg_df$y)

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
# plot(aquaspecies_df$x, aquaspecies_df$y) # Check data repartition

# We mostly have data in western and central Africa and eastern North America

# Import data borders and coastlines
world <- ne_countries(scale = "medium", returnclass = "sf") # Borders of the countries
world <- world[world$continent != "Antarctica", ] # Erase Antartica (issues with intersect function and useless)
world_vect <- vect(world)
regions <- world$name
coastline <- ne_download(scale = "medium", type = "coastline", category = "physical", returnclass = "sf") # Get coastlines
coastline_vect <- vect(coastline) # Convert to spatvector object
intersected <- terra::intersect(world_vect, coastline_vect) # Get countries list which have coastlines
countries_with_coastline <- unique(terra::values(intersected)$name)


# Call the object 'regions' to have the countries names (spell them right)
regions <- sort(regions) # Sort by alphabetical order


# Here, we decide arbitrarily to target India in order to have an interesting example
COUNTRY <- 'India'
which(regions == COUNTRY) # Check if present in the list
name_reg <- paste0(COUNTRY)
region <- world_vect[world_vect$name == name_reg, ] # Get country geometry

# Get buffered targeted region
# Draw polygon from coastline
intersect <- terra::intersect(coastline_vect, region)
# Crop coastline in targeted area
crop <- crop(region, intersect)
# Add a buffer everywhere following the border
buffer <- buffer(crop, width = 22000) # Apply 22km buffer on all coasts
# Combine Geometries
combined <- combineGeoms(region, buffer) # Final geometry

# Intersect the points of interest
lonlat <- cbind(aquaspecies_df$x, aquaspecies_df$y) # Get coordinates
pts <- vect(lonlat, crs = "WGS 84") # As SpatVector
inter_points <- terra::intersect(combined, pts) # Intersect points and buffer
coords <- geom(inter_points) # Extract coordinates with points
points_df <- data.frame(x = coords[, "x"], y = coords[, "y"]) # 

# Visualize the data in the world (plot) or in the targeted area
ggplot(region) +
  geom_sf(color = "black") +
  geom_sf(fill = "antiquewhite1") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'lightblue')) +
  geom_point(data = points_df, aes(x = x, y = y), size = 1, shape = 21, fill = "blue") +
  coord_sf(xlim = c(ext(region)[1], ext(region)[2]), ylim = c(ext(region)[3], ext(region)[4]), expand = FALSE) +
  labs(title = paste0("Species location points in ", COUNTRY)) +
  xlab("Longitude") + ylab("Latitude") +
  theme(plot.title = element_text(hjust = 0.5))

rm(lonlat, pts, coords, inter_points) ; gc()


##############################*
##############################*
COUNTRY <- 'Finland'
regions <- world$name
regions <- sort(regions)
which(regions == COUNTRY) # Check if present in the list
name_reg <- paste0(COUNTRY)
region <- world_vect[world_vect$name == name_reg, ] # Get country geometry
plot(region)

intersect <- terra::intersect(coastline_vect, region)
plot(intersect)
# Crop coastline in targeted area
crop <- crop(region, intersect)
plot(crop)
# Add a buffer everywhere following the border
buffer <- buffer(crop, width = 22000, singlesided = TRUE) # Apply 22km buffer on all coasts
# plot(intersect, type = "l")
# lines(buffer,  col = "red")
plot(buffer)

# Combine Geometries
combined_ <- combineGeoms(region, buffer) # Final geometry
plot(combined_)
# lines(region)



regions2 <- regions # Everything in order
r <-  regions 

which(r == 'China')
r <- r[-45]
which(r == 'Finland')
r <- r[-72]



regions <- r
# regions <- "China"
buffered_regions <- list()

for(i in seq_along(regions)){
  # Isolate region geometry
  region <- world_vect[world_vect$name == regions[i], ]
  
  if(regions[i] %in% countries_with_coastline){
    # Draw polygon from coastline
    intersect <- terra::intersect(coastline_vect, region)
    
    # Crop coastline in targeted area
    crop <- crop(region, intersect)
    
    # Add a buffer everywhere following the border
    buffer <- buffer(crop, width = 22000) # Apply 22km buffer on all coasts
    
    # Combine Geometries
    combined <- combineGeoms(region, buffer)
    
    # Crop raster
    buffered_regions[i] <- combined

  } else {
    buffered_regions[i] <- region
  }
}

buffered_regions_vect <- vect(buffered_regions)

plot(buffered_regions_vect, col = "red")

plot(world_vect[world_vect$name == "Finland", ])

plot(buffered_regions_vect[[240]])

##############################*
##############################*

saveRDS(buffered_regions_vect, "all_buff_countries_vect.rds")

# Data management ---------------------------------------------------------

#### ENVIRONMENTAL DATA ####

# Crop the raster to the extent wanted
env_crop <- GetCroppedRaster(list_raster = env_var, extent = COUNTRY) # "Empty geometry" line if country misspelled
rm(NO3, PO4, SI, bathy, surftemp, prim_prod) ; gc() # Remove unused raster
rm(env_var)

plot(env_crop[[1]]) # Check

# Resample: adapt all raster geometry (resolution) to one reference raster
env_rs <- list()
for (i in seq_along(env_crop)) {
  env_rs[[i]] <- resample(env_crop[[i]], BASE, "bilinear") # OK
}
rm(env_crop) ; gc()

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
bg_df2 <- as.data.table(bg_df2)
bg_df2 <- add_country_names(bg_df2, regions = world) # With SeaVal package
bg_df2$country <- gsub(":.*", "", bg_df2$country)  # Delete unused arguments (subregions)
bg_df2 <- tidyterra::rename(bg_df2, x = lon, y = lat) # Rename back the coordinates (useful for later)
# rm(bg_df) ; gc() # Clean memory (CM)


# ...AND aquaspecies dataframe 
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
# USELESS TO FILTER BY COUNTRY: WE STUDY SPECIES IN ALL AREAS POSSIBLE AND NOT LIMITED BY ADMINISTRATIVE BORDERS
# WE WANT TO FOCUS ON SPECIES PRESENT IN A COUNTRY OF INTEREST, NOTLIMIT THE AREA TO THE COUNTRY CONCERNING THE AREA OF DISTRIBUTION

# subsp_count <- aq_df2 %>% # Filter by targeted country 
  # tidyterra::filter(country == COUNTRY)

OCC <- 5 # Set threshold number of minimal occurrences 
aq_df_occ <- aq_df2 %>% # Get species occurrences for all countries (> threshold)
  group_by(species) %>%
  tidyterra::filter(n() >= OCC) %>% 
  ungroup()
rm(aq_df2) ; gc()
occurences2 <- table(aq_df_occ$species) # Check if threshold respected
print(occurences2) # OK, 147 species

# Target 1 species
SPECIES <-  "Anabas testudineus" # Here example of species present in India (we don't know where else it's present)
aq_df_sp <- aq_df_occ %>% 
  tidyterra::filter(species == SPECIES) # Filter only the species of interest

plot(aq_df_sp$x, aq_df_sp$y, main = SPECIES) # Plot the points

# Visualize the points of the targeted species in India
ggplot(data = world) +
  geom_sf(color = "black") +
  geom_sf(fill = "antiquewhite1") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'light blue'))+
  coord_sf(xlim = c(ext(india)[1], ext(india)[2]), ylim = c(ext(india)[3], ext(india)[4]), expand = FALSE) +
  geom_point(data = aq_df_sp, aes(x = x, y = y), size = 2,  # Put the dots from the species filtered subdataframe
             shape = 21, fill = "green") 

# Looks very much in the country so does not make sense that I'm missing the data in the final dataframe... pb of matching?
# Only 5 points appear out of 9.

# In the world
ggplot(data = world) +
  geom_sf(color = "black") +
  geom_sf(fill = "antiquewhite1") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'light blue'))+
  # coord_sf(xlim = c(ext(india)[1], ext(india)[2]), ylim = c(ext(india)[3], ext(india)[4]), expand = FALSE) +
  geom_point(data = aq_df_occ, aes(x = x, y = y), size = 1,  # Put the dots from the species filtered subdataframe
             shape = 21, fill = "red") +
  ggtitle(label = c("Species location points in the world (occurrence > 5)"))


# Get background species according to targeted species
sample_background <- function(bg_df, sp){ # Modify conditions about including NAs countries. Add the buffer. 
  
  # Extract countries where targeted species occur
  countries_filtered <- bg_df$country[bg_df$species == sp]
  
  # Filter SP background
  df.temp <- bg_df[bg_df$species != sp & countries_filtered %in% countries_filtered,]  
  row.temp <- sample(x = 1:nrow(df.temp), size = 10000, replace = F)  # random coordinates sample, size 10,000
  
  return(sub_df = df.temp[row.temp,])
}
spbg <- sample_background(bg_df = bg_df2, sp = SPECIES) # Select subdataframe of random background species where targeted aquaculture species occur

plot(spbg$x, spbg$y, main = c("Background points for ", SPECIES)) # DOES NOT WORK (probably because of the NA problem and selection of the countries)

# Logically, supposed to be in the same area than the species of interest because is a sample of background
# species in the area of distribution of the species of interest

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
aq_df_sp$PA <- 1 

# Merge the species dataframes (presence/absence df)
spPA <- rbind(aq_df_sp, spbg)

# Visualize (check where are the dots compared to the species of interest)
ggplot(data = world) +
  geom_sf(color = "black") +
  geom_sf(fill = "antiquewhite1") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'light blue'))+
  # coord_sf(xlim = c(ext(india)[1], ext(india)[2]), ylim = c(ext(india)[3], ext(india)[4]), expand = FALSE) +
  geom_point(data = spPA, aes(x = x, y = y, fill = factor(PA)), size = 3,  # Utilisez factor(PA) pour traiter PA comme une catégorie
             shape = 21) +
  scale_fill_manual( name = "Presence/Absence", values = c("0" = "red", "1" = "green")) + 
  ggtitle(label = c("Species background location points in the area of distribution of the target species")) +
  theme(legend.title = element_text(size = 9), title = element_text(size = 10))




### MERGE ALL ###
dat <- GetCombinedDf(final = env_mg, sp = spPA, base = BASE) # Add country 
# We add the species data 
# Only 1 data for 


# Plus l'impression que tout ca soit utile
# Generate per species the equation
# get_species_df <- function(speciesname, unique_sp) {
#   # Sélectionner le subset pour l'espèce donnée dans aquaspecies_df
#   sub_sp <- aquaspecies_df %>%
#     tidyterra::filter(species == speciesname)
#   
#   # Sélectionner le subset pour les espèces associées dans unique_sp
#   species_list <- unique_sp[[speciesname]]
#   sub_sp_bg <- bg_df %>%
#     tidyterra::filter(species %in% species_list & species != speciesname)
#   
#   # Combinaison des deux subsets
#   df <- bind_rows(sub_sp, sub_sp_bg)
#   
#   # Supprimer les doublons
#   df <- distinct(df)
#   
#   return(df)
# }

# PAS TOTALEMENT CORRECT CAR APPEL DES ESPECES BACKGROUND PAR NOM AU LIEU DE LOCATION
# CE QUI VEUT DIRE APPEL DE TOUTES LES LOCALISATIONS OU LES ESPECES BACKGROUND SONT 
# ET NON JUSTE CELLES D'INTERET (pays de l'espèce ciblee)

# Test
# sp_comb_df <- get_species_df(SPECIES, unique_species) # OK

# Call this function in a loop for species dataframes


# Get environmental data for areas targeted
# get_crop_env <- function(species_names, count_per_sp){
#   for(i in seq_along(species_names)){
#     count <- count_per_sp[[species_names[i]]]
#   }
#   for(i in seq_along(count)){
#     name_reg <- paste0(i)
#     region <- world_vect[world_vect$name == name_reg, ]
#     reg_ext <- ext(region)
#     rast_ext <- list()
#     for (i in seq_along(list_raster)) {
#       rast_ext[[i]] <- crop(list_raster[[i]], reg_ext)
#     }
#   }
#   return(rast_ext)
# }
# 







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

sdm_obj2=sim_func(names_x1,name_y, dat) # get back the sdm object
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
