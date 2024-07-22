# Goal: implement the background data of all the countries containing the species of interest
library(mgcv)

library(sp) # Spatial data manipulation (reverse geocoding)
library(rworldmap)
library(raster)

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
bg_df <- distinct(bg_df)
# Retrieve species with non NA aquaculture status
aquaspecies_df <- na.omit(bg_df)
# aquaspecies_df <- distinct(aquaspecies_df)
# plot(aquaspecies_df$x, aquaspecies_df$y) # Check data repartition
# We mostly have data in western and central Africa and eastern North America


### REDO THE GETFLAGS FUNCTION IN ORDER TO FILTRATE WHAT'S NOT ACCURATE
# Getflag()

# Import data borders and coastlines
world <- ne_countries(scale = "medium", returnclass = "sf") # Borders of the countries
world <- world[world$continent != "Antarctica", ] # Erase Antartica (issues with intersect function and useless)
world <-  world %>% # Sort by alphabetical order (except Åland which will be last, DO NOT DO sort(regions), not the same alphabetical order than arrange())
  arrange(by = name)
world_vect <- vect(world)
regions <- world$name
coastline <- ne_download(scale = "medium", type = "coastline", category = "physical", returnclass = "sf") # Get coastlines
coastline_vect <- vect(coastline) # Convert to spatvector object
intersected <- terra::intersect(world_vect, coastline_vect) # Get countries list which have coastlines
countries_with_coastline <- unique(terra::values(intersected)$name)
rm(intersected)

# Call the object 'regions' to have the countries names (spell them right)


###### Visualizing the points (aquaculture species occurrence in the targeted country) -----------------------------
# COUNTRY <- 'China'
# which(regions == COUNTRY) # Check if present in the list
# name_reg <- paste0(COUNTRY)
# region <- world_vect[world_vect$name == name_reg, ] # Get country geometry
# 
# # Get buffered targeted region
# # Draw polygon from coastline
# intersect <- terra::intersect(coastline_vect, region)
# # Crop coastline in targeted area
# crop <- crop(region, intersect)
# # Add a buffer everywhere following the border
# buffer <- buffer(crop, width = 22000) # Apply 22km buffer on all coasts
# # Combine Geometries
# combined <- combineGeoms(region, buffer) # Final geometry
# 
# # Intersect the points of interest
# lonlat <- cbind(aquaspecies_df$x, aquaspecies_df$y) # Get coordinates
# pts <- vect(lonlat, crs = "WGS 84") # As SpatVector
# inter_points <- terra::intersect(combined, pts) # Intersect points and buffer
# coords <- geom(inter_points) # Extract coordinates with points
# points_df <- data.frame(x = coords[, "x"], y = coords[, "y"]) # 
# 
# # Visualize the data in the world (plot) or in the targeted area
# ggplot(region) +
#   geom_sf(color = "black") +
#   geom_sf(fill = "antiquewhite1") +
#   theme_minimal() +
#   theme(panel.background = element_rect(fill = 'lightblue')) +
#   geom_point(data = points_df, aes(x = x, y = y), size = 1, shape = 21, fill = "blue") +
#   coord_sf(xlim = c(ext(region)[1], ext(region)[2]), ylim = c(ext(region)[3], ext(region)[4]), expand = FALSE) +
#   labs(title = paste0("Species location points in ", COUNTRY)) +
#   xlab("Longitude") + ylab("Latitude") +
#   theme(plot.title = element_text(hjust = 0.5))

# rm(lonlat, pts, coords, inter_points) ; gc()



# Data management ---------------------------------------------------------

# We want to prepare the environmental and species data here for the SDM.

#### WORLD ################################################################################################

# Countries Geometries

# Check for 1 country
# COUNTRY <- 'China'
# regions <- world$name
# regions <- sort(regions)
# which(regions == COUNTRY) # Check if present in the list
# name_reg <- paste0(COUNTRY)
# region <- world_vect[world_vect$name == name_reg, ] # Get country geometry
# plot(region)
# 
# intersect <- terra::intersect(coastline_vect, region)
# plot(intersect)
# # Crop coastline in targeted area
# crop <- crop(region, intersect)
# # plot(crop)
# # Add a buffer everywhere following the border
# buffer <- buffer(crop, width = 22000, singlesided = TRUE) # Apply 22km buffer on all coasts
# # plot(intersect, type = "l")
# # lines(buffer,  col = "red")
# 
# # Combine Geometries
# combined_ <- combineGeoms(region, buffer) # Final geometry
# # plot(combined_)
# # lines(region)


# For all countries
# buffered_regions <- readRDS("buffered_regions.rds")
buffered_regions <- list()

for(i in seq_along(regions)){
  # Isolate region geometry
  region <- world_vect[world_vect$name == regions[i], ]
  if(regions[i] %in% countries_with_coastline){
    # Draw polygon from coastline
    intersect <- terra::intersect(coastline_vect, region)
    # Crop coastline in targeted area
    crop <- terra::crop( region, intersect)
    # Add a buffer everywhere following the border
    buffer <- buffer(crop, width = 22000) # Apply 22km buffer on all coasts
    # Combine Geometries
    combined <- combineGeoms(region, buffer)
    # Aggregate everything that overlaps in the assembled coast and country SpatVector
    combined_polygons <- aggregate(combined, fun = "min", dissolve = TRUE)
    # Keep region attributes
    # region_attributes <- as.data.frame(region)
    # merged <-  merge(combined_polygons, region_attributes)
    # Add buffered object
    buffered_regions[i] <- combined_polygons
  
  } else {
    buffered_regions[i] <- region
  }
} # A bit long (import object) ; but when import object, session aborts, so keep the loop.

rm(intersect, crop, buffer) ; gc()

buffered_regions_vect <-  vect(buffered_regions)
# length(buffered_regions_vect) # 241, as intended

# plot(buffered_regions[[40]],  type = "l")
# plot(world_vect[world_vect$name == 'Canada', ])# plot(buffered_regions[[45]])
# legend(legend = "borders (red), buffer(black)")
# lines(world_vect[world_vect$name == 'China', ])

# Visualize the results
# img <-  ggplot(world_vect) +
#   geom_sf(color = "black") +
#   geom_sf(fill = "antiquewhite1") +
#   theme_minimal() +
#   theme(panel.background = element_rect(fill = 'lightblue'))
# img2 <-  ggplot(buffered_regions_vect) +
#   geom_sf(color = "black") +
#   geom_sf(fill = "antiquewhite1") +
#   theme_minimal() +
#   theme(panel.background = element_rect(fill = 'lightblue'))
# ggsave(filename = 'map_no_buffer.pdf', plot = img, width = 10, height = 4)
# ggsave(filename = 'map_buffer.pdf', plot = img2, width = 10, height = 4)



# Check if there are any intersections / overlays between the polygons

check_overlaps_relate <- function(vect) {
  n <- nrow(vect)
  overlaps <- matrix(FALSE, n, n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      overlaps[i,j] <- relate(vect[i, ], vect[j, ], relation = "overlaps")
    }
  }
  return(overlaps)
}

overlaps_matrix_relate <- check_overlaps_relate(buffered_regions_vect) # Check overlaps by pairs (241 x 241 countries), LONG
overlapping_pairs_relate <- which(overlaps_matrix_relate, arr.ind = TRUE) # Identify overlapping pairs in the matrix
# length(overlapping_pairs_relate)/2 # 183 overlaps

# dfnames <- overlapping_pairs_relate # Add country name to know where exactly it occurs (optional, just to have a visual)
# for(i in seq_along(overlapping_pairs_relate)){
#   ind <- overlapping_pairs_relate[i]
#   dfnames[i] <- regions[ind]
# }
# dfnames <- as.data.frame(dfnames) ; rm(ind)

# Do as so the first column wins the intersection region to delete the overlaps
overlapping_pairs_relate <- as.data.frame(overlapping_pairs_relate)
buffered_regions2 <- buffered_regions

for(i in seq_along(overlapping_pairs_relate$row)){
  ind1 <- overlapping_pairs_relate$row[i]
  ind2 <- overlapping_pairs_relate$col[i] # Get the index of row and col for overlaps (pair)
  
  overlap <- terra::intersect(buffered_regions2[[ind1]], buffered_regions2[[ind2]]) # Get intersection
  
  buffered_regions2[[ind1]] <- erase(buffered_regions2[[ind1]], overlap) # Erase the overlap only on the first region
}



den <- buffered_regions2[[57]] # Select the spatvector that is a problem (Denmark here)
den <- combineGeoms(den[1], den[2]) # Keep only 1 geometry instead of 2 (Combine the geometries)
buffered_regions2[[57]] <-  den # Replace with the new object in the regions object


buffered_regions_vect2 <- vect(buffered_regions2) # Create the SpatVector object
length(buffered_regions_vect2) # 241, ISSUE SOLVED
# plot(buffered_regions_vect)
# plot(buffered_regions_vect2)

rm(overlapping_pairs_relate, overlapping_pairs_relate2, overlaps_matrix_relate, overlaps_matrix_relate2, overlap) ; gc()
rm(buffered_regions, buffered_regions_vect)


# Add the attributes to the 241 countries (lost when changed the geometry)
reg_sf <- lapply(buffered_regions2, as_sf) # 241 sf objects

world2 <- world # WORKSSS
for(i in 1:dim(world2)[1]){
  world2$geometry[i] <- reg_sf[[i]]$geometry}

world_vect2 <- vect(world2) # NEW SPATVECTOR BORDERS 
# plot(world_vect2[world_vect2$name == "Brazil",])
# plot(world_vect[world_vect2$name == "Brazil",])


# Check for overlaps (see if it worked)
# overlaps_matrix_relate2 <- check_overlaps_relate(world_vect2)
# overlapping_pairs_relate2 <- which(overlaps_matrix_relate2, arr.ind = TRUE)
# length(overlapping_pairs_relate2) # 8 remains, but litterally impossible. Plus, empty geometry.
# dfnames2 <- overlapping_pairs_relate2 # Add country name to know where exactly it occurs
# for(i in seq_along(overlapping_pairs_relate2)){
#   ind <- overlapping_pairs_relate2[i]
#   dfnames2[i] <- regions[ind]
# }
# dfnames2 <- as.data.frame(dfnames2) ; rm(ind)


# Now, we have "world" containing the borders and "world2" containing the buffered coastal borders + continental borders


### SPECIES DATA ################################################################################################

# Test map
# Current geometry from rnaturalearth package but multiple issues to map the data
# See if there are pb also with cropping per country the data (missing data that are not inlands)
# world2 <- geodata::world(resolution = 5, level = 0, path = tempdir()) # Try to map with this
# plot(world2)


## REVERSE GEOLOCATION 
points <- data_frame('x' = bg_df$x, 'y' = bg_df$y) # Dataframe with all coordinates from bg_df

coords2country <- function(points, spatial_ref){  
  countries_geom <- as(spatial_ref, "Spatial") # Use my altered object to draw new borders
  # countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countries_geom)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countries_geom)
  
  # return the ADMIN names of each country
  return(indices$name) 
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

bg_df$country <- coords2country(points, world2)

diffe <- coords2country()



# Select subset of the targeted country for aquaculture species
OCC <- 5 # Set threshold number of minimal occurrences 
aq_df_occ <- aq_df2 %>% # Get species occurrences for all countries (> threshold)
  group_by(species) %>%
  tidyterra::filter(n() >= OCC) %>% 
  ungroup()
rm(aq_df2) ; gc()
# occurences2 <- table(aq_df_occ$species) # Check if threshold respected
# print(occurences2) # OK, 147 species
# length(occurences2)

list_species <- unique(aq_df_occ$species)


#######################################*
#######################################*
# GET NUMBER OF SPECIES THAT OCCUR >5, 10, 20 TIMES IN THE GRID

# Replace countries names that don't match with "world" data (for future function use)
which(regions %in% countries_filtered )

nb_occ <-  list()
for(i in seq_along(list_species)){ 
  ## SPECIES DATA
  aq_df_sp <- aq_df_occ %>% # Get species of interest's GBIF data
    tidyterra::filter(species == list_species[1])
  spbg <- sample_background(bg_df = bg_df2, sp = list_species[1]) # Get sample GBIF data acording to the species' country location
  aq_df_sp$PA <- 1 
  spbg$PA <- 0 # Add presence absence column
  spPA <- rbind(aq_df_sp, spbg) # Bind both
  
  ## ENV DATA
  countries_filtered <- unique(bg_df2$country[bg_df2$species == list_species[1]])
  env_crop <- lapply(X = countries_filtered[1], FUN = function(X){ # Crop to list of countries where species occurr
    GetCroppedRaster(list_raster = env_var, extent = X) 
  })
  env_crop <- unlist(env_crop) ; gc()
  # rm(NO3, PO4, SI, bathy, surftemp, prim_prod) ; gc() # Remove unused raster
  # rm(env_var) ; gc()
  env_rs <- list()
  for (j in seq_along(env_crop)) { # Resample
    env_rs[[j]] <- resample(env_crop[[i]], BASE, "bilinear") # OK
  }
  rm(env_crop) ; gc()
  env_df <- list()
  for (i in seq_along(env_rs)) { # Convert into dataframe
    env_df[[i]] <- as.data.frame(env_rs[[i]], xy = TRUE) # Very long.... OPTIMIZE
  }
  rm(env_rs) ; gc()
  count_nb <- length(unique(spPA$country)) # # Merge (+ cell ID)
  
  
  
}


#######################################*
#######################################*






# Target 1 species
SPECIES <-  "Carassius auratus" # (Goldfish, freshwater fish = inland points) 'commercial' for aquaculture but does not say if it's for human consumption
# Here example of species present in China (we don't know where else it's present)
aq_df_sp <- aq_df_occ %>% 
  tidyterra::filter(species == SPECIES) # Filter only the species of interest


# Visualize the points of the targeted species in the region of interest
region <- world_vect[world_vect$name == name_reg, ]
reg <- ext(region)
ggplot(data = world) +
  geom_sf(color = "black") +
  geom_sf(fill = "antiquewhite1") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'light blue'))+
  coord_sf(xlim = c(reg[1], reg[2]), ylim = c(reg[3], reg[4]), expand = FALSE) +
  geom_point(data = aq_df_sp, aes(x = x, y = y), size = 2,  # Put the dots from the species filtered subdataframe
             shape = 21, fill = "green") 


# In the world (All)
# ggplot(data = world) +
#   geom_sf(color = "black") +
#   geom_sf(fill = "antiquewhite1") +
#   theme_minimal() +
#   theme(panel.background = element_rect(fill = 'light blue'))+
#   # coord_sf(xlim = c(ext(india)[1], ext(india)[2]), ylim = c(ext(india)[3], ext(india)[4]), expand = FALSE) +
#   geom_point(data = aq_df_occ, aes(x = x, y = y), size = 1,  # Put the dots from the species filtered subdataframe
#              shape = 21, fill = "red") +
#   ggtitle(label = c("Species location points in the world (occurrence > 5)"))


# Get background species according to targeted species
sample_background <- function(bg_df, sp){ # Modify conditions about including NAs countries. Add the buffer. 
  
  # Extract countries where targeted species occur
  countries_filtered <- unique(bg_df$country[bg_df$species == sp])
  
  # Filter SP background
  df.temp <- bg_df2[bg_df2$species != SPECIES & bg_df2$country %in% countries_filtered,] 
  if(length(df.temp$species) > 10000){
    row.temp <- sample(x = 1:nrow(df.temp), size = 10000, replace = F)  # random coordinates sample, size 10,000
    sub_df = df.temp[row.temp,]
  } else {
    sub_df <- df.temp
  } 
  return(sub_df)
}

spbg <- sample_background(bg_df = bg_df2, sp = SPECIES) # Select subdataframe of random background species where targeted aquaculture species occur
# Here, only 4054 data but checked 



# Visualize background points
ggplot(data = world) +
  geom_sf(color = "black") +
  geom_sf(fill = "antiquewhite1") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'light blue'))+
  # coord_sf(xlim = c(ext(india)[1], ext(india)[2]), ylim = c(ext(india)[3], ext(india)[4]), expand = FALSE) +
  geom_point(data = spbg, aes(x = x, y = y), size = 1,  # Put the dots from the species filtered subdataframe
             shape = 21, fill = "red") +
  ggtitle(label = paste0("Background points for ", SPECIES))

# Prepare data
spbg$PA <- 0 # Add presence absence column
aq_df_sp$PA <- 1 

# Merge the species dataframes (presence/absence df)
spPA <- rbind(aq_df_sp, spbg)

##########################*
##########################*
spPA2 <- distinct(spPA) # Delete redundant rows
length(spPA2$species)
##########################*
##########################*


rm(spbg, aq_df_sp, aq_df_occ, aquaspecies_df, bg_df, bg_df2, coastline, coastline_vect) ; gc()

# Visualize (check where are the dots compared to the species of interest)
# [1:48,]
ggplot(data = world) +
  geom_sf(color = "black") +
  geom_sf(fill = "antiquewhite1") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'light blue'))+
  geom_point(data = spPA[1:48,], aes(x = x, y = y, fill = factor(PA)), size = 2,  # Utilisez factor(PA) pour traiter PA comme une catégorie
             shape = 21) +
  scale_fill_manual( name = "Presence/Absence", values = c("0" = "red", "1" = "green")) + 
  ggtitle(label = paste0("Species background location points in the area of distribution of ", SPECIES)) +
  theme(legend.title = element_text(size = 9), title = element_text(size = 10))

# Pb dots overlay, not that much of a concern, only bad rendering

#### ENVIRONMENTAL DATA ################################################################################################

# Get Countries where species occurr
countries_filtered <- unique(bg_df2$country[bg_df2$species == SPECIES])

# Crop the raster to the extent wanted (all countries targeted)
env_crop <- lapply(X = countries_filtered, FUN = function(X){
  GetCroppedRaster(list_raster = env_var, extent = X) # "Empty geometry" line if country misspelled
})

env_crop <- unlist(env_crop) ; gc()

# env_crop <- GetCroppedRaster(list_raster = env_var, extent = COUNTRY) # "Empty geometry" line if country misspelled
rm(NO3, PO4, SI, bathy, surftemp, prim_prod) ; gc() # Remove unused raster
rm(env_var) ; gc()
# plot(env_crop[[20]]) # Check

# Resample: adapt all raster geometry (resolution) to one reference raster
env_rs <- list()
for (i in seq_along(env_crop)) {
  env_rs[[i]] <- resample(env_crop[[i]], BASE, "bilinear") # OK
}
rm(env_crop) ; gc()


# Convert into dataframe
env_df <- list()
for (i in seq_along(env_rs)) {
  env_df[[i]] <- as.data.frame(env_rs[[i]], xy = TRUE) # Very long.... OPTIMIZE
}
rm(env_rs) ; gc()


# # Merge (+ cell ID)
count_nb <- length(unique(spPA$country))
df_list <- env_df
group_size <- length(env_df) / count_nb

# Merge (+ cell ID)
GetMerged <- function(df_list, group_size) {
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
  # final_merged <- reduce(merged_list, ~ full_join(.x, .y, by = c("x", "y")))
  # final_merged <- reduce(merged_list, rbind)
  
  return(merged_list)
}

env_mg <- GetMerged(df_list = env_df, group_size = length(env_df)/count_nb)


### MERGE ALL ###
# dat <- GetCombinedDf(final = env_mg, sp = spPA, base = BASE) # Add environmental data (does not work)

final <- env_mg[[1]]
sp <- spPA2
base <- env_var[[1]]
plot(base)

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
  
  index <- tapply(1:length(s_env), s_env, function(x) {return(x)}) # Create named array 
  rn <- index[as.character(s_sp[p])] # Select in index the ID of the GBIF location
  
  # Get final dataframe
  final$species[rn] <- sp$species[p] # Add species column
  final$PA[rn] <- sp$PA[p] # Add presence/absence column
  # length(which(!is.na(final$PA)))
  
  # Add country column, ensuring NA values are retained
  final$country[rn] <- sp$country[p]
  
  # Add aquaculture status column, ensuring NA values are retained
  final$Aquaculture_status[rn] <- sp$Aquaculture_status[p]
  
  return(final)
}

# f2 <- final[rn,]
# f2$species <- sp$species[p]
# f2$PA <- sp$PA[p]
# f2$country <- sp$country[p]
# f2$Aquaculture_status <- sp$Aquaculture_status[p]


## FOR CHINA
s <- spPA2[spPA2$country == countries_filtered[1],] # All species data for China
s2 <- s[s$PA ==1,] # Only presence data


t <- env_mg[[1]] # Environmental dataframe for China
# t2 <- rast(t)
# plot(t2[[25]]) # Check if it works

d <- GetCombinedDf(final = t, sp = s2, base = base) # Combine env + sp dataframes for China
length(which(d$PA == 1)) # See if it worked



### FOR ALL COUNTRIES (complete later when issue with multiple species solved)
# d2 <- GetCombinedDf(final = final, sp = s, base = base) 
# 
# dat <- list()
# for(i in seq_along(env_mg)){
#   dat[[i]] <- GetCombinedDf(final = env_mg[[i]], sp = spPA, base = base) # Add environmental data
# }
# 
# dat2 <- reduce(dat, rbind) # Get one final dataframe








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




# Species Distribution Model ---------------------------------------------------------------------


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
