library(mgcv) # For the GAM
# Find isocodes per coordinates
library(countrycode)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(leaflet)
library(leaflet.extras)
library(plotly)

setwd("C:/Users/User/Desktop/Internship/Data")# Download and load data (!!! LOCAL ADRESS)
# Source Function
source("C:/Users/User/Documents/GitHub/AquaModel/Functions.R") # Allows to access the functions (!!! LOCAL ADRESS)

# SPECIES DATA
aquaspecies_df <- read_rds("aquaspecies_df.rds")
bg_df <- read_rds("background_data_clean.rds")

# ENV DATA 
pathtmin <- "C:/Users/User/Desktop/Internship/Data/Climate/tmin" # Manually put the data inside separate files (otherwise, same file)
path_tmin <- paste0(pathtmin,"/wc2.1_30s") # Make a loop in the future for the different files
raster_tmin <- list.files(path_tmin, pattern = "\\.tif$", full.names = TRUE)
tmin <- rast(raster_tmin)



# Créer un objet spatial à partir des coordonnées x et y
coordinates(aquaspecies_df) <- ~ x + y
proj4string(aquaspecies_df) <- CRS("+proj=longlat +datum=WGS84")
# Charger les frontières des pays à partir de Natural Earth
world <- ne_countries(scale = "medium", returnclass = "sf")
countries <- vect("C:/Users/User/Desktop/Internship/Data/Regions/world-administrative-boundaries/world-administrative-boundaries.shp")

# Density plot (without ne_countries function)
countries <- geodata::world(resolution = 5, level = 0, path = tempdir()) # Try to map with this
# Convert dataframe to spatial vector

points <- vect(data, geom = c("x", "y"), crs = crs(countries))
ext <- ext(countries)
res <- 0.5  # résolution du raster (vous pouvez ajuster selon vos besoins)

# Créer un raster vide avec l'extension et la résolution désirées
country_raster <- rast(ext, res = res)
t <- as.data.frame(countries, xy=TRUE)
# Rasteriser les pays, chaque cellule du raster reçoit la valeur de l'ID du pays
country_raster <- rasterize(countries, country_raster, field = "iso3")
points_raster <- rasterize(points, country_raster, fun = "length", background = 0)

density_by_country <- zonal(points_raster, country_raster, fun = sum)
colnames(density_by_country)[2] <- "density"







# Convertir df en sf object pour l'intersection
df_sf <- st_as_sf(aquaspecies_df)
# Faire l'intersection pour obtenir les noms des pays
df_with_country <- st_join(df_sf, world, join = st_intersects)
# Ajouter les codes ISO alpha-3 en utilisant countrycode
df_with_country$iso_a3 <- countrycode(df_with_country$name, "country.name", "iso3c")
# Voir le tableau final avec les coordonnées et les codes ISO alpha-3
head(df_with_country)
aquaspecies_df2 <- df_with_country %>%
  select("iso_a3")
aquaspecies_df2 <- cbind(aquaspecies_df,aquaspecies_df2) # FINAL DF
df_sf <- st_as_sf(aquaspecies_df, coords = c("x", "y"), crs = 4326)

# Regrouper par espece et par region
df_summary <- aquaspecies_df2 %>%
  group_by(x, y, iso_a3) %>%
  summarise(count = n())
# Convertir le résumé en objet spatial sf
df_summary_sf <- st_as_sf(df_summary, coords = c("x", "y"), crs = 4326)
# Observations par espece
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = df_sf, ~x, ~y, radius = 2,
                   color = ~colorFactor(topo.colors(10), aquaspecies_df$species)(species),
                   popup = ~paste("Species:", species)) %>%
  addLegend("bottomright", pal = colorFactor(topo.colors(10), aquaspecies_df$species),
            values = ~species, title = "Species",
            opacity = 1)
# Densité des observations
leaflet() %>%
  addTiles() %>%
  addHeatmap(data = df_summary_sf, ~x, ~y, intensity = ~count,
             blur = 20, max = 0.05, radius = 15) %>%
  addLegend("bottomright", pal = colorNumeric("viridis", df_summary$count),
            values = ~count, title = "Number of Observations",
            opacity = 1)


image <- ggplot() +
  geom_sf(data = world) +
  geom_sf(data = df_with_country, aes(fill = species)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    panel.grid = element_line(color = "white"),
    legend.position = "bottom",
    legend.title = element_text(size=15, hjust = -1, vjust = 1),
    legend.text = element_text(angle = 315, vjust = 0)
  ) 
print(image)




# Tests -----------------------------------

aquaspecies_df <- read_rds("aquaspecies_df.rds")

# Obtenir les données des frontières des pays
mapdata <- map_data("world") # 252 countries
world <- ne_countries(scale = "medium", returnclass = "sf")
# Convertir les coordonnées en objets sf
points_sf <- st_as_sf(aquaspecies_df, coords = c("x", "y"), crs = 4326)
# Déterminer le pays pour chaque point
joined <- st_join(points_sf, world, join = st_within)
# Extraire le nom du pays et ajouter à la table initiale
aquaspecies_df$region <- joined$name_long
head(aquaspecies_df)
length(which(is.na(aquaspecies_df$region))) # 5 uniquement
# aquaspecies_df <- aquaspecies_df %>% tidyterra::filter(!is.na(map_data$count))
map_data <- left_join(mapdata, aquaspecies_df, by= "region") # Gros dataset
gc()
head(map_data)
map_data <- map_data[,-7] # Delete count (why here ??)
# Compter le nombre de points par pays
counts_by_country <- map_data %>%
  group_by(region) %>%
  summarise(count = n())
head(counts_by_country)

# Recommencer la méthode avec counts_per_country
rm(points_sf,joined)
rm(map_data)
gc()
map_data <- left_join(mapdata, counts_by_country, by= "region")
length(which(is.na(counts_by_country$count))) # 0
# mapdata1 <- map_data %>% tidyterra::filter(!is.na(map_data$count)) (do if presence of NAs)
which(map_data$region == "Antartica") # No Antartica


# Start plotting
map1 <- ggplot(map_data, aes(x = long, y = lat, group = group)) +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() +
  coord_cartesian(ylim = c(-55, 85)) + # Delete Antartica (some rendering problems)
  geom_polygon(aes(fill = count), color = "black") +
  # Normal scale
  # scale_fill_gradient(name = "Number of samples", low = "yellow", high = "red", na.value = "grey50")+
  # Log scale
  scale_fill_gradientn(
    name = "Number of samples \n (log scale)",
    colors = c("yellow", "goldenrod1" ,"red"),
    trans = "log10",
    na.value = "grey50",
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank(),
        panel.background = element_rect(fill = "lightcyan"),
        panel.grid = element_line(color = "white"))
map1

ggsave(file="species_density_per_country.pdf", plot=map1, width=13, height=6)

# scale_fill_gradient(name = "Number of samples", low = "yellow", high = "red", na.value = "grey50")+
  