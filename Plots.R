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



# Add the ISOCODE3 to map the aquaspecies data
# Créer un objet spatial à partir des coordonnées x et y
coordinates(aquaspecies_df) <- ~ x + y
proj4string(aquaspecies_df) <- CRS("+proj=longlat +datum=WGS84")
# Charger les frontières des pays à partir de Natural Earth
world <- ne_countries(scale = "medium", returnclass = "sf")
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
