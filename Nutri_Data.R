# Libraries
# Data management
library(tidyr)
library(tidyverse)
library(dplyr)
library(readxl)
library(purrr)
# Data Visualization
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)  # Optionnel pour des résolutions plus élevées
library(terra)
library(rworldmap)
library(countrycode)
library(maps)
library(rnaturalearth) 
library(sf)
library(wbstats)
library(mapview)

# Plotting
library(ggplot2)
library(gggrid)
library(lattice)
library(leafpop)
library(mapview)
library(cowplot)
library(gridExtra)
# library(vapoRwave)
library(viridis)
library(RColorBrewer)
# library(tmap)
# library(tmaptools)
# library(rJava)
# library(OpenStreetMap)
library(leaflet)

# Mapping data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Get the nutritional data
setwd("C:/Users/User/Desktop/Internship/Data/Nutrition")

# Load data for global deficiencies
# beta_carotene <- read.csv("Beta-carotene_Combined.csv"? stringsAsFactors = TRUE) # Not used because obsolete
ferritin <- read.csv("Ferritin_Combined.csv", stringsAsFactors = TRUE)
haemoglobin <- read.csv("Haemoglobin_Combined.csv", stringsAsFactors = TRUE)
retinol <- read.csv("Retinol_Combined.csv", stringsAsFactors = TRUE)
zinc <- read.csv("Zinc_Combined.csv", stringsAsFactors = TRUE)


# MACRONUTRImacroS ----------------------------------------------------------

macro_df <- read.csv("macro_data.csv", stringsAsFactors = TRUE)
# Filter data and delete what's before 2000
macro_df_clean <- macro_df %>%
  tidyterra::filter(Year >= 2000) %>% # Chose nb of occurrences depending on the 
  ungroup()
rm(macro_df)
gc()
# Selectionner les donnees d'interet
Entities <- levels(macro_df_clean$Entity)
length(Entities) # 243
par(mfrow=c(1,1))
macro <- macro_df_clean %>% # Remove unused rows
  filter(!grepl("FAO|Union|income", Entity))
# Entities without an ISO3
# Africa|Asia|Europe|Melanesia|Micronesia|Netherlands Antilles|North America|
# Oceania|Polynesia|Serbia and Montenegro|South America|World 
# Change some details in the countries names
# macro <- macro %>%
#   mutate(Entity = gsub("Micronesia \\(country\\)", "Micronesia", Entity))
# macro$Entity <- factor(macro$Entity)
macro <- droplevels(macro)

# Map
x11()
macro$ISO3 <- countrycode(macro$Entity, "country.name", "iso3c") # Add ISO3 column
macro$ISO3 <- factor(macro$ISO3)
macro_map <- joinCountryData2Map(macro, joinCode = "ISO3", nameJoinColumn = "ISO3")
macro_map <- under_map[row.names(under_map) != 'Antarctica',]
mapCountryData(macro_map, nameColumnToPlot = "Share.of.the.daily.caloric.intake.that.comes.from.animal.protein", 
               catMethod = "pretty", missingCountryCol = gray(.8),
               colourPalette = "heat", addLegend = TRUE, borderCol = "gray39", 
               mapTitle = "", oceanCol = "lightcyan")

# Interactive map
x11()
mapview(macro_map,   zcol = "Share.of.the.daily.caloric.intake.that.comes.from.animal.protein", 
        col.regions = brewer.pal(5, "YlOrRd"), na.color = "grey")

# Tenter une carte + jolie
world_data <- merge(world, macro, by.x = "iso_a3", by.y = "ISO3", all.x = TRUE)
# Plot avec ggplot2
x11()
image <- ggplot(data = world_data) +
  geom_sf(aes(fill = Share.of.the.daily.caloric.intake.that.comes.from.animal.protein)) +
  scale_fill_viridis_c(option = "viridis", na.value = "lightgray") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    panel.grid = element_line(color = "white"),
    legend.position = "bottom"
  ) +
  labs(
    title = "Share of the daily caloric intake from animal protein",
    fill = "Percentage"
  )
ggsave(file="test.pdf", plot=image, width=10, height=8)



# Read data for fish composition
setwd("C:/Users/User/Desktop/Internship/Data/Nutrition")
df_species <- read.table("sp_names.csv", sep = ";", header = TRUE)
fish_compo <- read.table("fish_composition.csv", sep = ";", dec = ",", header = TRUE)
### Selection
## Micronutrimacros
# ZN(mg) = Zinc
# FE(mg) = Iron
# RETOL(mcg) = Retinol (vitamin A)
# CARTB(mcg) = Beta-carotene (Vitamin A), probably won't be used because not monitored nowadays in countries, or efficimacro for food...idk

## Macronutrimacros (where do I stop ?)
# ENERC(kJ) = energy kJ
# ENERC(kcal) = energy kcal (more accurate in my opinion)
# NT(g) = nitrogen
# PROTCNT(g) = total protein 
# FAT(g) = total fat
# CHOAVLDF(g) = available carbohydrates


# Cut after "FAUN(g)" (fatty acids non idmacroified) column 66
fish_compo_clean <- fish_compo[,-66:188]



# Define levels "low", "medium" and high
# Use mutate() function to create a new column with conditions


# Filter the dataset with only these componmacros


