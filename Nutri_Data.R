library(tidyverse)

# Get the nutritional data
setwd("C:/Users/User/Desktop/Internship/Data/Nutrition")

# Load data for global deficiencies
# beta_carotene <- read.csv("Beta-carotene_Combined.csv") # Not used because obsolete
ferritin <- read.csv("Ferritin_Combined.csv")
haemoglobin <- read.csv("Haemoglobin_Combined.csv")
retinol <- read.csv("Retinol_Combined.csv")
zinc <- read.csv("Zinc_Combined.csv")


# MACRONUTRIENTS ----------------------------------------------------------

macro_df <- read.csv("macro_data.csv")

# Filtrer les datas par années (pas avant 2000's)
# Enlever les NAs
# micronutrients <- rbind(beta_carotene$Country, ferritin$Country,haemoglobin$Country, retinol$Country, zinc$Country)
# df2 <- c(beta_carotene$Country, ferritin$Country,haemoglobin$Country, retinol$Country, zinc$Country)
# colnames(micronutrients) <-c("beta_c", "ferritin", "haemog", "retinol", "zinc")

# Filter data and delete what's before 2000
macro_df_clean <- macro_df %>%
  tidyterra::filter(Year >= 2000) %>% # Chose nb of occurrences depending on the 
  ungroup()





# Read data for fish composition
setwd("C:/Users/User/Desktop/Internship/Data/Nutrition")
df_species <- read.table("sp_names.csv", sep = ";", header = TRUE)
fish_compo <- read.table("fish_composition.csv", sep = ";", dec = ",", header = TRUE)
### Selection
## Micronutrients
# ZN(mg) = Zinc
# FE(mg) = Iron
# RETOL(mcg) = Retinol (vitamin A)
# CARTB(mcg) = Beta-carotene (Vitamin A), probably won't be used because not monitored nowadays in countries, or efficient for food...idk

## Macronutrients (where do I stop ?)
# ENERC(kJ) = energy kJ
# ENERC(kcal) = energy kcal (more accurate in my opinion)
# NT(g) = nitrogen
# PROTCNT(g) = total protein 
# FAT(g) = total fat
# CHOAVLDF(g) = available carbohydrates


# Cut after "FAUN(g)" (fatty acids non identified) column 66
fish_compo_clean <- fish_compo[,-66:188]



# Define levels "low", "medium" and high
# Use mutate() function to create a new column with conditions


# Filter the dataset with only these components


