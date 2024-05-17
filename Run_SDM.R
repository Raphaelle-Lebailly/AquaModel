# Run SDM

setwd("C:/Users/User/Desktop/Internship/Data") # Download and load data (!!! LOCAL ADRESS)

# Source Function
source("C:/Users/User/Documents/GitHub/AquaModel/Functions.R") # Allows to access the functions (!!! LOCAL ADRESS)
# source("C:/Users/User/Documents/GitHub/AquaModel/Data.R") # Allows to access the data (!!! LOCAL ADRESS)

# # PACKAGES ----------------------------------------------------------------
### LOADED FROM SOURCE CODE FOR THE FUNCTIONS

# DATA IMPORTATION --------------------------------------------------------
# Alpha countries data
data("d.countries") # Data with codes a2 and a3 to convert for flags
countcode <- d.countries %>%
  select(a2, a3)
# Species for SDM 
# aquaspecies_df <- read_rds("aquaspecies_df.rds")
# Background species data 
# bg <- read_rds("background_data.rds")

# Environmental data
# tmin <- worldclim_global(var = "tmin", res = 0.5, path=tempdir())

# FUNCTIONS ---------------------------------------------------------------
### LOADED FROM SOURCE CODE FOR THE FUNCTIONS


# PROTOCOLE ---------------------------------------------------------------
# Test if every function is working well and giving generalizable results.

# Clean the background data
# bg_clean <- GetSpDf(bg)
# warnings()
# saveRDS(bg_clean, "background_data_clean.rds")

# Species for background
# # Background data 
# setwd("C:/Users/User/Desktop/Internship/Data")
# distrifish1 <- readRDS("distrifish1.rds") 
# distrifish2 <- readRDS("distrifish2.rds")
# distrifish3 <- readRDS("distrifish3.rds")
# distrifish4 <- readRDS("distrifish4.rds")
distrifish5 <- readRDS("distrifish5.rds")
# 
# 
# # First element is the same as the last one from the previous batch (my bad)
# distrifish2 <- distrifish2[-1]
# distrifish3 <- distrifish3[-1]
# distrifish4 <- distrifish4[-1]
distrifish5 <- distrifish5[-1]


distrifish5_clean <- GetSpDf(distrifish5)
saveRDS(distrifish5_clean, "background_data_clean5.rds")
dim(distrifish5_clean)

#  rm(list = ls())
# gc()
# bg <- readRDS("background_data_clean1.rds")
# head(bg)
