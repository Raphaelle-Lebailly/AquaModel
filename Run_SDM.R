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
aquaspecies_df <- read_rds("aquaspecies_df.rds")
# Background species data 
bg <- read_rds("background_data.rds")

# Environmental data
tmin <- worldclim_global(var = "tmin", res = 0.5, path=tempdir())

# FUNCTIONS ---------------------------------------------------------------
### LOADED FROM SOURCE CODE FOR THE FUNCTIONS


# PROTOCOLE ---------------------------------------------------------------
# Test if every function is working well and giving generalizable results.


