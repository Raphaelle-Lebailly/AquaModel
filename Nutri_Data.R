# Get the nutritional data
setwd("C:/Users/User/Desktop/Internship/Data/Nutrition")

# Load data
beta_carotene <- read.csv("Beta-carotene_Combined.csv")
ferritin <- read.csv("Ferritin_Combined.csv")
haemoglobin <- read.csv("Haemoglobin_Combined.csv")
retinol <- read.csv("Retinol_Combined.csv")
zinc <- read.csv("Zinc_Combined.csv")

# Filtrer les datas par années (pas avant 2000"s)
# Enlever les NAs