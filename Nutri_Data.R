# Get the nutritional data
setwd("C:/Users/User/Desktop/Internship/Data/Nutrition")

# Load data
beta_carotene <- read.csv("Beta-carotene_Combined.csv")
ferritin <- read.csv("Ferritin_Combined.csv")
haemoglobin <- read.csv("Haemoglobin_Combined.csv")
retinol <- read.csv("Retinol_Combined.csv")
zinc <- read.csv("Zinc_Combined.csv")

# Filtrer les datas par années (pas avant 2000's)
# Enlever les NAs
View(beta_carotene)

micronutrients <- rbind(beta_carotene$Country, ferritin$Country,haemoglobin$Country, retinol$Country, zinc$Country)
df2 <- c(beta_carotene$Country, ferritin$Country,haemoglobin$Country, retinol$Country, zinc$Country)
colnames(micronutrients) <-c("beta_c", "ferritin", "haemog", "retinol", "zinc")


length(unique(micronutrients))

setwd("C:/Users/User/Desktop/Internship/Data")
df <- read.table("sp_names.csv", sep = ";", header = TRUE)
