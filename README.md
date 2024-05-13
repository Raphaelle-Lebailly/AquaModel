# 🐟 AquaModel: a model to link aquaculture and nutritional needs in the world 🐟

**Authors: Raphaëlle Lebailly (Master's student at Rennes University) & Brian Leung (Researcher and Associate Professor at McGill University)**

With this project, we aim to built a Decision Making System for Aquaculture practices. 
The main goal is to link nutritional issues (malnutrition or deficiencies) with species suitable for aquaculture and for the local environment. We would like to make it accessible for everyone, from scientists and researchers to potential aquaculture farmers, eventually. 

Aquaculture is one of the most dynamic and promising food production area (Naylor *et al.*, 2023), which makes it interesting to develop tools for to better master it. 
Futhermore, many species of fishes, crustaceans, molluscs and seaweed (from both seawater and freshwater) contain a lot of micronutrients that a vast majority of people living in the Global South lack.

To do so, we decided to combine a Species Distribution Model (SDM) of the used and potentionally used aquaculture species and the distribution of macronutrients and micronutrients needs. 

**Files:**
- **README.md**: Explains the goal of the project.
- **SDM_data_preparation.md**: Detailed explanations of the methodology.
- **aquafish.rds**: Raw GBIF data for the species used in aquaculture.
- **aquaspecies_df.rds**: Tidy species dataframe from **aquafish.rds**.
- **Test-SDM.r**: First test R file for the SDM.
 
