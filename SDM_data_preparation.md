# Preparation of the Data for SDM 


### Variables
- Verify data collinearity: threshold pairwise correlation coefficient value of |r| > .7
- Standardization of the data to a mean of 0 and SD of 1

### Data
- Chosing the right resolution

**Species data**
- Grid variables (linking environmental data and species data with the same projection):
    - Removing records without all the data of each variables
    - Removing cells with multiple occurrences
    - Standardized cleaning with ‘CoordinateCleaner’ package (*removed any records with equal or zero/zero coordinates, found in urban areas, near biodiversity institutions, outside of their listed country, or at the centroids of countries and its subdivisions*)
    - Selection of the data within the time period of interest
- Classification of species occurrences by species type (criteria depending on the contex of the study) and at the chosen point of view and scale (absolute distance, habitat) = ranges
- Remove points with not enough occurrences to avoid overfitting

**Background environment selection**
Fitting of the model with presence background data -> find an adapted method here. Allows to know if there is an overfitting or not. (Generalizable / transferable SDM) --> cf. paper when we are at this part
- ‘target-group background’ approach to select the background sites (?)

**Modelling Species Distribution**
Here, two modelling approaches, but depends on the context. --> Cf. paper

    Know what model to use before preparing the data??

--> see the rest later.


# References
Nguyen & Leung, 2022
