# Preparation of the Data for SDM 


### Variables
- Verify data collinearity: threshold pairwise correlation coefficient value of |r| > .7
- Standardization of the data to a mean of 0 and SD of 1

### 1. Environmental data
- Chosing the right resolution


#### 1.1 Resample

The different layers need to have the same geometry to be projected in the same grid. Therefore, the 'resample' function, of the package 'terra' is used in order to adapt one SpatRaster's geometry to another's. 

```{r}
resample(layer_raw, layer_base, "method")
```
The method will depend on the nature of the variable itself. 
- Nearest neighbor: **Discrete**
- Majority: **Discrete**
- Bilinear: **Continuous**
- Cubic: **Continuous**

        layer.rsp


**Snap to grid**

Snap to grid is useful to adapt every layer to the same skeleton scaffold. 
Other method if resample() is not used.

```{r}
SnapToGrid <- function(layer){
  df <- as.data.frame(layer,xy=T) # Use the xy dataframe and append the (x,y) values of each cell + index value
  # Resolution
  Dim <- dim(layer) 
  ResX <- Dim[1] # Resolution for x
  ResY <- Dim[2] # Resolution for y 
  DimLayer <- list(ResX, ResY)
  # Extent
  ext <- ext(layer) # Extent for data (terra object)
  xmin <- ext$xmin
  ymin <- ext$ymin
  xmax <- ext$xmax
  ymax <- ext$ymax
  ext.layer <- list(xmin, ymin, xmax, ymax)
  #Delta
  deltaX <- (xmax - xmin)/ResX
  deltaY <- (ymax - ymin)/ResY 
  # Create grid
  df$snapX <- as.integer(((df$x-xmin)/deltaX) + 0.5)
  df$snapY <- as.integer(((df$y-ymin)/deltaY) + 0.5)
  len <- dim(df)
  df$index <- seq(1,len[1])
  # Return
  return(df)
  # list(df, DimLayer, ext.layer)
}
```

        layer.stg (or final.df)


#### 1.2 Mean value

The a fine time scale isn't relevant in our case. The values per month are not interesting, so we compute the mean and replace the raw data by a mean value column into the new dataset. 

```{r}
mean.df <- function(layer, arg){
  name.col <- paste0("mean_", arg)
  # Calculate mean value per row 
  sub.df.mean1 <-  layer %>%
    mutate(name.col =  rowMeans(dplyr::select(., contains(arg)), na.rm = FALSE)) 
  # Subset without raw data
  sub.df.mean2 <- sub.df.mean1 %>%
    dplyr::select(., -contains(arg))
  # Rename column
  names(sub.df.mean2)[names(sub.df.mean2) == 'name.col'] <- toString(name.col)
  # Final df
  return(sub.df.mean2)
}
```
        layer.mn

#### 1.3 Merge dataframes

When the geometry is right for every layer and the mean value is calculated, we want to merge the datasets in order to have all variables in the same space.

```{r}
new.df <- df1 %>%
  inner_joint(df2)
```
*use the dplyr package*. The common columns are x and y.
No matter if x and y are different, anyways will be better organized thanks to snap to grid function.

        layer.mg


#### 1.4 Plot the layers

To check if it works, we need to have a visual representation of the final dataframe. 

**Remark: downloading the data per country for the borders takes a lot of time, maybe try for all of them at once?**

### 2. Species data

#### 2.1 Clean the data
Cleaning the data allows to have a dataset without irrelevant values. 
For that, we use the package 'CoordinateCleaner'. 




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
