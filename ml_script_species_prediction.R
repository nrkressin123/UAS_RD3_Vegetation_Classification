# Helper packages
library(dplyr)    # for data wrangling
library(ggplot2)  # for awesome graphics
library (sf) # for spatial data

# Modeling packages
library(ranger)   # a c++ implementation of random forest 
library(h2o)      # a java-based implementation of random forest
library(rsample) # to sample
library(caret) # for resampling
library(smotefamily) # for sampling distribution
library(terra)

## load in predicted map
r <- rast("output_maps/predict_majorityfilter_cover_noGrass.tif")

## 5 classifications: "built", "farm", "shrub", "tree", and "water"
tree_mask <- r == "tree"
#built_mask <- r == "built"
farm_mask <- r == "farm"
shrub_mask <- r == "shrub"
#water_mask <- r == "water"

## load in train data
data <- st_read("pt_data/train_pts_species_08072025.shp")
names <- c('species', 'cover','blue','green','red','rededge','nir','CTVI','DVI','GEMI','GNDVI','KNDVI','MCARI','MSAVI','MSAVI2','NDVI','NDWI','NRVI','RVI','SAVI','SR','TTVI','TVI','WDVI','NGRDI','NFRE','GRVI','NGWI','RG','LogR','LogRE','GLCM','Entropy','Morph', 'glcm_constrast', 'glcm_dissimilarity', 'glcm_homogeneity', 'glcm_ASM', 'glcm_entropy', 'mean', 'glcm_variance', 'glcm_correlation', 'CIRE', 'CHM', 'geometry')
names(data) <- names

# df processing
df <- data
df$cover <- as.factor(df$cover)
df_no_geom <- st_drop_geometry(df)

## separate into cover type we wish to predict
data <- df_no_geom[df_no_geom$cover == "tree", ]
data$species <- as.factor(data$species)
data$cover <- NULL
data <- df_no_geom[df_no_geom$cover == "shrub", ]
data$species <- as.factor(data$species)
data$cover <- NULL
data <- df_no_geom[df_no_geom$cover == "farm", ]
data$species <- as.factor(data$species)
data$cover <- NULL

## split into train/test
set.seed(111)
inTrain <- createDataPartition(data$species, p=0.8, list=FALSE)
train <- data[inTrain,]
test <- data[-inTrain,]

# rf model
rf <- ranger(
  species ~ ., 
  data = train,
  num.trees = 500,
  mtry = floor(42/3),
  replace=TRUE,
  respect.unordered.factors = "order",
  seed = 111,
  num.threads = parallel::detectCores(),
  importance = "impurity"
)

# predict
rf_pred  <- predict(rf, data = test)$predictions

# 2. Actual
actual <- test$species

# confusion matrix
confusionMatrix(rf_pred, actual)

# feature importance (gini)
importances <- rf$variable.importance
importances <- sort(importances, decreasing = TRUE)
head(importances)
top10_vars <- names(importances)[1:10]

# load raster for predictions
rstack <- rast("img_data/composite_final2.tif") 
terraOptions(threads = parallel::detectCores())

# predict on these 10 bands, 10 because they explain most of the variability 
## trees
#bands <- c("CHM", "GLCM", "RG", "blue", "CIRE", "NGRDI","NFRE","Entropy","MCARI","nir") 
## shrub
#bands <- c("CHM","GLCM","RG","Entropy","Morph","KNDVI","blue","LogR","NFRE","red") 
## farm
bands <- c("blue","red","NGWI","NDWI","green","LogR","LogRE","rededge","GLCM","glcm_variance") 
r_sub <- subset(rstack, bands)

# subset model
sel_vars <- c("species", bands)
train_subset <- select(train_farm, all_of(sel_vars))
test_subset <- select(test_farm, all_of(sel_vars))
names(test_subset) <- c("species", "blue","red","NGWI","NDWI","green","LogR","LogRE","rededge","GLCM","glcm_variance")

#rf subset
rf_subset <- ranger(
  species ~ ., 
  data = train_subset,
  num.trees = 500,
  mtry = floor(10/3),
  replace=TRUE,
  respect.unordered.factors = "order",
  seed = 111,
  num.threads = parallel::detectCores(),
)

# prediction subset function
predict_fun <- function(model, data, ...) {
  predict(model, data = data, type = "response")$predictions
}

  # make predicted map with R terra
  predicted_map <- predict(
    object = r_sub,
    model = rf_subset,
    fun = predict_fun,
    filename = "output_maps/rf_predict_species_Farm.tif",  # write to disk to save memory
    overwrite = TRUE,
    na.rm = TRUE,
  )

output <- rast("output_maps/rf_predict_species_Farm.tif")
output <- rast("output_maps/rf_predict_species_Farm.tif")
output[!farm_mask] <- NA

water_mask[!water_mask] <- NA
built_mask[!built_mask] <- NA
writeRaster(water_mask, "output_maps/rf_waterMask.tif")

test <- rast("output_maps/rf_predict_farmMask.tif")
