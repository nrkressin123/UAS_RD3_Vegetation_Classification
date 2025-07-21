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

# Read shapefile and change to dataframe
data <- st_read("cover_data/train_pts_cover_noGrass.shp")

# change names of data
names <- c('cover','blue','green','red','rededge','nir','CTVI','DVI','GEMI','GNDVI','KDNVI','MCARI','MSAVI','MSAVI2','NDVI','NDWI','NRVI','RVI','SAVI','SR','TTVI','TVI','WDVI','NGRDI','NFRE','GRVI','NGWI','RG','LogR','LogRE','GLCM','Entropy','Morph', 'glcm_constrast', 'glcm_dissimilarity', 'glcm_homogeneity', 'glcm_ASM', 'glcm_entropy', 'mean', 'glcm_variance', 'glcm_correlation', 'CIRE', 'CHM', 'geometry')
names(data) <- names

# df processing
df <- data
df$cover <- as.factor(df$cover)
df_no_geom <- st_drop_geometry(df)

# train/test
set.seed(111)
inTrain <- createDataPartition(df_no_geom$cover, p=0.8, list=FALSE)
train <- df_no_geom[inTrain,]
test  <- df_no_geom[-inTrain,]

# rf model
rf_cover_noGrass <- ranger(
  cover ~ ., 
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
rf_pred <- predict(rf_cover_noGrass, data = test)$predictions

# 2. Actual
actual <- test$cover

# confusion matrix
confusionMatrix(rf_pred, actual)

# feature importance (gini)
importances <- rf_cover_noGrass$variable.importance
importances <- sort(importances, decreasing = TRUE)
head(importances)
top19_vars <- names(importances)[1:19]

# load raster for predictions
rstack <- rast("img_data/composite_final2.tif") 
terraOptions(threads = parallel::detectCores())

#predict on these 19 bands, 19 because they explain most of the variability 
bands <- c("CHM", "GLCM", "Entropy", "blue", "NGWI", "NDWI","MCARI","NGRDI","green","GNDVI","GRVI","RG","NFRE","CIRE", "red", "LogR", "nir", "rededge", "LogRE")
r_sub <- subset(rstack, bands)

# subset model
sel_vars <- c("cover", bands)
train_subset <- select(train, all_of(sel_vars))
test_subset <- select(test, all_of(sel_vars))
names(test_subset) <- c("cover", "CHM", "GLCM", "Entropy", "blue", "NGWI", "NDWI","MCARI","NGRDI","green","GNDVI","GRVI","RG","NFRE","CIRE", "red", "LogR", "nir", "rededge", "LogRE")

#rf subset
rf_subset <- ranger(
  cover ~ ., 
  data = train_subset,
  num.trees = 500,
  mtry = floor(19/3),
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
  filename = "output_maps/rf_predict_cover_noGrass.tif",  # write to disk to save memory
  overwrite = TRUE,
  na.rm = TRUE,
)

# predict
rf_test_subset <- predict(rf_subset, data = test_subset)$predictions

# 2. Actual
actual_testsub <- test_subset$cover

# confusion matrix
confusionMatrix(rf_test_subset, actual_testsub)

# load prediction map
predict <- rast("output_maps/rf_predict_cover_noGrass.tif")

# majority filtering
# Apply 3x3 majority filter
predict_majorityfilter <- focal(predict, w = matrix(1,3,3), fun = 'modal', na.rm='omit')

plot(predict_majorityfilter)

# output
options = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES")
writeRaster(predict_majorityfilter, "output_maps/predict_majorityfilter_cover_noGrass.tif", gdal=options,overwrite=TRUE)
