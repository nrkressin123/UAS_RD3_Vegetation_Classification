library(terra)
library(sf)
library(caret)

## load in data to compare
r <- rast("output_maps/RD3_VegClass_Combined.tif")
pts <- st_read("pt_data/Training_Points_FinalModel.shp")

# Compare in confusion matrix
pts$predicted <- extract(r, pts)[,2]

# Convert both to factors with same levels
pts$species <- factor(pts$species)
pts$predicted <- factor(pts$predicted, levels=levels(pts$species))

# Create confusion matrix
conf_mat <- table(Observed = pts$species, Predicted = pts$predicted)
print(conf_mat)

# Confusion matrix with statistics
confusionMatrix(conf_mat)

# Convert to data frame
conf_df <- as.data.frame.matrix(conf_mat)

# Save to CSV
write.csv(conf_df, "confusion_matrix_FinalPredicted.csv", row.names = TRUE)