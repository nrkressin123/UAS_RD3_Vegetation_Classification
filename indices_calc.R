library(terra)
library(RStoolbox)
library(raster)

## load map
map <- rast("img_data/composite_extraTexture.tif")

## mask
map[map == 0] <- NA
ms_masked <- mask(map, map)
writeRaster(ms_masked, "img_data/masked_multispectral.tif", overwrite=TRUE)

## test if worked
mask <- rast("img_data/masked_multispectral.tif")

## calculate indices
names(mask) <- c("blue", "green", "red", "rededge", "nir")
composite <- spectralIndices(mask, blue= "blue", green = "green", red = "red", nir = "nir", redEdge1 = "rededge")

mask_comp <- c(mask, composite)
# write to disk
options = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES")
writeRaster(mask_comp, "img_data/compressed_ms_comp.tif", filetype="GTiff", gdal=options)

# Function to rescale a single layer
rescale_to_uint16 <- function(x) {
  vmin <- min(x[], na.rm=TRUE)
  vmax <- max(x[], na.rm=TRUE)
  scaled <- (x - vmin) / (vmax - vmin) * 65535
  scaled <- round(scaled)
  clamp(scaled, lower=0, upper=65535)
}

# Apply to all bands (layers)
bands <- nlyr(rast)
scaled_list <- lapply(1:bands, function(i) rescale_to_uint16(rast[[i]]))
scaled_raster <- rast(scaled_list)

# Write the output as 16-bit unsigned
writeRaster(scaled_raster, "img_data/rescaled_uint16.tif", 
            datatype="INT2U", gdal=options,overwrite=TRUE)

rescale <- rast("img_data/rescaled_uint16.tif")


## indice calculations
red = rescale[["red"]]
green = rescale[["green"]]
blue = rescale[['blue']]
nir = rescale[['nir']]
rededge = rescale[['rededge']]

NGRDI = (green - red)/ (green + red)
NFRE = (nir - rededge)/(nir + rededge)
CIRE = (nir/rededge) - 1
GRVI = (nir/green)
NGWI = (green - nir) / (green+nir)
LogR = log(red)
LogRE = log(rededge)
RG = 2*red-green

names(NGRDI) = "NGRDI"
names(NFRE) = "NFRE"
names(CIRE) = "CIRE"
names(GRVI) = "GRVI"
names(NGWI) = "NGWI"
names(LogR) = "LogR"
names(LogRE) = "LogRE"
names(RG) = "RG"

stack = c(NGRDI, NFRE, CIRE, GRVI, NGWI, LogR, LogRE, RG)

# Apply to all bands (layers)
bands1 <- nlyr(stack)
scaled_list1 <- lapply(1:bands1, function(i) rescale_to_uint16(stack[[i]]))
calc_indices <- rast(scaled_list1)

## CIRE
CIRE_clipped <- clamp(CIre, lower = -1, upper = 1, values=TRUE)
