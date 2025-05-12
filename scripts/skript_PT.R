# skrtipt produktivita

library(terra)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

###################################################################
# VMB load

R2 <- vect("data/VMB/VMB_R2.shp")
new_cols <- data.frame(
  BIOTOP_CODES = as.factor(gsub(" \\(\\d+\\)", "", R2$BIOTOP_SEZ)),
  HABIT_CODES = as.factor(gsub(" \\(\\d+\\)", "", R2$HABIT_SEZ))
)
R2 <- cbind(R2, new_cols)
R2 <- R2[, c("SEGMENT_ID", "FSB", "BIOTOP_CODES", "HABIT_CODES", "SHAPE_Area", "DATUM")]

X <- vect("data/VMB/VMB_X1_X5_X7AB.shp")
new_cols <- data.frame(
  BIOTOP_CODES = as.factor(gsub(" \\(\\d+\\)", "", X$BIOTOP_SEZ)),
  HABIT_CODES = as.factor(gsub(" \\(\\d+\\)", "", X$HABIT_SEZ))
)
X <- cbind(X, new_cols); rm(new_cols)
X <- X[, c("SEGMENT_ID", "FSB", "BIOTOP_CODES", "HABIT_CODES", "SHAPE_Area", "DATUM")]

###################################################################
# productivity rasters load

GP_LT_NDVI95 <- rast("data/_Boletice_CZ0314123/Productivity/GLTP/CZ0314123_19940101_20211231_GP_GLTP_NDVI95.tif")
GP_LT_NDVIampl <- rast("data/_Boletice_CZ0314123/Productivity/GLTP/CZ0314123_19940101_20211231_GP_GLTP_NDVIampl.tif")
GP_ST_TPROD <- rast("data/_Boletice_CZ0314123/Productivity/GSTP/CZ0314123_20170101_20221231_GP_GSTP_TPROD.tif")
GP_ST_SPROD <- rast("data/_Boletice_CZ0314123/Productivity/GSTP/CZ0314123_20170101_20221231_GP_GSTP_SPROD.tif")

###################################################################
# CRS

refCRS <- crs(GP_LT_NDVI95)
R2 <- project(R2, refCRS)
X <- project(X, refCRS)
GP_LT_NDVI95 <- project(GP_LT_NDVI95, refCRS)
GP_LT_NDVIampl <- project(GP_LT_NDVIampl, refCRS)
GP_ST_SPROD <- project(GP_ST_SPROD, refCRS)
GP_ST_TPROD <- project(GP_ST_TPROD, refCRS)

writeVector(R2, "data/GP/processing/R2.shp", overwrite = T)
writeVector(X, "data/GP/processing/X.shp", overwrite = T)
writeRaster(GP_LT_NDVI95, "data/GP/processing/GP_LT_NDVI95.tif", overwrite = T)
writeRaster(GP_LT_NDVIampl, "data/GP/processing/GP_LT_NDVIampl.tif", overwrite = T)
writeRaster(GP_ST_SPROD, "data/GP/processing/GP_ST_SPROD.tif", overwrite = T)
writeRaster(GP_ST_TPROD, "data/GP/processing/GP_ST_TPROD.tif", overwrite = T)

###################################################################
# reload data

R2 <- vect("data/GP/processing/R2.shp")
X <- vect("data/GP/processing/X.shp")
GP_LT_NDVI95 <- rast("data/GP/processing/GP_LT_NDVI95.tif")
GP_LT_NDVIampl <- rast("data/GP/processing/GP_LT_NDVIampl.tif")
GP_ST_TPROD <- rast("data/GP/processing/GP_ST_TPROD.tif")
GP_ST_SPROD <- rast("data/GP/processing/GP_ST_SPROD.tif"); gc()
