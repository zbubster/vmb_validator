

R2 <- vect("data/GP/processing/R2.shp")
X <- vect("data/GP/processing/X.shp")
GP_LT_NDVI95 <- rast("data/GP/processing/GP_LT_NDVI95.tif")
GP_LT_NDVIampl <- rast("data/GP/processing/GP_LT_NDVIampl.tif")
GP_ST_TPROD <- rast("data/GP/processing/GP_ST_TPROD.tif")
GP_ST_SPROD <- rast("data/GP/processing/GP_ST_SPROD.tif")

raster_list <- list(
  NDVI95   = GP_LT_NDVI95,
  NDVIampl = GP_LT_NDVIampl,
  TPROD    = GP_ST_TPROD,
  SPROD    = GP_ST_SPROD
)
names(R2)
names(raster_list)

raster_list <- lapply(raster_list, function(r) {
  NAflag(r) <- 254
  r <- classify(r, cbind(254, NA))  # Replace 254 with NA
  as.factor(r)
})

vector <- R2


head(vector)
writeVector(vector, "data/pokusnej/vec.shp", overwrite = T, options = "ENCODING=UTF-8", filetype = "ESRI Shapefile") 

