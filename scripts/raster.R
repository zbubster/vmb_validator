# raster metoda

library(terra)

rastr.files <- list.files(path = "data/_Boletice_CZ0314123/", pattern = ".tif$", full.names = T, recursive = T)
rastr.files

rastr <- lapply(rastr.files, rast)
rastr

empty_raster <- rast(rastr[[1]])
n_bands <- length(rastr)
empty_raster <- rast(nrows = nrow(rastr[[1]]), ncols = ncol(rastr[[1]]), nlyrs = n_bands, crs = crs(rastr[[1]]), ext = ext(rastr[[1]]))
empty_raster

for (i in 1:n_bands) {
  empty_raster[[i]] <- setValues(empty_raster[[i]], values(rastr[[i]]))
}

empty_raster
plot(empty_raster)

writeRaster(empty_raster, "data/_Boletice_CZ0314123/all_raster.tif", overwrite = T)
rm(empty_raster); gc()

################

ref <- rastr[[1]]

# Align all rasters to the reference: resample or project
aligned <- lapply(rastr, function(r) {
  if (!compareGeom(r, ref, stopOnError = FALSE)) {
    resample(r, ref, method = "near")  # or method = "near" for categorical data
  } else {
    r
  }
})

# Stack into a multi-layer raster
multi_layer <- rast(aligned)
crs(multi_layer)

# Save if needed
writeRaster(multi_layer, "stacked_layers.tif", overwrite = TRUE)
rm(multi_layer); gc()

################################################################################

rastr <- rast("data/_Boletice_CZ0314123/all_raster.tif")

vmb <- read.csv("data/VMB_intersect.csv", header = T)
str(vmb)
vmb$BIOTOP_CODES <- as.factor(gsub(" \\(\\d+\\)", "", vmb$BIOTOP_SEZ))
vmb$HABIT_CODES <- as.factor(gsub(" \\(\\d+\\)", "", vmb$HABIT_SEZ))
vmb <- vmb %>%
  select(SEGMENT_ID, FSB, BIOTOP_CODES, HABIT_CODES, SHAPE_Area, DATUM)
vmb$FSB <- as.factor(vmb$FSB)


