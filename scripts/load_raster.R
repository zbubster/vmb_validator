# data load 1
# tento skript podle zadane cesty nacte vsechny .tif soubory ve slozce a vytvori z nich objekt 'raster_list', ten vstupuje do dalsich analyz

#folder_path <- readline(prompt = "Zadejte cestu k rastrum! \n(neco/jako/tohle/) \ndata/GP/processing/") # cesta
folder_path <- "data/_Boletice_CZ0314123/GrasslandType/"
tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE) # nacte VSECHNY soubory .tif

raster_list <- list() # prazdny list pro tvorbu vysledku

for (file in tif_files) {
  raster_name <- tools::file_path_sans_ext(basename(file))  # odstarnit extension
  raster_list[[raster_name]] <- terra::rast(file) 
}

rm(file, folder_path, raster_name, tif_files)
gc()
