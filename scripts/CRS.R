# CRS vyresit

# funkce pro kontrolu shodnosti CRS
check_same_crs <- function(layers) {
  crs_list <- lapply(layers, crs)
  all(sapply(crs_list, function(x) {
    x == crs_list[[1]]
    }))
}

# funkce k harmonizaci CRS mezi vrstvami
harmonize_crs <- function(layers) {
  if (check_same_crs(layers)) {
    cat("DOBRY!\n")
    return(layers)
  } else {
    cat("The CRS of layers do not match.\n\n")
    for (i in seq_along(layers)) {
      cat(paste0("[", i, "] ", names(layers)[i], " - ", crs(layers[[i]], describe = T), "\n"))
      cat("\n\n")
    }
    refCRS <- as.numeric(readline(prompt = "Enter the number of the layer to use as reference CRS: "))
    if (is.na(refCRS) || refCRS < 1 || refCRS > length(layers)) {
      stop("Invalid selection.")
    }
    ref_crs <- crs(layers[[refCRS]])
    for (i in seq_along(layers)) {
      if (i != refCRS) {
        cat(paste("Reprojecting", names(layers)[i], "to match", names(layers)[refCRS], "...\n"))
        layers[[i]] <- project(layers[[i]], ref_crs)
      }
    }
    cat("All layers now share the same CRS.\n")
    return(layers)
  }
}

all_layers <- c(raster_list, list(vector = vector)) ######################### prepsat!!

res <- harmonize_crs(all_layers)

nr <- length(raster_list)
raster_list <- res[1:nr]
vector <- res[[nr + 1]]

rm(all_layers, res, nr)
gc()
