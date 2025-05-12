# cisteni rastru

# spustit, pokud jsou v ramci jednotlivych rastru ruzne definice NA
clean_raster <- function(rl){
  for (name in names(rl)) {
    cat("\nProcessing raster:", name, "\n")
    r <- rl[[name]]
    repeat {
      cat("Unique values:\n")
      print(unique(r))
      
      a <- readline(prompt = "Vsechny hodnoty plati? (yes/no): ")
      
      if (tolower(a) %in% c("yes", "y")) {
        break  # super, dalsi raster
      } else {
        val_to_replace <- readline(prompt = "Kterou hodnotu nahradit NA? ")
        val_to_replace <- as.numeric(val_to_replace)
        
        if (val_to_replace %in% unlist(unique(r))) {
          r[r == val_to_replace] <- NA
        } else {
          cat("Neplatny vstup.\n\n")
        }
      }
    }
    rl[[name]] <- r # upraveny raster zpet do listu
  }
  return(rl)
}

raster_list <- clean_raster(raster_list)
