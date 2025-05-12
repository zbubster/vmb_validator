# knihovnik

knihovnik <- function(knihovny) {
  for (kniha in knihovny) {
    if (!require(kniha, character.only = TRUE)) {
      install.packages(kniha, dependencies = T)
      library(kniha, character.only = TRUE)
    }
  }
}

co <- c("terra", "dplyr", "ggplot2")
knihovnik(co)

rm(co)
gc()
