# load VMB

# path <- readline(prompt = "Zadejte cestu k referencni vektorove Vrstve mapovani biotopu: \n(neco/jako/tohle/vector.shp)\ndata/VMB/VMB_bio_hab.shp\n") # cesta
path <- "data/VMB/VMB_Boletice.shp"
VMB <- vect(path)

new_cols <- data.frame(
  BIOTOP_CODES = as.factor(gsub(" \\(\\d+\\)", "", VMB$BIOTOP_SEZ)),
  HABIT_CODES = as.factor(gsub(" \\(\\d+\\)", "", VMB$HABIT_SEZ))
  
)
VMB <- cbind(VMB, new_cols)
VMB <- VMB[, c("SEGMENT_ID", "FSB", "BIOTOP_CODES", "HABIT_CODES", "SHAPE_Area", "DATUM")]

vector <- VMB

rm(VMB, path, new_cols)
gc()
