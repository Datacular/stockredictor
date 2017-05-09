library(readxl)

fetch_descr <- function(file, range) {
  raw <- as.data.frame(read_excel(file, range = range, trim_ws = TRUE))
  fst_half <- raw[, c(1:5)]
  snd_half <- raw[, c(7:11)]
  colnames(snd_half) <- colnames(fst_half)
  return(rbind(fst_half, snd_half))
}

descriptive_stats <- fetch_descr("All data file.xlsx", "Sheet1!A3:K48")
