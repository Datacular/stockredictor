library(readxl)

articles <- as.data.frame(read_excel("All data file.xlsx",
                                     range = "Content!A1:O25987",
                                     trim_ws = TRUE))