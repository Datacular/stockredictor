library(readxl)
library(tidytext)
library(tm)
library(dplyr)

articles <- as.data.frame(read_excel("All data file.xlsx",
                                     range = "Content!A1:O25987",
                                     trim_ws = TRUE))

tokenizer <- function(df){
  for (i in 1:nrow(df)) {
    df[i,"Content"] <- paste((tidy(VCorpus(VectorSource(df[i,"Content"]))) %>% 
                            unnest_tokens(word, text))$word, collapse = ';')
  }
  return(df)
}

articles <- tokenizer(articles)
