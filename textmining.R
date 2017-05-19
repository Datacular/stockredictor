library(readxl)
# NB! Use the latest tidytext from GitHub
# library(devtools)
# install_github("juliasilge/tidytext")
library(tidytext)
library(tidyr)
library(tm)
library(dplyr)

articles <- as.data.frame(read_excel("All data file.xlsx",
                                     range = "Content!A1:O25987",
                                     trim_ws = TRUE))

add_sentiment_counts <- function(df){
  df[,c("positive", "negative", "litigious", "uncertainty", "constraining", "superfluous")] <- 0
  loughran_sentiments <- get_sentiments("loughran")
  for (i in 1:nrow(df)) {
    processable_content <- VCorpus(VectorSource(df[i,"Content"]))
    tokens <- unnest_tokens(tidy(processable_content), word, text)
    
    # Stock Sentiment count exactly like in http://tidytextmining.com/dtm.html#financial
    # Text Mining with R by Julia Silge and David Robinson
    sentiment_count <- inner_join(tokens, loughran_sentiments, by = "word") %>% count(sentiment)
    
    df[i,as.vector(sentiment_count$sentiment)] <- as.vector(sentiment_count$n)
  }
  return(df)
}

articles <- add_sentiment_counts(articles)
write.csv(articles, file = "sentiments.csv", row.names=FALSE)