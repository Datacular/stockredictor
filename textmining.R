library(readxl)
# NB! Use the latest tidytext from GitHub
# library(devtools)
# install_github("juliasilge/tidytext")
library(tidytext)
library(tidyr)
library(tm)
library(dplyr)
library(methods)

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

# SENTIMENT CALCULATION
# Fetch the ID - ticker correspondance
tickers <- as.data.frame(read_excel("All data file.xlsx",
                                    range = "ID!A1:A101",
                                    trim_ws = TRUE))

# Change the ID in Financials to the corresponding ticker
stock.data <- as.data.frame(read_excel("All data file.xlsx",
                                       range = "Financials!A1:K75601",
                                       trim_ws = TRUE))

stock.data <- as.data.frame(read.csv('financials.train.csv'))


for (i in 1:nrow(stock.data)){
  stock.data$ID[[i]][1] <- tickers[stock.data$ID[[i]][1],]
}

# Fetch the previously found sentiment word counts
sentiments <- read.csv("sentiments.csv")
# Most suitable when we want [-1,...,1] scoring
findSents <- function(pos, neg){
  return((pos - neg)/(pos + neg + 0.00000001))
}

# Find the sentiment scores based on the function
sents <- findSents(sentiments$positive, sentiments$negative)

# Calculate the sentiment scores for each day based on the surrounding
# sentiment values
findSentPeriod <- function(sents, sentiments){
  curr_sentiment <- rep(0,7)
  date_sentiment <- data.frame(sentiments$Ticker[1],
                               sentiments$Day[1], 
                               sentiments$Month[1], 
                               sentiments$Year[1],
                               0)
  for (i in 1:(nrow(sentiments)-1)){
    # check if we have several articles on same day
    if (sentiments$Month[i]==sentiments$Month[i+1] & 
        sentiments$Day[i] == sentiments$Day[i+1] &
        sentiments$Year[i] == sentiments$Year[i+1]){
      # TODO: Make so that same date ones are not devalued
      # Recalculate the weights of previous articles in the sentiment
      curr_sentiment <- c(sents[i], curr_sentiment[1]*0.9, curr_sentiment[2]*0.8, 
                          curr_sentiment[3]*0.6, curr_sentiment[4]*0.5,
                          curr_sentiment[5]*0.4, curr_sentiment[6]*0.2)
    } else {
      # Recalculate the weights of previous articles in the sentiment
      curr_sentiment <- c(sents[i], curr_sentiment[1]*0.9, curr_sentiment[2]*0.8, 
                          curr_sentiment[3]*0.6, curr_sentiment[4]*0.5,
                          curr_sentiment[5]*0.4, curr_sentiment[6]*0.2)
      #print(mean(curr_sentiment))
      date_sentiment[(nrow(date_sentiment)+1),] <- c(as.String(sentiments$Ticker[i]),
                                                     sentiments$Day[i], 
                                                     sentiments$Month[i], 
                                                     sentiments$Year[i],
                                                     mean(curr_sentiment))
      
    }
    # check if we are dealing with the same stock for next article, or should we re-
    # initalise the sentiment score
    if (sentiments$Ticker[i]!=sentiments$Ticker[i+1]){
      curr_sentiment <- rep(0,7)
    }
    
  }
  return(date_sentiment)
}

date_sentiments <- findSentPeriod(sents, sentiments)
date_sentiments <- date_sentiments[2:nrow(date_sentiments),]
colnames(date_sentiments) <- c('ID','Day','Month','Year','PredSentiment')
head(date_sentiments)

dates <- rep(as.Date('12/15/16',format='%m/%d/%y'),nrow(date_sentiments))
for (i in 1:nrow(date_sentiments)){
  x <- paste(date_sentiments$Month[i], '/', date_sentiments$Day[i],
             '/', date_sentiments$Year[i], sep = '')
  dates[i] <- as.Date(x, format='%m/%d/%y')
}
dateframe <- data.frame(dates, date_sentiments$ID, date_sentiments$PredSentiment)
colnames(dateframe) <- c('Date', 'ID', 'Sentiment')

singleStock <- function(ticker, sent.data, stock.data){
  sent.data <- sent.data[which(sent.data$ID == ticker),]
  stock.data <- stock.data[which(stock.data$ID == ticker),]
  joined.data <- data.frame(stock.data$Date[1], stock.data$ID[1], stock.data$Price.Close[1],
                            stock.data$Volume[1], stock.data$TurnoverProxy[1],
                            stock.data$Dividend.yield[1], sent.data$Sentiment[1])
  d <- 1
  for (i in 2:(nrow(stock.data))){
    # Check if we have new information for this period, then change sentiment
    if(is.na(sent.data$Date[d])){
      break
    }
    if (as.Date(stock.data$Date[i], format='%d/%m/%Y') >= sent.data$Date[d]){
      while (as.Date(stock.data$Date[i], format='%d/%m/%Y') >= sent.data$Date[d]){
        d <- d + 1
        if(is.na(sent.data$Date[d])){
          break
        }
      }
      # glue the stuff together based on the index d-1 and i
      # [d-2] if with previous date sentiment
      # [d-1] if iwth current date sentiment
      if ((d-2) <= 0){
        joined.data[i,] <- data.frame(stock.data$Date[i], stock.data$ID[i], stock.data$Price.Close[i],
                                      stock.data$Volume[i], stock.data$TurnoverProxy[i],
                                      stock.data$Dividend.yield[i],
                                      sent.data$Sentiment[max(1,d-1)])
      }else{
        joined.data[i,] <- data.frame(stock.data$Date[i], stock.data$ID[i], stock.data$Price.Close[i],
                                      stock.data$Volume[i], stock.data$TurnoverProxy[i],
                                      stock.data$Dividend.yield[i],
                                      sent.data$Sentiment[d-2])
      }
    }else{
      if ((d-2) <= 0){
        joined.data[i,] <- data.frame(stock.data$Date[i], stock.data$ID[i], stock.data$Price.Close[i],
                                      stock.data$Volume[i], stock.data$TurnoverProxy[i],
                                      stock.data$Dividend.yield[i],
                                      sent.data$Sentiment[max(1,d-1)])
      }else{
        joined.data[i,] <- data.frame(stock.data$Date[i], stock.data$ID[i], stock.data$Price.Close[i],
                                      stock.data$Volume[i], stock.data$TurnoverProxy[i],
                                      stock.data$Dividend.yield[i],
                                      sent.data$Sentiment[d-2])
      }
    }
  }
  return(joined.data)
}


## Create test data
stock.data.test <- as.data.frame(read.csv('financials.test.csv'))
for (i in 1:nrow(stock.data.test)){
  stock.data.test$ID[[i]][1] <- tickers[stock.data.test$ID[[i]][1],]
}

testStock <- function(stock, dateframe, stock.data.test, model){
  joined.data.test <- singleStock(stock, dateframe, stock.data.test)
  colnames(joined.data.test) <- c("Date", "ID", "Price Close", "Volume", "TurnoverProxy",
                                  "Dividend yield", "Sentiment")
  joined.data.test$Sentiment <- as.numeric(as.character(joined.data.test$Sentiment))
  price.diff <- rep(0, nrow(joined.data.test))
  for (i in 2:nrow(joined.data.test)){
    price.diff[i] <- joined.data.test$`Price Close`[i] - joined.data.test$`Price Close`[i-1]
  }
  price.diff[is.na(price.diff)] <- 0
  joined.data.test$`Price Close` <- price.diff
  # predictions from here
  pred.lm <- predict(model, joined.data.test)
  return(data.frame(pred.lm, price.diff))
}

calcProfit <- function(prediction, ref){
  holding <- 0
  profit <- 0
  for (i in 1:length(prediction)){
    if (prediction[i] > 0){
      holding <- 10
    } else if(prediction[i] < 0){
      holding <- 0
    }
    profit <- profit + ref[i]*holding
  }
  return(profit)
}

trainStock <- function(stock, dateframe, stock.data, stock.data.test){
  ## Create training data
  joined.data <- singleStock(stock, dateframe, stock.data)
  colnames(joined.data) <- c("Date", "ID", "Price Close", "Volume", "TurnoverProxy",
                             "Dividend yield", "Sentiment")
  joined.data$Sentiment <- as.numeric(as.character(joined.data$Sentiment))
  
  price.diff <- rep(0, nrow(joined.data))
  for (i in 2:nrow(joined.data)){
    price.diff[i] <- joined.data$`Price Close`[i] - joined.data$`Price Close`[i-1]
  }
  price.diff[is.na(price.diff)] <- 0
  #print(price.diff)
  joined.data$`Price Close` <- price.diff
  
  ## LEARN THE MODELS HERE
  library(caret)
  stock.lm.all <- lm(`Price Close` ~ Sentiment + `Dividend yield` + Volume + TurnoverProxy, 
                     data = joined.data)
  stock.lm <- lm(`Price Close` ~ Sentiment, data = joined.data)
  stock.svm.poly.all <- train(`Price Close` ~ Sentiment + `Dividend yield` + Volume + TurnoverProxy, 
                              data = joined.data, method = "svmPoly")
  stock.svm.poly <- train(`Price Close` ~ Sentiment, data = joined.data, method = "svmPoly")
  stock.svm.rad.all <- train(`Price Close` ~ Sentiment + `Dividend yield` + Volume + TurnoverProxy, 
                             data = joined.data, method = "svmRadial")
  stock.svm.rad <- train(`Price Close` ~ Sentiment, data = joined.data, method = "svmRadial")
  stock.rf.all <- train(`Price Close` ~ Sentiment + `Dividend yield` + Volume + TurnoverProxy, 
                        data = joined.data, method = "rf")
  stock.rf <- train(`Price Close` ~ Sentiment, data = joined.data, method = "rf")
  
  ## TEST MODELS HERE
  prediction <- testStock(stock, dateframe, stock.data.test, stock.lm)
  profit.lm <- calcProfit(prediction[,1], prediction[,2])
  prediction <- testStock(stock, dateframe, stock.data.test, stock.lm.all)
  profit.lm.all <- calcProfit(prediction[,1], prediction[,2])
  prediction <- testStock(stock, dateframe, stock.data.test, stock.svm.poly)
  profit.svm.poly <- calcProfit(prediction[,1], prediction[,2])
  prediction <- testStock(stock, dateframe, stock.data.test, stock.svm.poly.all)
  profit.svm.poly.all <- calcProfit(prediction[,1], prediction[,2])
  prediction <- testStock(stock, dateframe, stock.data.test, stock.svm.rad)
  profit.svm.rad <- calcProfit(prediction[,1], prediction[,2])
  prediction <- testStock(stock, dateframe, stock.data.test, stock.svm.rad.all)
  profit.svm.rad.all <- calcProfit(prediction[,1], prediction[,2])
  prediction <- testStock(stock, dateframe, stock.data.test, stock.rf)
  profit.rf <- calcProfit(prediction[,1], prediction[,2])
  prediction <- testStock(stock, dateframe, stock.data.test, stock.rf.all)
  profit.rf.all <- calcProfit(prediction[,1], prediction[,2])
  result <- data.frame(profit.lm,profit.lm.all,profit.svm.poly,
                       profit.svm.poly.all,profit.svm.rad,profit.svm.rad.all,profit.rf,
                       profit.rf.all)
  return(result)
}

profits <- data.frame("Name", 0, 0, 0, 0, 0, 0, 0, 0)
colnames(profits) <- c("Ticker", "lm", "lm.all", "svmPoly",
                       "svmPoly.all", "svmRadial", "svmRadial.all", "Random.Forest",
                       "Random.Forest.all")
for (i in 80:length(tickers[[1]])){
  print(tickers[[1]][i])
  profits[i,] <- trainStock(tickers[[1]][i], dateframe, stock.data, stock.data.test)
  profits[i,1] <- as.character(tickers[[1]][i])
  print(profits[i,])
}

write.csv(profits, file = "profits.csv", row.names=FALSE)




