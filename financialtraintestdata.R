library(readxl)

stock.data <- as.data.frame(read_excel("All data file.xlsx",
                                       range = "Financials!A1:K75601",
                                       trim_ws = TRUE))

#Years
years <- c(14,15,16)
# January, February, March (Q1)
Q1 <- c(1,2,3)
# April, May, June (Q2)
Q2 <- c(4,5,6)
# July, August, September (Q3)
Q3 <- c(7,8,9)
# October, November, December (Q4)
Q4 <- c(10,11,12)

financials.train <-
  data.frame(
    "Date" = numeric(0),
    "Month" = numeric(0),
    "quarterly" = numeric(0),
    "Year" = numeric(0),
    "ID" = numeric(0),
    "Price Close" = numeric(0),
    "LNPchange" = numeric(0),
    "Volume" = numeric(0),
    "TurnoverProxy" = numeric(0),
    "LNTurnoverProxy" = numeric(0),
    "Dividend yield" = numeric(0)
  )
financials.test <- financials.train

for (i in 1:4){
  test_year <- sample(years,1)
  train_years <-  years[!years %in% test_year]
  if(i == 1){
    test <- stock.data[which(stock.data$Year == test_year & stock.data$Month %in% Q1),]
    train <- stock.data[which(stock.data$Year %in% train_years & stock.data$Month %in% Q1),]
  } else if(i==2){
    test <- stock.data[which(stock.data$Year == test_year & stock.data$Month %in% Q2),]
    train <- stock.data[which(stock.data$Year %in% train_years & stock.data$Month %in% Q2),]
  } else if(i==3){
    test <- stock.data[which(stock.data$Year == test_year & stock.data$Month %in% Q3),]
    train <- stock.data[which(stock.data$Year %in% train_years & stock.data$Month %in% Q3),]
  } else{
    test <- stock.data[which(stock.data$Year == test_year & stock.data$Month %in% Q4),]
    train <- stock.data[which(stock.data$Year %in% train_years & stock.data$Month %in% Q4),]
  }
  financials.train <- rbind(financials.train,train)
  financials.test <- rbind(financials.test,test)
}

write.csv(financials.train, file = "financials.train.csv", row.names=FALSE)
write.csv(financials.test, file = "financials.test.csv", row.names=FALSE)
