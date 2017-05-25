library(plyr)
library(readxl)
library(ggplot2)
library(hydroGOF)
library(reshape2)
library(lubridate)

## Predict the Price Close for one company based on correlated companies
predict_for <- function(company_id, correlations, data, threshold=0){
  selection = correlations[correlations$Company==company_id & abs(correlations$Correlation)>threshold,]
  prediction = data.frame(prediction=numeric(nrow(data[data$ID==1,])))
  temp = 0
  for (i in selection$Competitor) {
    area = tibble('Price Close' = data[data$ID==i,6])
    #area = (data[data$ID==i,6])
    lag = selection$Lag[selection$Competitor==i]
    area = area[c((0:lag)*0+1,2:(nrow(area)-lag)),]
    prediction = selection$Correlation[selection$Competitor==i]*area + prediction   #Prediction function, corrolation as weight
    temp = temp + abs(selection$Correlation[selection$Competitor==i])
  }
  prediction = prediction/temp
  return(prediction)
}

## Read Price Close and Correlation data
df <- read_excel("All data file.xlsx", 
                 sheet = 11,
                 col_names = TRUE)
#df <- data.frame(read.csv("financials.train.csv"))
#df$ID <- as.numeric(df$ID )
#tibble(df[df$ID==i,6])

correlations = read.csv2(file = "Correlations.csv")


 ## Predict values based on correlated companies
company = 37
prediction = predict_for(company, correlations, df, 0.7)
truedata = tibble('Price Close' = df[df$ID==company,6])
## Display the predicted and true graphs
ggplot(truedata, aes(x = 1:nrow(truedata))) +
  geom_line(aes(y = truedata, col='Actual'))+
  geom_line(aes(y = prediction, col='Predicted')) +
  xlab("Date")+ ylab("Price Close")+labs(color="") + ggtitle("Prediction of Facebook Stock based on Corrolated Company Stocks")




## Calculate accuracies of each prediction
predict_stats = data.frame(ID = numeric(100),
                           pearson = numeric(100),
                           spearman = numeric(100),
                           rmse = numeric(100))
for (i in 1:100){
  prediction = predict_for(i, correlations, df, 0.7)    #Change 0.7 as it is the threshold for selecting corrolated companies
  truedata = tibble('Price Close' = df[df$ID==i,6])
  predict_stats[i,] = c(i,cor(truedata, prediction, method= 'pearson'),cor(truedata, prediction, method= 'spearman'),rmse(truedata, prediction, na.rm=TRUE)^0.5)
}
#predict_stats[is.na(predict_stats)] = 0
best_predictions = order(-(predict_stats$pearson+predict_stats$spearman))
predict_stats = predict_stats[best_predictions,]
predict_stats$ID <- as.character(predict_stats$ID)
predict_stats$ID <- factor(predict_stats$ID, levels=unique(predict_stats$ID))

## Show the corrolations of each company and its predicted value
ggplot(predict_stats, aes(x = predict_stats$ID)) +
  geom_point(aes(y = predict_stats$pearson, col='Pearson'))+
  geom_point(aes(y = predict_stats$spearman, col='Spearman'))+
  xlab("Company ID")+ ylab("Prediction Corrolation")+labs(color="Correlation Measure")


best_predictions = order(predict_stats$rmse)
predict_stats = predict_stats[best_predictions,]
predict_stats$ID <- as.character(predict_stats$ID)
predict_stats$ID <- factor(predict_stats$ID, levels=unique(predict_stats$ID))

## Show the corrolations of each company and its predicted value
ggplot(predict_stats, aes(x = predict_stats$ID)) +
  geom_point(aes(y = predict_stats$rmse))+
  xlab("Company ID")+ ylab("Prediction RMSE") + scale_y_log10() 

colMeans(predict_stats[,2:3],na.rm=TRUE)
predict_stats[1:5,]

correlations[correlations$Company==37,]
