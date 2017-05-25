library(plyr)
library(readxl)
library(ggplot2)
library(reshape2)
library(lubridate)

#### Find corrolated companies

## Corrolations and lags
ccf_fit <- function(A, B){
  max_lag = 30
  res = ccf(A, B, lag.max = max_lag, plot = FALSE, type = "correlation")
  cor = res$acf[(max_lag+1):(2*max_lag+1),,1]
  lag = res$lag[(max_lag+1):(2*max_lag+1),,1]
  fit = data.frame(cor,lag)
  
  result = fit[c(which.max(fit$cor), which.min(fit$cor)),]
  result = fit[c(which.max(fit$cor), which.max(abs(fit$cor))),]
  return(result)
}

## Create data matrix where lagging corrolations are shifted
one_fit_matrix <- function(lag_array, data, index){
  n = 30
  l = 756
  select = 1:l
  oneFitMatrix = matrix(0, nrow=(l+n), ncol=100, byrow = FALSE)
  for (i in 1:100){
    lag = lag_array[index, i]
    oneFitMatrix[(select+lag),i] = data[select , i]
  }
  return(oneFitMatrix)
}



## Read Price Close Data
df <- read_excel("All data file.xlsx", 
                 sheet = 11,
                 col_names = TRUE)
idx <- read_excel("ID_translate.xlsx",
                 col_names = TRUE)


## Converet the data into matrix format
indlist = names(table(df$ID))
dataMatrix = matrix(0, nrow=756, ncol=100, byrow = FALSE)
normdataMatrix = matrix(0, nrow=756, ncol=100, byrow = FALSE)

for (i in indlist){
  a = na.omit(df[df$ID==i,6])[1:756,]
  dataMatrix[,as.numeric(i)] = as.matrix(a)
  normdataMatrix[,as.numeric(i)] = as.matrix(scale(a))
}


## Show Price Close data for each company
melted_cormat <- melt(normdataMatrix)
melted_cormat$Var2 <- as.character(melted_cormat$Var2)
melted_cormat$Var2 <- factor(melted_cormat$Var2, levels=unique(melted_cormat$Var2))
ggplot(melted_cormat, aes(Var1, Var2)) +
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen") +
  xlab("Date")+ ylab("Company ID")+labs(fill="Price Close")

## Show Price Close data for each company
melted_cormat <- melt(normdataMatrix)
melted_cormat$Date = df$Date
for (i in c(4, 7, 14, 17, 23, 30, 53, 70, 79, 85)){
melted_cormat <- melted_cormat[!(melted_cormat$Var2==i),]
}
melted_cormat$Var2 <- factor(unlist(idx[melted_cormat$Var2,2]))
ggplot(melted_cormat, aes(Date, Var2)) +
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen") +
  xlab("Date")+ ylab("Ticket")+labs(fill="Scaled Price Close") + ggtitle("Company Stock Values Scaled Over a 3 year Timeframe")

## Calculate the lag and corrolation of each company for each company
lagMatrix = matrix(0, nrow=100, ncol=100, byrow = FALSE)
corMatrix = matrix(0, nrow=100, ncol=100, byrow = FALSE)
for (i in 1:100){
  for (j in 1:100){
    if(!any(is.na(dataMatrix[,i])) && !any(is.na(dataMatrix[,j]))){
      c = ccf_fit(dataMatrix[,i],dataMatrix[,j])
      lagMatrix[i,j] = c[2,2];
      corMatrix[i,j] = c[2,1];
      if (i==j){
        lagMatrix[i,j] = 0;
        corMatrix[i,j] = 0;
      }
    }else{
      lagMatrix[i,j] = 0;
      corMatrix[i,j] = 0;
    }
  }
  print(i)
}


## Show Lag for each corrolation(White = good, Red = bad)
melted_cormat <- melt(lagMatrix)
ggplot(melted_cormat, aes(Var1, Var2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = "darkred", high = "white") +
  xlab("Company ID")+ ylab("Competitor ID")+labs(fill="Lag")


## Show Correlation between each company(Blue & Red = good, White = bad)
melted_cormat <- melt(corMatrix)
for (i in c(4, 7, 14, 17, 23, 30, 53, 70, 79, 85)){
  melted_cormat <- melted_cormat[!(melted_cormat$Var2==i),]
  melted_cormat <- melted_cormat[!(melted_cormat$Var1==i),]
}
melted_cormat$Var2 <- factor(unlist(idx[melted_cormat$Var2,2]))
melted_cormat$Var1 <- factor(unlist(idx[melted_cormat$Var1,2]))
ggplot(melted_cormat, aes(Var1, Var2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkgreen") +
  xlab("Company Ticket")+ ylab("Competitor Ticket")+labs(fill="Correlation") + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) + ggtitle("All Company Correlations")




## Create ordered matrix based on the corrolations and display the gradients
tieMatrixID = matrix(0, nrow=100, ncol=100, byrow = FALSE)
tieMatrixVal = matrix(0, nrow=100, ncol=100, byrow = FALSE)
tieMatrixLag = matrix(0, nrow=100, ncol=100, byrow = FALSE)
for (i in 1:100){
  order = order(-abs(corMatrix[i,]))
  tieMatrixID[i,] = order
  tieMatrixVal[i,] = corMatrix[i,order]
  tieMatrixLag[i,] = lagMatrix[i,order]
}


## Show gradients(Blue & Red = good, White = bad)
melted_cormat <- melt(tieMatrixVal[1:100,])
melted_cormat$Var1 <- as.character(melted_cormat$Var1)
melted_cormat$Var1 <- factor(melted_cormat$Var1, levels=unique(melted_cormat$Var1))
ggplot(melted_cormat, aes(Var1, Var2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue") +
  xlab("Company ID")+ ylab("Correlated Competitors")+labs(fill="Correlation") + 
  theme(axis.text.y = element_blank())


## Show gradients lag(White = good, Red = bad)
melted_cormat <- melt(tieMatrixLag[1:100,])
melted_cormat$Var1 <- as.character(melted_cormat$Var1)
melted_cormat$Var1 <- factor(melted_cormat$Var1, levels=unique(melted_cormat$Var1))
ggplot(melted_cormat, aes(Var1, Var2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = "darkred", high = "white") +
  xlab("Company ID")+ ylab("Correlated Competitors")+labs(fill="Lag") + 
  theme(axis.text.y = element_blank())



## Calculate and display the 3 best corrolated competitors for a company, both as is and lag adjusted
company = 9
best = tieMatrixID[company,1:3]
oneFitMatrix = one_fit_matrix(lagMatrix,normdataMatrix,company)
melted_cormat <- as.data.frame(oneFitMatrix[,best])
widenormdataMatrix = normdataMatrix[c(1:756,(1:30)*0+756),]

#Show 3 Best corrolations
ggplot(melted_cormat, aes(x = 1:nrow(melted_cormat))) +
  geom_line(aes(y = widenormdataMatrix[,company], col=factor(unlist(idx[company,2])) ))+
  geom_line(aes(y = widenormdataMatrix[,best[1]]  * ((corMatrix[company,best[1]] > 0)*2-1), col=factor(unlist(idx[best[1],2])) )) +
  geom_line(aes(y = widenormdataMatrix[,best[2]]  * ((corMatrix[company,best[2]] > 0)*2-1), col=factor(unlist(idx[best[2],2])) )) +
  geom_line(aes(y = widenormdataMatrix[,best[3]]  * ((corMatrix[company,best[3]] > 0)*2-1), col=factor(unlist(idx[best[3],2])) )) +
  xlab("Date")+ ylab("Price Close Normalized")+labs(col="") + 
  theme(axis.text.y = element_blank())

#Show Lag adjusted
ggplot(melted_cormat, aes(x = 1:nrow(melted_cormat))) +
  geom_line(aes(y = widenormdataMatrix[,company], col=factor(unlist(idx[company,2])) ))+
  geom_line(aes(y = V1 * ((corMatrix[company,best[1]] > 0)*2-1), col=factor(unlist(idx[best[1],2])) )) +
  geom_line(aes(y = V2 * ((corMatrix[company,best[2]] > 0)*2-1), col=factor(unlist(idx[best[2],2])) )) +
  geom_line(aes(y = V3 * ((corMatrix[company,best[3]] > 0)*2-1), col=factor(unlist(idx[best[3],2])) )) +
  xlab("Date")+ ylab("Price Close Normalized")+labs(col="") + 
  theme(axis.text.y = element_blank())


## Save lag and corrolation information of each companys competitor
CorLagDF <- melt(corMatrix)
CorLagDF$Lag = melt(lagMatrix)[,3]
CorLagDF = plyr::rename(CorLagDF, c("Var1"="Company","Var2"="Competitor", "value"="Correlation"))
CorLagDF = CorLagDF[order(-abs(CorLagDF$Correlation)),]
write.csv2(CorLagDF, file = "Correlations.csv")

