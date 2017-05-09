library(readxl)
library(ggplot2)

fetch_descr <- function(file, range) {
  raw <- as.data.frame(read_excel(file, range = range, trim_ws = TRUE))
  fst_half <- raw[, c(1:5)]
  snd_half <- raw[, c(7:11)]
  colnames(snd_half) <- colnames(fst_half)
  return(rbind(fst_half, snd_half))
}

descriptive_stats <- fetch_descr("All data file.xlsx", "Sheet1!A3:K48")
descr_no_articles <- descriptive_stats[,c(1,2)]
descr_mean_length <- descriptive_stats[,c(1,3)]
descr_mean_posinegative_words <- descriptive_stats[,c(1,4,5)]


articles_distribution <- 
  ggplot(descr_no_articles, 
         aes(reorder(Ticker, -`No. Articles`),`No. Articles`, fill=`No. Articles`))+
  geom_bar(stat="identity")+ 
  theme(axis.text.x=element_text(angle=90))+
  xlab("Ticket") +
  ylab("No. Articles") +
  ggtitle("Distribution of Articles about Companies")

# Plot Distributions of Articles about Companies
plot(articles_distribution)


mean_length_distribution <- 
  ggplot(descr_mean_length, 
         aes(reorder(Ticker, -`Mean Length of an Article`),
             `Mean Length of an Article`, fill=`Mean Length of an Article`))+
  geom_bar(stat="identity")+ 
  theme(axis.text.x=element_text(angle=90))+
  xlab("Ticket") +
  ylab("Mean Length") +
  ggtitle("Mean Length of Articles about Companies")

# Plot Mean Length of Articles about Companies
plot(mean_length_distribution)

descr_mean_posinegative_words <-
  ggplot(descr_mean_posinegative_words, 
         aes(descr_mean_posinegative_words, group = 1))+
  theme(axis.text.x=element_text(angle=90))+
  xlab("Ticket") +
  ylab("Mean Number") +
  ggtitle("Mean Number of Positive/Negative Words about Companies")+
  geom_line(aes(x=Ticker,
                y =`Mean Number of positive words`,
                colour ="Positive"))+
  geom_line(aes(x=Ticker, 
                y =`Mean Number of negative words`,
                colour ="Negative")) 

# Plot Mean Number of Positive/Negative Words 
# found in articles about Companies
plot(descr_mean_posinegative_words)
