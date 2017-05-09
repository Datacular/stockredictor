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
descr_no_articles <- 
  descriptive_stats[order(descriptive_stats$`No. Articles`),c(1,2)]

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