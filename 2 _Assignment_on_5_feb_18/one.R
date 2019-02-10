library(ggplot2)
library(GGally)
library(RColorBrewer)

stock_data <- read.csv(file.choose())

dim(stock_data)
str(stock_data)
summary(stock_data)
head(stock_data)

identical(stock_data$Close, stock_data$Adj.Close)

stock_data$Adj.Close <- NULL
str(stock_data)
stock_data$Date <- dmy(stock_data$Date)

#my_palet_1 <- brewer.pal(9, "Greens")

plot(stock_data$Date, stock_data$Volume)
ggplot(data = stock_data, aes(Date, Volume)) +
  geom_jitter()

ggplot(data = stock_data, aes(Date, Volume)) +
  geom_point(position = "jitter", alpha = .5) +
  geom_abline()

boxplot(stock_data$Open)
hist(stock_data$Open)

boxplot(stock_data$High)
hist(stock_data$High)

boxplot(stock_data$Low)
hist(stock_data$Low)

boxplot(stock_data$Close)
hist(stock_data$Close)

boxplot(stock_data$)