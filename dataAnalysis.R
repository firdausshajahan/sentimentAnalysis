library(readr)
library(dplyr)
library(tidytext)
library(splitstackshape)
library(textdata)
library(wordcloud)
library(reshape2)
library(ggplot2)
library(lubridate)

combineDataComplete <- read_csv("~/firdaus/studies/researchProject/SentimentAnalysis/data/cleanData/combineDataComplete.csv")
grabComments <- combineDataComplete %>% filter(name=="Grab")
userComments <- combineDataComplete %>% filter(name!="Grab")

grabComments <- grabComments %>%
  mutate(time = as_date(grabComments$time))

grabCommentFrequency <- grabComments %>%
  group_by(grabComments$time) %>% 
  summarise(frequency = n())

colnames(grabCommentFrequency) <- c("date", "frequency")

userComments <- userComments %>%
  mutate(time = as_date(userComments$time))

userCommentFrequency <- userComments %>%
  group_by(userComments$time) %>% 
  summarise(frequency = n())

colnames(userCommentFrequency) <- c("date", "frequency")

xValue <- grabCommentFrequency$date
yValue <- grabCommentFrequency$frequency
data <- data.frame(xValue,yValue)

xValue2 <- userCommentFrequency$date
yValue2 <- userCommentFrequency$frequency
data2 <- data.frame(xValue2,yValue2)

# Plot
# ggplot(data, aes(x=xValue, y=yValue)) +
#   geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=2) +
#   ggtitle("Grab Comment") +
#   ylab("Frequency") +
#   xlab("Date")

ggplot() +
  geom_line(data = data, aes(x = xValue, y = yValue), color = "green") +
  geom_line(data = data2, aes(x = xValue2, y = yValue2), color = "blue") +
  xlab('Date') +
  ylab('Frequency Comments')



