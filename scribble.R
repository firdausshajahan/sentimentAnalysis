library(readr)
library(dplyr)
library(tidytext)
library(splitstackshape)
library(textdata)
library(wordcloud)
library(reshape2)
library(ggplot2)

#setwd("~/firdaus/studies/researchProject/SentimentAnalysis")
combineDataComplete <- read_csv("~/firdaus/studies/researchProject/SentimentAnalysis/data/cleanData/combineDataComplete.csv")
dataWords <- cSplit(combineDataComplete,"comment", sep =" ", direction = "long")

dataWords <- dataWords %>%
  anti_join(stop_words)

grabComments <- dataWords %>% filter(name=="Grab")
userComments <- dataWords %>% filter(name!="Grab")

names(grabComments) <- c("id","name","word","time")
names(userComments) <- c("id","name","word","time")

userComments$word <- gsub(",$", "",userComments$word)
userComments$word <- gsub(":", "",userComments$word, fixed=TRUE)
userComments$word <- gsub(".", "",userComments$word, fixed=TRUE)

customerTibble <- as_data_frame(userComments)

customerTibbleBing <- customerTibble %>%
  inner_join(get_sentiments("bing"))

# customerTibbleAfinn <- customerTibble %>%
#   inner_join(get_sentiments("afinn"))
# 
# customerTibbleNrc <- customerTibble %>%
#   inner_join(get_sentiments("nrc"))

bing_word_counts <- customerTibble %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

customerTibbleBing <- customerTibbleBing %>%
  with(customerTibbleBing)

#Basic Wordcloud
customerTibbleBing %>%
  with(customerTibbleBing)%>%
  anti_join (stop_words) %>%
  count(word) %>%
  with (wordcloud(word, n, max.words = 100))

#Wordcloud group +-
customerTibbleBing %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D","#00BFC4"),
                   max.words = 100)


#############Grab Comments on Post#############################

grabComments$word <- gsub(",$", "",grabComments$word)
grabComments$word <- gsub(":", "",grabComments$word, fixed=TRUE)
grabComments$word <- gsub(".", "",grabComments$word, fixed=TRUE)

grabTibble <- as_data_frame(grabComments)

grabTibbleBing <- grabTibble %>%
  inner_join(get_sentiments("bing"))

grab_bing_word_counts <- grabTibble %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

grab_bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

grabTibbleBing <- grabTibbleBing %>%
  with(grabTibbleBing)

#Basic Wordcloud
grabTibbleBing %>%
  with(grabTibbleBing)%>%
  anti_join (stop_words) %>%
  count(word) %>%
  with (wordcloud(word, n, max.words = 100))

#Wordcloud group +-
grabTibbleBing %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D","#00BFC4"),
                   max.words = 100)