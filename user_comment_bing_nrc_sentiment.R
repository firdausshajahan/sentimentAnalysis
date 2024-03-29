library(readr)
library(tidyr)
library(dplyr)
library(tidytext)
library(splitstackshape)
library(textdata)
library(wordcloud)
library(reshape2)
library(ggplot2)
library(jsonlite)

#set working directory to load data, you can change yours here
setwd("~/firdaus/studies/researchProject/SentimentAnalysis/data/cleanData")
combineDataComplete <- read_csv("combineDataComplete.csv")

userComments <- combineDataComplete %>% filter(name!="Grab")

dataWords <- userComments %>%
  select(id, comment) %>% 
  group_by(row_number()) %>% 
  ungroup()

user_comment_tokens <- dataWords %>%
  unnest_tokens(word, comment)

user_comment_tokens <- user_comment_tokens %>%
  anti_join(stop_words)

bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")
 
bing_user_word_no_count <- user_comment_tokens %>%
 inner_join(get_sentiments("bing"))

bing_word_time <- bing_user_word_no_count %>%
  inner_join(combineDataComplete, by = "id") %>%
  select(id,word,sentiment,time)
#change yours export location here
write_csv(bing_word_time, path = "C:/Users/seetrustudio-29/Documents/firdaus/studies/researchProject/SentimentAnalysis/data/finalData/bingLabelData.csv")

bing_user_word_counts <- user_comment_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_user_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  labs(y = "Negative compared Positive", x = NULL) +
  coord_flip() + 
  ggtitle('Bing: Top positive and negative word count in the GrabPay Facebook Post')

nrc_user_word_no_counts <- user_comment_tokens %>%
  inner_join(get_sentiments("nrc"))

nrc_word_time <- nrc_user_word_no_counts %>%
  inner_join(combineDataComplete, by = "id") %>%
  select(id,word,sentiment,time) %>%
  filter( word !="grab")
#change yours export location here
write_csv(nrc_word_time, path = "C:/Users/seetrustudio-29/Documents/firdaus/studies/researchProject/SentimentAnalysis/data/finalData/nrcLabelData.csv")

nrc_user_word_counts <- user_comment_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  filter( word != "grab" ) %>%
  ungroup()

nrc_user_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  labs(y = "NRC Sentiment", x = NULL) +
  coord_flip() + 
  ggtitle('NRC: Top positive and negative word count in the GrabPay Facebook Post')

dataWordBing <- user_comment_tokens %>%
  inner_join(get_sentiments("bing"))

dataWordBing2 <- dataWordBing %>%
  with(dataWordBing)

#Basic Wordcloud
dataWordBing2 %>%
  with(dataWordBing2)%>%
  anti_join (stop_words) %>%
  count(word) %>%
  with (wordcloud(word, n, max.words = 100))

#Wordcloud group +-
dataWordBing2 %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D","#00BFC4"),
                   max.words = 100)

bing_user_sentiment_counts <- user_comment_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, sort = TRUE) %>%
  ungroup()

bing_user_sentiment_counts %>%
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  ggtitle('Bing: Positive and Negative in GrabPay Comments')

nrc_user_sentiment_counts <- user_comment_tokens %>%
  filter( word != "grab" ) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment, sort = TRUE) %>%
  ungroup()

nrc_user_sentiment_counts %>%
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(y = "NRC Sentiment Counts", x = NULL) +
  ggtitle('NRC: Emotions in GrabPay Comments')

#change yours json export location here
allDataJson <- toJSON(combineDataComplete)
write(allDataJson, "C:/Users/seetrustudio-29/Documents/firdaus/studies/researchProject/SentimentAnalysis/data/finalData/json/allDataJson.json")

bingDataJson <- toJSON(bing_user_word_counts)
write(bingDataJson, "C:/Users/seetrustudio-29/Documents/firdaus/studies/researchProject/SentimentAnalysis/data/finalData/json/bingDataJson.json")

nrcDataJson <- toJSON(nrc_user_word_counts)
write(nrcDataJson, "C:/Users/seetrustudio-29/Documents/firdaus/studies/researchProject/SentimentAnalysis/data/finalData/json/nrcDataJson.json")

bingWordTimeDataJson <- toJSON(bing_word_time)
write(bingWordTimeDataJson, "C:/Users/seetrustudio-29/Documents/firdaus/studies/researchProject/SentimentAnalysis/data/finalData/json/bingWordTimeDataJson.json")

nrcWordTimeDataJson <- toJSON(nrc_word_time)
write(nrcWordTimeDataJson, "C:/Users/seetrustudio-29/Documents/firdaus/studies/researchProject/SentimentAnalysis/data/finalData/json/nrcWordTimeDataJson.json")

bingSentimentCountJson <- toJSON(bing_user_sentiment_counts)
write(bingSentimentCountJson, "C:/Users/seetrustudio-29/Documents/firdaus/studies/researchProject/SentimentAnalysis/data/finalData/json/bingSentimentCountJson.json")

nrcSentimentCountJson <- toJSON(nrc_user_sentiment_counts)
write(nrcSentimentCountJson, "C:/Users/seetrustudio-29/Documents/firdaus/studies/researchProject/SentimentAnalysis/data/finalData/json/nrcSentimentCountJson.json")