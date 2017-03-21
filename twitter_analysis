# get package
require(twitteR)
library(dplyr)
library(ggplot2)
library(tidytext)

# do auth
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# get tweets
gvl_twitter <- searchTwitter("#codingdurer", n = 2000)
gvl_twitter_df <- twListToDF(gvl_twitter)

# remove retweets
gvl_twitter_unique <- gvl_twitter_df %>% filter(!isRetweet)

# remove link
gvl_twitter_nolink <- gvl_twitter_unique %>% mutate(text = gsub("https?://[\\w\\./]+", "", text, perl = TRUE))

# who is tweeting
people = gvl_twitter_nolink %>%
  count(screenName, sort = TRUE) %>% slice(1:20) %>%
  ggplot(aes(x = reorder(screenName, n, function(n) -n), y = n)) + 
  ylab("Number of Tweets") +
  xlab("") +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Most active twitter users")

# what is being said
tweet_words <- gvl_twitter_nolink %>% select(id, text) %>% unnest_tokens(word, text)

# remove stop words
my_stop_words <- stop_words %>% select(-lexicon) %>% bind_rows(data.frame(word = c("codingdurer","https", "t.co", "amp")))
tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)

# remove name of tweeters
gvl_twitter_df$screenName = tolower(gvl_twitter_df$screenName)
tweet_words_interesting = filter(tweet_words_interesting, !(word %in% unique(gvl_twitter_df$screenName)))

# singularize words
tweet_words_interesting$word2 = singularize(unlist(tokenize(tweet_words_interesting$word)))
tweet_words_interesting$word2[tweet_words_interesting$word2 == "datum"] = "data"
tweet_words_interesting$word2[tweet_words_interesting$word == "people"] = "people"

word = tweet_words_interesting %>% 
  count(word2, sort = TRUE) %>% 
  slice(1:20) %>% 
  ggplot(aes(x = reorder(word2, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab("Word Occurrence") +
  xlab("") +
  ggtitle("Most used words in tweets")

# plot all together
grid.arrange(people, word, nrow=2, top = "Twitter Analysis of #codingdurer")

# sentiment
bing_lex <- get_sentiments("bing")
gvl_sentiment <- tweet_words_interesting %>% left_join(bing_lex)
gvl_sentiment %>% filter(!is.na(sentiment)) %>% group_by(sentiment) %>% summarise(n = n())

# Plot the frequency of gvl_twitter_unique over time in two hour windows
minutes <- 120
ggplot(data=gvl_twitter_unique, aes(x=created)) + 
  geom_histogram(aes(fill=..count..), binwidth=60*minutes) + 
  scale_x_datetime("created", date_breaks = "6 hours") + 
  scale_y_continuous("Frequency") +
  scale_color_fivethirtyeight("cyl") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
