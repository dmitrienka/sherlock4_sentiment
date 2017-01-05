library(dplyr)
library(readr)
library(tidytext)
library(lubridate)
library(ggplot2)
library(wordcloud)
library(tidyr)

strip.to.hours <- function(datetime){
    a <- date(datetime)
    hour(a) <- hour(datetime)
    as.POSIXct(a)
}

start.series <- as.POSIXct("2017-01-01 20:30:00", "UTC")
end.series <- as.POSIXct("2017-01-01 22:00:00", "UTC")

start.look <- as.POSIXct("2017-01-01 00:00:00", "UTC")

raw.tweets <- read_csv("sherlock4tweets.csv")

tidytweets <- raw.tweets %>%
    select(text, id, created) %>%
    unnest_tokens(word, text)

tweetscores.afinn <- tidytweets %>%
    left_join(get_sentiments("afinn"), by="word") %>%
    mutate(score = replace(score, is.na(score), 0)) %>%
    group_by(id, created) %>%
    summarise(score = sum(score))

tweetscores.bing <- tidytweets %>%
    left_join(get_sentiments("bing"), by="word") %>%
    mutate(sentiment = replace(sentiment, is.na(sentiment), "neutral")) %>%
    count(id, created, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(score = positive - negative)


tweetscores.nrc <- tidytweets %>%
    left_join(get_sentiments("nrc"), by="word") %>%
    mutate(sentiment = replace(sentiment, is.na(sentiment), "neutral")) %>%
    count(id, created, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(score = positive - negative)


ggplot(aes(created, positive - negative), data=tweetscores.nrc %>% filter(created > start.look)) +
    geom_jitter(width=0, height=0.45, alpha=0.05) +
    geom_vline(xintercept = as.numeric(end.series), linetype=4) +
    geom_vline(xintercept = as.numeric(start.series), linetype=4) +
    geom_smooth(aes(color="sentiment score")) +
    geom_smooth( aes(y=joy, color="joy")) +
    geom_smooth( aes(y=sadness, color="sadness")) +
    scale_colour_brewer("Smoothers:", palette=2, type="qual",
                        breaks = c("sentiment score", "sadness", "joy"),
                        labels = c("Sentiment score", "Sadness", "Joy")) +
    theme_bw() +
    ylim(c(-4,4)) + ylab("Sentiment score")
    


ggplot(tweetscores.afinn %>% filter(created > start.look),
       aes(created, score)) +
    geom_jitter(alpha=0.05, width=0, height=0.45) +
    geom_smooth() +
    geom_vline(xintercept = as.numeric(end.series), linetype=4) +
    geom_vline(xintercept = as.numeric(start.series), linetype=4) +
    theme_bw() + ylim(c(-10, 10)) 



tweetscores.afinn.hours <- tweetscores.afinn %>% mutate(hour = strip.to.hours(created)) %>% group_by(hour) %>% summarise(av.score = mean(score), n = n())

ggplot(data=tweetscores.afinn.hours[tweetscores.afinn.hours$hour > start.look,],
       aes(hour, av.score)) +
    geom_bar(stat="identity", fill="red", aes(alpha=log(n+1))) +
    theme_bw() + geom_vline(xintercept = as.numeric(end.series))

