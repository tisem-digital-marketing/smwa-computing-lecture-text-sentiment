#' trump_sentiment.R
#'
#' Analyse Sentiment in Trumps' tweets as President in Early 2020
#'

# --- Library --- #
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(tidytext)
library(vader)
library(lubridate)
library(tokenizers)
library(textdata)
library(yardstick)
library(ggplot2)

# --- read data --- #

tweets <- read_csv('data/trump_early_2020_tweets.csv')

# add calender week to data
tweets <-
    tweets %>%
    mutate(cal_week = week(date_est))

# --- Clean Twitter Junk --- #

tweets <-
    tweets %>%
    mutate(
        # remove links
        text = str_remove_all(text, "https\\S*"),
        text = str_remove_all(text, "http\\S*"),
        text = str_remove_all(text, "t.co*"),
        # remove mentions
        text = str_remove_all(text, "@\\S*"),
        # remove annoying html stuff
        text = str_remove_all(text, "amp"),
        text = str_remove_all(text, "&S*"),
        text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
        text = str_replace_all(text, "<a(.*?)>", " "),
        text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
        text = str_replace_all(text, "&#[:digit:]+;", " "),
        text = str_remove_all(text, "<[^>]*>"),
        # remove numbers
        text = str_remove_all(text, "[:digit:]"),
        # remove excess whitespace
        text = str_squish(text),
        text = str_trim(text),
        # remove RT for retweets -- keeping retweets in the data
        text = str_remove_all(text, "RT")
    ) %>%
    filter(count_words(text) > 1)

# --- Tweets to Tidy Tweets --- #
tidy_tweets <-
    tweets %>%
    unnest_tokens(word, text)

# --- Remove Stopwords --- #
tidy_tweets <-
    tidy_tweets %>%
    anti_join(stop_words)
    
# --- Sentiment Analysis - AFINN --- #
get_sentiments("afinn") %>%
    filter(value == 1)

# add sentiment of word back to the data
tidy_afinn <-
    tidy_tweets %>%
    left_join(get_sentiments("afinn"))

# go from word level sentiment to tweet level sentiment
sentiment_afinn <- 
    tidy_afinn %>%
    group_by(id) %>%
    summarise(score = sum(value, na.rm = TRUE)) %>%
    #mutate(id = as.numeric(id)) %>%
    #arrange(id) %>%
    ungroup() %>%
    mutate(sentiment_afinn = case_when(
        score > 0 ~ "positive",
        score < 0 ~ "negative",
        TRUE ~ "neutral"
        )
    ) %>%
    select(-score)

tweets <-
    tweets %>%
    left_join(sentiment_afinn)


# --- Sentiment Analysis - BING --- #
get_sentiments("bing")

tidy_bing <-
    tidy_tweets %>%
    left_join(get_sentiments("bing"))

# from word level to tweet level
sentiment_bing <-
    tidy_bing %>%
    group_by(id, sentiment) %>%
    count()

# go from long and skinny to wider
sentiment_bing2 <-
    sentiment_bing %>%
    filter(!is.na(sentiment)) %>%
    pivot_wider(names_from = sentiment, 
                values_from = n, 
                values_fill = 0
                ) %>%
    mutate(sentiment_bing = case_when(
        positive > negative  ~ "positive",
        negative > positive ~ "negative",
        TRUE ~ "neutral"
        )
    ) %>%
    select(-positive, -negative)

tweets <-
    tweets %>%
    left_join(sentiment_bing2) %>%
    mutate(sentiment_bing = if_else(is.na(sentiment_bing), "neutral", sentiment_bing))

# can we compare the two sentiment lexicon outputs?
# first need sentiment variables as factor
# IN the dataset
tweets <-
    tweets %>%
    mutate(sentiment_bing = as.factor(sentiment_bing),
           sentiment_afinn = as.factor(sentiment_afinn)
           )

conf_mat(tweets,
         sentiment_bing,
         sentiment_afinn,
         dnn = c('bing', 'afinn')
         )

accuracy(tweets, 
         sentiment_bing, 
         sentiment_afinn
    )

# --- Sentiment Analysis - VADER --- #
# VADER does not want a tidy df .. it wants the raw text string
# so we are gonna have different text strings

# Vader: polarity, intensity, emoticons, capitalization, punctuation and 'social media terms'
# These features include:
# A full list of Western-style emoticons ( for example - :D and :P )
# Sentiment-related acronyms ( for example - LOL and ROFL )
# Commonly used slang with sentiment value ( for example - Nah and meh )

# Five Heuristics of VADER:
#     
# Punctuation, namely the exclamation point (!), increases the magnitude of the intensity without modifying the semantic orientation. For example: “The weather is hot!!!” is more intense than “The weather is hot.”
# Capitalization, specifically using ALL-CAPS to emphasize a sentiment-relevant word in the presence of other non-capitalized words, increases the magnitude of the sentiment intensity without affecting the semantic orientation. For example: “The weather is HOT.” conveys more intensity than “The weather is hot.”
# Degree modifiers (also called intensifiers, booster words, or degree adverbs) impact sentiment intensity by either increasing or decreasing the intensity. For example: “The weather is extremely hot.” is more intense than “The weather is hot.”, whereas “The weather is slightly hot.” reduces the intensity.
# Polarity shift due to Conjunctions, The contrastive conjunction “but” signals a shift in sentiment polarity, with the sentiment of the text following the conjunction being dominant. For example: “The weather is hot, but it is bearable.” has mixed sentiment, with the latter half dictating the overall rating.
# Catching Polarity Negation, By examining the contiguous sequence of 3 items preceding a sentiment-laden lexical feature, we catch nearly 90% of cases where negation flips the polarity of the text. For example a negated sentence would be “The weather isn't really that hot.”.

# as a result VADER outperforms humans in correctly labelling sentiment 
# assigned by another human coder 
# accuracy of .96 vs .84
#
vader <- 
    tweets %>%
    select(text) %>%
    vader_df()

# from compound score to classification
# 0.05 is the threshold suggested in the paper
vader <-
    vader %>%
    mutate(
        sentiment_vader = case_when(
            compound > 0.05 ~ "positive",
            compound < -0.05 ~ "negative",
            TRUE ~ "neutral"
        )
    ) %>%
    select(sentiment_vader) %>%
    rownames_to_column("id") %>%
    mutate(id = str_remove(id, "[a-z]+"),
           sentiment_vader = as.factor(sentiment_vader)
           )

tweets <-
    tweets %>%
    inner_join(vader)

# compare models
conf_mat(tweets,
         sentiment_bing,
         sentiment_vader,
         dnn = c('bing', 'vader')
)

accuracy(tweets, 
         sentiment_bing, 
         sentiment_vader
)

write_csv(tweets, "data/tweets_sentiment.csv")

# --- Plotting Some Output ---- #
# How does Trump's weekly sentiment evolve over the early stages of 2020?
# I'll use the VADER outputs

weekly_sent <-
    tweets %>%
    group_by(cal_week, sentiment_vader, business_hours) %>%
    count() %>%
    ungroup() %>%
    pivot_wider(names_from = sentiment_vader, values_from = n) %>%
    mutate(pos_neg_ratio = positive / negative)

weekly_sent %>%
    ggplot(aes(x = cal_week, 
               y = pos_neg_ratio,
               color = business_hours)
    ) +
    geom_line() +
    theme_bw()
