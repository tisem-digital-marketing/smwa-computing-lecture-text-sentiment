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
    filter(count_words(text) > 1) %>%
    rownames_to_column("id") %>%
    select(-text_id)

# --- Tweets to Tidy Tweets --- #

tidy_tweets <-
    tweets %>%
    unnest_tokens(word, text, token = "tweets")

# --- Remove Stopwords --- #
tidy_tweets <-
    tidy_tweets %>%
    anti_join(stop_words)

# ---  What does Trump tweet about at different points in the day? --- #
# note: probably not covered in lecture due to time constraint
# count occurence of words by when the occur
# keep words that only occur > 10 times
word_occurence <- 
    tidy_tweets %>%
    count(word, business_hours) %>%
    group_by(word) %>%
    filter(sum(n) > 10) %>%
    ungroup() %>%
    pivot_wider(names_from = business_hours,
                values_from = n,
                values_fill = 0) %>%
    janitor::clean_names()

# compute log odds ratio
word_ratios <-
    word_occurence %>%
    mutate_if(is.numeric, list(~(. + 1) / sum(.)  + 1)) %>%
    mutate(logratio = log(business_hours / non_business_hours))

# plot the result using 15 most unique words per group
word_ratios %>%
    group_by(logratio < 0) %>%
    slice_max(abs(logratio), n = 15) %>% 
    ungroup() %>%
    mutate(word = reorder(word, logratio)) %>%
    ggplot(aes(word, logratio, fill = logratio < 0)) +
    geom_col() +
    coord_flip() +
    ylab("log odds ratio (business hours/ non business hours)") +
    scale_fill_discrete(name = "", labels = c("business hours", "non business hours"))

# --- Sentiment Analysis - AFINN --- #

get_sentiments("afinn")

# positive
get_sentiments("afinn") %>%
    filter(value == 4)

# negative
get_sentiments("afinn") %>%
    filter(value == -4)

tidy_afinn <-
    tidy_tweets %>%
    left_join(get_sentiments("afinn"))
    # inner_join(get_sentiments("afinn"))

tweet_sentiment_afinn <-
    tidy_afinn %>%
    group_by(id) %>%
    summarise(score = sum(value, na.rm = TRUE),
              score2 = mean(value, na.rm = TRUE)) %>%
    mutate(id = as.numeric(id),
           score2 = replace_na(score2, 0)) %>%
    arrange(id) %>%
    mutate(sentiment_class = case_when(
        score > 0 ~ "positive",
        score < 0 ~ "negative",
        TRUE ~ "neutral"
        ),
        sentiment_class2 = case_when(
            score2 > 0 ~ "positive",
            score2 < 0 ~ "negative",
            TRUE ~ "neutral"
        )
    )

# do our two measures align?

conf_mat(tweet_sentiment_afinn, 
         sentiment_class, 
         sentiment_class2,
         dnn = c("sum", "avg"))

accuracy(tweet_sentiment_afinn, 
         as.factor(sentiment_class), 
         as.factor(sentiment_class2))

# alt measure 3: treat count number of positive and negative words, base score on which is higher


# --- Sentiment Analysis - BING --- #
get_sentiments("bing") %>% filter(sentiment == "positive")

tidy_bing <-
    tidy_tweets %>%
    left_join(get_sentiments("bing"))

# create a count of positive and negative words per tweet

# do this first
tidy_bing_sentiment <-
    tidy_bing %>%
    filter(sentiment != "NA") %>%
    count(sentiment, id) %>%
    mutate(id = as.numeric(id)) %>%
    arrange(id)

tweet_sentiment_bing <- 
    tidy_bing_sentiment %>%
    pivot_wider(
        names_from = sentiment,
        values_from = n,
        values_fill = 0
    ) %>%
    mutate(sentiment_bing = case_when(
        positive > negative ~ "positive",
        positive < negative ~ "negative",
        TRUE ~ "neutral"
        )
    ) %>%
    mutate(id = as.character(id))

# what do we do with the tweets with no sentiment laden words?
# bring them back in and assign zero

tweet_sentiment_bing <- 
    tweets %>%
    select(id) %>%
    left_join(tweet_sentiment_bing, by = "id") %>%
    mutate(sentiment_bing = replace_na(sentiment_bing, "neutral"))

# compare afinn and bing?

comparison_df <-
    tweet_sentiment_afinn %>%
    select(id, sentiment_class) %>%
    mutate(id = as.character(id)) %>%
    inner_join(tweet_sentiment_bing, by = "id")

conf_mat(comparison_df, 
         sentiment_class, 
         sentiment_bing,
         dnn = c("afinn", "bing"))

accuracy(comparison_df, 
         as.factor(sentiment_class), 
         as.factor(sentiment_bing)) 

# --- Sentiment Analysis - NRC --- #
# Note: probably not covered in lecture due to time constraint
get_sentiments("nrc")

# note theres not just +ve and -ve here... i personally dont find the other classifications useful though
tidy_nrc <-
    tidy_tweets %>%
    left_join(get_sentiments("nrc")) %>%
    filter(sentiment %in% c('positive', 'negative'))

# count # +ve and -ve per tweet
tidy_nrc_sentiment <-
    tidy_nrc %>%
    filter(sentiment != "NA") %>%
    count(sentiment, id) %>%
    mutate(id = as.numeric(id)) %>%
    arrange(id)

# cast wide, compute sentiemnt
tweet_sentiment_nrc <- 
    tidy_nrc_sentiment %>%
    pivot_wider(
        names_from = sentiment,
        values_from = n,
        values_fill = 0
    ) %>%
    mutate(sentiment_nrc = case_when(
        positive > negative ~ "positive",
        positive < negative ~ "negative",
        TRUE ~ "neutral"
    )
    ) %>%
    mutate(id = as.character(id))

# merge back in tweets that had no sentiment laden words
# classify as neutral
tweet_sentiment_nrc <- 
    tweets %>%
    select(id) %>%
    left_join(tweet_sentiment_nrc, by = "id") %>%
    mutate(sentiment_nrc = replace_na(sentiment_nrc, "neutral")) %>%
    select(id, sentiment_nrc)

# add to comparison df so we can compare output
comparison_df <-
    comparison_df %>%
    inner_join(tweet_sentiment_nrc, by = "id")

# do some comparisons ...
# afinn and nrc
conf_mat(comparison_df, 
         sentiment_class, 
         sentiment_nrc,
         dnn = c("afinn", "nrc"))

accuracy(comparison_df, 
         as.factor(sentiment_class), 
         as.factor(sentiment_nrc)) 

# nrc and bing
conf_mat(comparison_df, 
         sentiment_bing, 
         sentiment_nrc,
         dnn = c("bing", "nrc"))

accuracy(comparison_df, 
         as.factor(sentiment_bing), 
         as.factor(sentiment_nrc)) 

# note there's even less agreement between NRC and the other lexicons ...

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

# Let's do it:

vader_sents <- 
    vader_df(tweets$text)


# how to get sentiment from this:
# focus on the compound score which is a normalization of the sum of word sentiments 
# so the output lies between -1 and 1
# cutting into pos neg neu using the threshold suggested by original authors
vader_sents2 <- 
    vader_sents %>%
    rowid_to_column("id") %>%
    filter(word_scores != 'ERROR') %>%
    mutate(vader_class = case_when(
        compound < -0.05 ~ "negative",
        compound > 0.05 ~ "positive",
        TRUE ~ "neutral"
    )
    ) %>% 
    select(id, vader_class) %>%
    mutate(id = as.character(id))

# Add back to comparison df:
comparison_df <-
    comparison_df %>%
    inner_join(vader_sents2, by = "id")

# do some comparisons
# afinn and vader

conf_mat(comparison_df, 
         sentiment_class, 
         vader_class,
         dnn = c("afinn", "vader"))

accuracy(comparison_df, 
         as.factor(sentiment_class), 
         as.factor(vader_class)) 

# bing and vader

conf_mat(comparison_df, 
         sentiment_bing, 
         vader_class,
         dnn = c("bing", "vader"))

accuracy(comparison_df, 
         as.factor(sentiment_bing), 
         as.factor(vader_class)) 

# nrc and vader

conf_mat(comparison_df, 
         sentiment_nrc, 
         vader_class,
         dnn = c("nrc", "vader"))

accuracy(comparison_df, 
         as.factor(sentiment_nrc), 
         as.factor(vader_class)) 

# --- Plotting Some Output ---- #
# How does Trump's weekly sentiment evolve over the early stages of 2020?
# I'll use the VADER outputs

weekly_sent <-
    tweets %>%
    inner_join(vader_sents2) %>%
    group_by(cal_week, vader_class, business_hours) %>%
    count() %>%
    ungroup() %>%
    pivot_wider(names_from = vader_class, values_from = n) %>%
    mutate(pos_neg_ratio = positive / negative)

weekly_sent %>%
    ggplot(aes(x = cal_week, 
               y = pos_neg_ratio,
               color = business_hours)
    ) +
    geom_line() +
    theme_bw()

# takeaway: tweets are angrier in non-business hours!