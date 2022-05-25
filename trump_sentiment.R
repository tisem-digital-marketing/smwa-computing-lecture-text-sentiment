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


# --- Remove Stopwords --- #

# ---  What does Trump tweet about at different points in the day? --- #
# note: probably not covered in lecture due to time constraint

# --- Sentiment Analysis - AFINN --- #


# --- Sentiment Analysis - BING --- #

# --- Sentiment Analysis - NRC --- #
# Note: probably not covered in lecture due to time constraint

# --- Sentiment Analysis - VADER --- #


# --- Plotting Some Output ---- #
# How does Trump's weekly sentiment evolve over the early stages of 2020?
# I'll use the VADER outputs
