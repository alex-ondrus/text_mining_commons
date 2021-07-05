##### Sourcing scripts #####

source("mining_functions.R")
source("rtweet_auth.R") # Not included, implements rtweet::create_token()

##### Constructing Tweets #####

construct_who_tweet <- function(Parliament, Session, Sitting_Number){
  hansard_date <- get_hansard_date(Parliament, Session, Sitting_Number)
  hansard_data <- get_hansard_data(Parliament, Session, Sitting_Number)
  who_graph <- who_is_talking(hansard_data, hansard_date)
  ggsave("look.png", width = 6, height = 6, dpi = 400)
  tweet_text <- paste("Here's who had the most to say in the House of Commons on",
                      hansard_date)
  
  post_tweet(status = tweet_text,
             media = "look.png",
             token = token)
  return(who_graph)
}

construct_positive_tweet <- function(Parliament, Session, Sitting_Number){
  hansard_date <- get_hansard_date(Parliament, Session, Sitting_Number)
  hansard_data <- get_hansard_data(Parliament, Session, Sitting_Number)
  hansard_url <- hansard_url_constructor(Parliament, Session, Sitting_Number)
  extreme_sentiments <- extreme_paragraphs(named_hansard = hansard_data,
                                           hansard_date = hansard_date)
  
  positive_visual <- quote_visual(extreme_sentiments, "Positive")
  ggsave("positive.png", width = 6, height = 6, dpi = 400)
  
  tweet_text <- paste("This is the most 'positive' paragraph spoken in the House of Commons on ",
                      hansard_date,
                      ". Using the AFINN lexicon and methods from https://www.tidytextmining.com/sentiment.html. ",
                      "Data from ourcommons.ca",
                      " #cdnpoli",
                      sep = "")
  
  post_tweet(status = tweet_text,
             media = "positive.png",
             token = token)
}

construct_negative_tweet <- function(Parliament, Session, Sitting_Number){
  hansard_date <- get_hansard_date(Parliament, Session, Sitting_Number)
  hansard_data <- get_hansard_data(Parliament, Session, Sitting_Number)
  hansard_url <- hansard_url_constructor(Parliament, Session, Sitting_Number)
  extreme_sentiments <- extreme_paragraphs(named_hansard = hansard_data,
                                           hansard_date = hansard_date)
  
  negative_visual <- quote_visual(extreme_sentiments, "Negative")
  ggsave("negative.png", width = 6, height = 6, dpi = 400)
  
  tweet_text <- paste("This is the most 'negative' paragraph spoken in the House of Commons on ",
                      hansard_date,
                      ". Using the AFINN lexicon and methods from https://www.tidytextmining.com/sentiment.html. ",
                      "Data from ourcommons.ca #cdnpoli",
                      sep = "")
  
  post_tweet(status = tweet_text,
             media = "negative.png",
             token = token)
}

##### Iterating Construction #####

construct_several_tweets <- function(range_sitting_numbers, parl = 43, session = 2){
  for(i in range_sitting_numbers){
    construct_who_tweet(parl, session, i)
    construct_positive_tweet(parl, session, i)
    construct_negative_tweet(parl, session, i)
  }
}