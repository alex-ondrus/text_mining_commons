##### Sourcing data functions #####

source("web_data_parsing.R")

##### Text Mining Functions #####

# Used for pulling the most 'positive' and 'negative' paragraphs
# using AFINN sentiment scores

extreme_paragraphs <- function(named_hansard, hansard_date){
  tidy_w_paragraphs <- named_hansard %>% 
    mutate(ParagraphNumber = row_number(),
           ParagraphLength = nchar(ParagraphText)) %>% 
    select(Speaker, 
           ParagraphText, 
           SpeakerRidingRole, 
           SpeakerParty, 
           ParagraphNumber,
           ParagraphLength) 
  
  paragraph_sentiments <- tidy_w_paragraphs %>% 
    unnest_tokens(word,
                  ParagraphText) %>% 
    inner_join(afinn_sentiment) %>% 
    group_by(ParagraphNumber) %>% 
    summarise(TotalSentiment = sum(value),
              .groups = "drop") %>% 
    left_join(tidy_w_paragraphs, by = "ParagraphNumber")
  
  negative_sentiment <- filter(paragraph_sentiments,
                               TotalSentiment == min(TotalSentiment)) %>% 
    filter(ParagraphLength == max(ParagraphLength)) %>% 
    mutate(Positive_or_Negative = "Negative")
  
  positive_sentiment <- filter(paragraph_sentiments,
                               TotalSentiment == max(TotalSentiment)) %>% 
    filter(ParagraphLength == max(ParagraphLength)) %>% 
    mutate(Positive_or_Negative = "Positive")
  
  return_df <- rbind(negative_sentiment, positive_sentiment) %>% 
    mutate(Date = hansard_date)
  
  return(return_df)
}


##### Generating Visuals #####

# Generates simple bar graph of who has spoken most in commons during a given
# session

who_is_talking <- function(named_hansard,
                           hansard_date){
  who_data <- named_hansard %>% 
    unnest_tokens(word,
                  ParagraphText) %>% 
    group_by(Speaker,
             SpeakerParty) %>% 
    summarise(NumWords = n(),
              .groups = "drop") %>% 
    arrange(desc(NumWords)) %>% 
    mutate(Rank = row_number()) %>% 
    filter(Rank <= 10)
  
  parties_included <- unique(who_data$SpeakerParty)
  colours_included <- filter(party_colours, Party %in% parties_included)$Colour
  
  who_graph <- ggplot(who_data,
                      aes(x = NumWords,
                          y = reorder(Speaker, NumWords),
                          fill = SpeakerParty)) +
    geom_bar(stat = "identity") +
    labs(title = "Look Who's Talking Now",
         subtitle = paste("Number of words spoken (top 10)",
                          hansard_date,
                          sep = "\n"),
         x = NULL,
         y = NULL,
         fill = NULL,
         caption = "Data: ourcommons.ca\nGraph: @DrAOndrus") +
    scale_fill_manual(values = colours_included) +
    theme_fivethirtyeight() +
    theme(plot.background = element_rect(fill = NA),
          panel.background = element_rect(fill = NA),
          legend.background = element_rect(fill = NA),
          panel.grid.major.y = element_blank(),
          panel.ontop = TRUE,
          axis.text.y.left = element_text(margin = margin(0,-10,0,0)),
          plot.title.position = "panel") +
    xlim(0,NA)
  
  return(who_graph)
}

# Formats the most 'positive' or 'negative' paragraphs for display on twitter

quote_visual <- function(extreme_sentiment, pos_or_neg){
  quote_row <- filter(extreme_sentiment,
                      Positive_or_Negative == pos_or_neg)
  
  # The quote data is the first row, the attribution is the second
  
  quote_data <- tibble(
    x_start = c(0, 0.25),
    x_end = c(1, 1),
    y_start = c(0.1, 0),
    y_end = c(1, 0.1),
    Text = c(trimws(quote_row$ParagraphText),
             paste(quote_row$Speaker, quote_row$SpeakerRidingRole, quote_row$SpeakerParty, quote_row$Date,
                   sep = ", "))
  )
  
  quote_gg <- ggplot(quote_data,
                     aes(xmin = x_start,
                         xmax = x_end,
                         ymin = y_start,
                         ymax = y_end,
                         label = Text)) +
    geom_fit_text(reflow = TRUE,
                  grow = TRUE,
                  place = "topleft") +
    theme_void() +
    coord_equal()
  
  return(quote_gg)
}