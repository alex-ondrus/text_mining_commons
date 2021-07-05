##### Load Libraries #####

library(tidyverse)
library(xml2)
library(tidytext)
library(textdata)
library(ggthemes)
library(rtweet)
library(ggfittext)

##### Storing Values #####

parties <- c("BQ", "CPC", "GP", "Lib.", "NDP")
colours <- c("#87CEFA", "#6495ED","#3D9B35", "#EA6D6A","#F4A460")
party_colours <- tibble(Party = parties, Colour = colours) %>% 
  arrange(Party)
CurrentMembers <- read_csv("../data/CurrentMembers.csv")

afinn_sentiment <- get_sentiments("afinn")
bing_sentiment <- get_sentiments("bing")

##### Reading Web Data #####

# Constructing the URL for a given session. Sitting number must be in quotations
# if it begins with a 0

hansard_url_constructor <- function(Parliament, Session, Sitting_Number){
  hansard_url <- paste("https://www.ourcommons.ca/Content/House/", 
                       Parliament,
                       Session,
                       "/Debates/",
                       Sitting_Number,
                       "/HAN",
                       Sitting_Number,
                       "-E.XML",
                       sep = "") %>% 
    url()
  return(hansard_url)
}

# Given a tag of type ParaText, this function finds the speaker if there is one
# Note that this assumes that the speaker is the first sibling to the parent of the
# ParaText tag that is of PersonSpeaking type. Returns NA if no such tag exists

find_speaker <- function(ParaTextTag){
  SpeakerTag <- xml_find_first(ParaTextTag, 
                               "parent::node()/parent::node()/child::PersonSpeaking")
  return(SpeakerTag %>% xml_text())
}

# The following function determines if any of the mentions in a paragraph refer to
# the prime minister (PM) or other ministers/secretaries (MinisterOrSecretary)

find_mention_tags <- function(ParaText_tag){
  mention_tags <- xml_find_all(ParaText_tag,
                               "child::Affiliation")
}

mention_PM <- function(ParaText_tag){
  mention_types <- find_mention_tags(ParaText_tag) %>% 
    xml_attr(attr = "Type")
  return("1" %in% mention_types)
}

mention_MinisterOrSecretary <- function(ParaText_tag){
  mention_types <- find_mention_tags(ParaText_tag) %>% 
    xml_attr(attr = "Type")
  return(("18" %in% mention_types) | ("4" %in% mention_types))
}

mentioned_MPs <- function(ParaText_tag){
  mention_tags <- find_mention_tags(ParaText_tag)
  MPs_df <- tibble(
    Type = sapply(mention_tags, xml_attr, attr = "Type"),
    Text = sapply(mention_tags, xml_text)
  )
  
  if(dim(MPs_df)[1] == 0) {
    return(NA)
  } else {
    MPs_df <- MPs_df %>% 
      left_join(CurrentMembers, by = c("Text" = "Constituency")) %>% 
      filter(Type == "2")
    return(MPs_df)
  }
}

mention_MP_Parties <- function(ParaText_tag){
  MPs_df <- mentioned_MPs(ParaText_tag)
  if(is.null(dim(MPs_df))) {
    return(NA)
  } else {
    return(MPs_df$`Political Affiliation` %>% 
             unique() %>% 
             paste(collapse = ", "))
  }
}

# Finds all of the ParaText tags, finds the speaker for each ParaText tag by applying
# the find_speaker function to each using sapply, finds the members mentioned in each
# ParaText tag (concatenating them if there are multiple), and then creates a data frame
# with the following columns:

# Speaker = text from the PersonSpeaking tag associated to the ParaText tag
# ParagraphText = text from the ParaText tag
# Mentions = text from the Affiliation tags contained within the ParaText tag

parse_hansard_xml <- function(hansard_xml){
  ParaText_Nodes <- xml_find_all(hansard_xml, 
                                 "//ParaText")
  ParaText_Text <- sapply(ParaText_Nodes, 
                          xml_text)
  ParaText_Speakers <- sapply(ParaText_Nodes, 
                              find_speaker)
  ParaText_Mentions <- sapply(ParaText_Nodes, 
                              function(x){
                                xml_find_all(x, "child::Affiliation") %>%
                                  xml_text() %>% 
                                  paste(collapse = ", ")
                              }
  )
  
  ParaText_Mentions_PM <- sapply(ParaText_Nodes,
                                 mention_PM)
  
  ParaText_Mention_Minister_Secretary <- sapply(ParaText_Nodes,
                                                mention_MinisterOrSecretary)
  
  ParaText_Mention_MP_Parties <- sapply(ParaText_Nodes,
                                        mention_MP_Parties)
  
  return_df <- tibble(
    Speaker = ParaText_Speakers,
    ParagraphText = ParaText_Text,
    MentionText = ParaText_Mentions,
    MentionsPM = ParaText_Mentions_PM,
    MentionsCabinetOrSecretary = ParaText_Mention_Minister_Secretary,
    MentionsMPParties = ParaText_Mention_MP_Parties
  ) %>% 
    filter(!is.na(Speaker)) %>% 
    mutate(Speaker = str_replace(Speaker, ":", "") %>% str_trim(),
           MentionsMPParties = na_if(MentionsMPParties, ""))
  
  return(return_df)
}

get_hansard_date <- function(Parliament, Session, Sitting_Number){
  hansard_url <- hansard_url_constructor(Parliament, Session, Sitting_Number)
  hansard_xml <- read_xml(hansard_url)
  hansard_date <- xml_find_first(hansard_xml,
                                 "//ExtractedItem[@Name='Date']") %>% 
    xml_text()
  
  return(hansard_date)
}

# Cleaning up names
# First, I build a table of all MPs separating out their names, ridings, and parties

split_name_using_brackets <- function(names){
  bracket_pattern <- "([^[\\(]]+)(\\(.+\\))"
  split_name <- str_match_all(names, 
                              bracket_pattern)
  return(split_name)
}

# Implements https://stackoverflow.com/questions/4227223/convert-a-list-to-a-data-frame
# to generate the dataframe from the list

build_mp_lookup <- function(names_column){
  unique_names <- unique(names_column)
  list_of_name_parts <- split_name_using_brackets(unique_names) %>% 
    compact()
  
  parts_df <- data.frame(matrix(unlist(list_of_name_parts),
                                nrow=length(list_of_name_parts),
                                byrow=TRUE),
                         stringsAsFactors = FALSE)
  colnames(parts_df) <- c("OriginalSpeaker", "Name","InBrackets")
  
  parts_df$Name <- str_trim(parts_df$Name)
  
  end_bracket_pattern <- ", [A-z.]+\\)"
  
  PartyStarts <- str_locate(parts_df$InBrackets,
                            end_bracket_pattern)
  
  parts_df <- cbind(parts_df, PartyStarts)
  
  return_df <- parts_df %>% 
    mutate(Riding_or_Role = str_sub(InBrackets, 2, start - 1),
           Party = str_sub(InBrackets, start+2, -2)) %>% 
    select(OriginalSpeaker, Name, Riding_or_Role, Party) %>% 
    filter(!is.na(Party))
  
  return(return_df)
}

# The output of this function removes all paragraphs by the speaker of the house
# this is done intentionally, as their role is meant to be impartial and not represent
# the agenda of any given party (which is the purpose of this analysis)

attach_mp_names <- function(parsed_hansard){
  mp_lookup <- build_mp_lookup(parsed_hansard$Speaker)
  parsed_hansard$Speaker <- str_replace(parsed_hansard$Speaker, " \\(.+", "")
  hansard_w_names <- left_join(parsed_hansard,
                               mp_lookup,
                               by = c("Speaker" = "Name")) %>% 
    rename("SpeakerRidingRole" = "Riding_or_Role",
           "SpeakerParty" = "Party") %>% 
    mutate(MentionsMPParties = recode(MentionsMPParties,
                                      "Liberal" = "Lib.",
                                      "Conservative" = "CPC",
                                      "Green Party" = "GP",
                                      "Bloc Québécois" = "BQ",
                                      "Independent" = "Ind.")) %>% 
    drop_na(SpeakerParty) %>% # This drops all paragraphs spoken in the role of speaker
    select(-OriginalSpeaker)
}

# The following wrapper puts it all together, generating the URL, obtaining the
# XML, parsing the XML, attaching the names, and returning a data frame

get_hansard_data <- function(Parliament, Session, Sitting_Number){
  hansard_xml <- read_xml(hansard_url_constructor(Parliament, Session, Sitting_Number))
  parsed_hansard <- parse_hansard_xml(hansard_xml)
  return(attach_mp_names(parsed_hansard))
}