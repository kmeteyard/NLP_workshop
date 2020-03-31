library(dplyr)
library(magrittr)
library(tidytext)
library(stringr)
library(ggplot2)

# Example 4

# Load the star_wars_scripts.rds dataset
df <- readRDS("data/star_wars_scripts.rds")

# Use {tidytext} to tokenize the star wars scripts, where a token is a single 
# word to create a one-token-per-row data frame. Also remove summary columns.
tidy_script <- df %>%
  select(-length, -ncap, -nexcl, -nquest, -nword) %>% 
  unnest_tokens(output = word, input = dialogue)

# Remove the stop words from the data frame and create “tidy_script” object.
tidy_script <- tidy_script %>%
  anti_join(stop_words, by = "word")

# Use {tidytext} and create the data frame “afinn” of the AFINN sentiment lexicon and inspect it.
afinn <- get_sentiments("afinn")

# Inner join the AFINN sentiment lexicon to tidy_script from Example 2
# and calculate the total sentiment per movie per line
sentiment_script <- tidy_script %>% 
  inner_join(afinn) %>% 
  group_by(movie, line) %>% 
  mutate(sentiment = sum(value)) %>% 
  ungroup() %>% 
  select(-word, -value) %>% 
  distinct()

# Attach the sentiment scores to the original star wars script dataset. 
# What do you think is the most negative script line from all movies?
df <- df %>% 
  inner_join(sentiment_script)

# Who is the most negative character of all movies?
res <- df %>% 
  group_by(character) %>% 
  summarise(total_sentiment = sum(sentiment)) %>% 
  ungroup() %>% 
  arrange(total_sentiment)

# Visualise the sentiment score changes line by line for each movie
df %>%
  ggplot(aes(line, sentiment, fill = movie)) +
  geom_col(show.legend = FALSE) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~movie, ncol = 1)
