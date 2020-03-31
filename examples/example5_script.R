library(dplyr)
library(magrittr)
library(tidytext)
library(stringr)
library(ggplot2)

# Example 5

# Load the star_wars_scripts.rds dataset
df <- readRDS("data/star_wars_scripts.rds")

# Use {tidytext} to tokenize the star wars scripts, where a token is a single
# word to create a one-token-per-row data frame. Also remove summary columns.
# Then count the frequency of each word for each move and apply the 
# TF-IDF function of {tidytext} and extract the top 10 words per movie
tf_idf_script <- df %>%
  select(-length, -ncap, -nexcl, -nquest, -nword) %>% 
  unnest_tokens(output = word, input = dialogue) %>% 
  count(movie, word, sort = TRUE) %>%
  bind_tf_idf(word, movie, n) %>%
  ungroup() %>% 
  group_by(movie) %>%
  top_n(10) %>% 
  arrange(movie, desc(tf_idf))

