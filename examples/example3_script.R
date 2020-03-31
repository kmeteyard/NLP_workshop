library(dplyr)
library(magrittr)
library(tidytext)
library(textstem)

# Example 4

# Load the star_wars_scripts.rds dataset
df <- readRDS("data/star_wars_scripts.rds")

# Use {tidytext} to tokenize the star wars scripts, where a token is a single 
# word to create a one-token-per-row data frame. Also remove summary columns.
tidy_script <- df %>%
  select(-length, -ncap, -nexcl, -nquest, -nword) %>% # Remove summary cols
  unnest_tokens(output = word, input = dialogue) # Tokenise

#Stemming
tidy_script_stemmed <- tidy_script %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = stem_strings(word)) %>%
  count(word, sort = TRUE)

#Lemmatisation
tidy_script_lemma <- tidy_script %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = lemmatize_strings(word)) %>%
  count(word, sort = TRUE)

