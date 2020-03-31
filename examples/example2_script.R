library(dplyr)
library(magrittr)
library(tidytext)

# Example 2

# Load the star_wars_scripts.rds dataset
df <- readRDS("data/star_wars_scripts.rds")

# Use {tidytext} to tokenize the star wars scripts, where a token is a single # word to create a one-token-per-row data frame. Also remove summary columns.
tidy_script <- df %>%
  select(-length, -ncap, -nexcl, -nquest, -nword) %>% # Remove summary cols
  unnest_tokens(output = word, input = dialogue) # Tokenise

# Remove the stop words from the data frame and create “tidy_script” object.
tidy_script <- tidy_script %>%
  anti_join(stop_words, by = "word")

# Find the top 5 words for all movies and create a bar chart visualisation.
library(ggplot2)

tidy_script %>%
  count(word, movie) %>%
  ungroup() %>% 
  group_by(movie) %>%
  top_n(5) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = movie)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL, x = NULL) +
  facet_wrap(~movie, ncol = 3, scales = "free_y") +
  coord_flip()

# Find the most common word used for all the characters. What do you think is Yoda's?
res <- tidy_script %>%
  count(word, character) %>%
  ungroup() %>% 
  group_by(character) %>%
  top_n(1) %>%
  ungroup() %>% 
  arrange(desc(n))

# Create an awesome word cloud!

# devtools::install_github("lchiffon/wordcloud2") # Might require some package installation steps
library(wordcloud2)

plot_data <- tidy_script %>%
  count(word) %>%
  ungroup() %>% 
  mutate(word = factor(word),
         freq = as.numeric(n)) %>% 
  arrange(desc(freq))

wordcloud2(plot_data, size = 1, figPath="data/vader.png")

wordcloud2(plot_data, size = 1, figPath="data/yoda.png")
