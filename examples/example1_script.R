library(dplyr)
library(magrittr)

# Example 1

# Load the star_wars_scripts.rds dataset
df <- readRDS("data/star_wars_scripts.rds")

# Which movie has the most lines?
df %>% 
  group_by(movie) %>% 
  summarise(line_count = n())

# Which movie has the most characters?
df %>% 
  group_by(movie) %>% 
  summarise(character_count = n_distinct(character))

# Summarise the lines, exclamations, questions, words per character per movie and sort by words (descending).
res <- df %>% 
  group_by(movie, character) %>% 
  summarise(line_count = n(),
            total_excl = sum(nexcl),
            total_quest = sum(nquest),
            total_words = sum(nword)) %>% 
  arrange(desc(total_words))



