library(dplyr)
library(magrittr)
library(tidytext)
library(tidyr)
library(irlba)
library(broom)

# Example 6

# Load the star_wars_scripts.rds dataset
df <- readRDS("data/star_wars_scripts.rds")

# Use {tidytext} to tokenize the star wars scripts, where a token is a single # word to create a one-token-per-row data frame. Also remove summary columns.
tidy_script <- df %>%
  select(-length, -ncap, -nexcl, -nquest, -nword) %>% # Remove summary cols
  unnest_tokens(output = word, input = dialogue) # Tokenise

#Create unigram probabilities
unigram_probs <- tidy_script %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

unigram_probs

library(widyr)

#Create skipgrams for 8-grams
tidy_skipgrams <- df %>%
  #Here we'll ignore the films
  mutate(lineID = row_number()) %>% 
  unnest_tokens(ngram, dialogue, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  unite(skipgramID, lineID, ngramID) %>%
  unnest_tokens(word, ngram) %>% 
  select(skipgramID, word)

tidy_skipgrams

#Skipgram probabilities
skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

#Normalise the probabilities wrt word occurance
normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2) %>% 
  drop_na()

#Words most associated with jedi
normalized_prob %>% 
  filter(word1 == "jedi") %>%
  arrange(-p_together)

#Cast to a sparse matrix
pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

class(pmi_matrix)

library(irlba)

#Decompose the matrix
pmi_svd <- irlba(pmi_matrix, 128, maxit = 1e3)

#Extract word vectors
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

#Helper function to view synonyms from a selected word
library(broom)
search_synonyms <- function(word_vectors, selected_vector) {
  
  similarities <- word_vectors %*% selected_vector %>%
    tidy() %>%
    as_tibble() %>%
    rename(token = .rownames,
           similarity = unrowname.x.)
  
  similarities %>%
    arrange(-similarity)    
}

jedi <- search_synonyms(word_vectors, word_vectors["jedi",])
jedi

chewie <- search_synonyms(word_vectors, word_vectors["luke",])
chewie

jedi %>%
  mutate(selected = "jedi") %>%
  bind_rows(chewie %>%
              mutate(selected = "chewie")) %>%
  group_by(selected) %>%
  top_n(15, similarity) %>%
  ungroup %>%
  mutate(token = reorder(token, similarity)) %>%
  ggplot(aes(token, similarity, fill = selected)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~selected, scales = "free") +
  coord_flip() +
  theme(strip.text=element_text(hjust=0, family="Roboto-Bold", size=12)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, title = "What word vectors are most similar to chewie and jedi?")

#Find an example - not too good on such a small corpus
mystery_product <- word_vectors["good",] - word_vectors["luke",] + word_vectors["vader",]
search_synonyms(word_vectors, mystery_product)

#Let's do it in 2-dimensional space to view the words
pmi_svd <- irlba(pmi_matrix, 2, maxit = 500)

#next we output the word vectors:
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

#grab 25 words
forplot <- as.data.frame(word_vectors[100:125,])
forplot$word <- rownames(forplot)

#now plot - again not too clear for such a small corpus
library(ggplot2)
ggplot(forplot, aes(x=V1, y=V2, label=word)) +
  geom_text(aes(label=word), hjust=0, vjust=0, color="blue") +
  theme_minimal() +
  xlab("First Dimension Created by SVD") +
  ylab("Second Dimension Created by SVD")
