library(tidyverse)
library(wordVectors)

glove_vectors <- read.vectors("../pretrained_wordvectors/fasttext/wiki-news-300d-1M-subword.vec", binary = F)
# glove_vectors <- read.vectors("../pretrained_wordvectors/GoogleNews-vectors-negative300.bin")

closest_words <- function(word, vector_space = glove_vectors, n = 10) {
  vectors <- vector_space[word, ]
  closest <- closest_to(vector_space, vectors, n = n + 1) %>%
    slice(-1)
  return(closest$word)
}

cosine_similarity <- function(word1, word2, vector_space = glove_vectors) {
  sim <- cosineSimilarity(glove_vectors[word1, ], glove_vectors[word2, ])
  return(as.double(sim))
}

pairsim <- function(word1, word2, n = 10) {
  word1_closest <- closest_words(word1, n = n)
  word2_closest <- closest_words(word2, n = n)
  
  neighbor_sim <- crossing(
    word1 = word1_closest,
    word2 = word2_closest
  ) %>%
    mutate(
      sim = map2_dbl(word1, word2, cosine_similarity)
    ) %>%
    pull(sim) %>%
    mean()
  
  return(neighbor_sim)
}


semantic_overlap <- function(word1, word2, n = 10, vector_space = fasttext) {
  
}


experiment_words <- c("propose", "suggest", "consider", "run", "walk")

experiment <- crossing(
  word1 = experiment_words,
  word2 = experiment_words
) %>%
  filter(word1 != word2) %>%
  mutate(
    overlap = map2_dbl(word1, word2, pairsim)
  ) 
