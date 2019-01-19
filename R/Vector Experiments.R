library(tidyverse)
library(wordVectors)

glove_vectors <- read.vectors("../pretrained_vectors/fasttext/wiki-news-300d-1M-subword.vec", binary = F)
# glove_vectors <- read.vectors("../pretrained_wordvectors/GoogleNews-vectors-negative300.bin")

closest_words <- function(word, vector_space = glove_vectors, n = 10) {
  vectors <- vector_space[word, ]
  closest <- closest_to(vector_space, vectors, n = n + 1) %>%
    slice(-1)
  return(closest$word)
}


cosine_similarity <- function(word1, word2, vector_space = glove_vectors) {
  sim <- cosineSimilarity(vector_space[word1, ], vector_space[word2, ])
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

# manual work
# test1 <- closest_words("drink")
# test2 <- closest_words("propose")
# 
# neighbor_test1 <- glove_vectors[test1, ]
# neighbor_test2 <- glove_vectors[test2, ]
# 
# ov1 <- cosineSimilarity(neighbor_test1, glove_vectors["propose",]) %>% mean()
# ov2 <- cosineSimilarity(neighbor_test2, glove_vectors["drink",]) %>% mean()
# 
# mean(c(ov1, ov2))

semantic_overlap <- function(word1, word2, n = 10, vector_space = glove_vectors) {
  word1_neighbors = closest_words(word1, n = n)
  word2_neighbors = closest_words(word2, n = n)
  
  neighbor1_vectors <- vector_space[word1_neighbors, ]
  neighbor2_vectors <- vector_space[word2_neighbors, ]
  
  overlap1 = mean(cosineSimilarity(neighbor1_vectors, vector_space[word2, ]))
  overlap2 = mean(cosineSimilarity(neighbor2_vectors, vector_space[word1, ]))
  
  overlap = mean(c(overlap1, overlap2))
  
  return(overlap)
}

semantic_overlap("propose", "drink", n = 20)
semantic_overlap("drink", "propose")

semantic_overlap("hard", "strict")

pairsim("hard", "strict")

closest_words("harder")
# funtastic
experiment_words <- c("propose", "suggest", "consider", "run", "walk")

experiment <- crossing(
  word1 = experiment_words,
  word2 = experiment_words
) %>%
  filter(word1 != word2) %>%
  mutate(
    pairsim_overlap = map2_dbl(word1, word2, pairsim),
    semantic_overlap = map2_dbl(word1, word2, semantic_overlap)
  ) 

experiment %>% arrange(-pairsim_overlap)


sims <- map_dbl(seq(10, 100, by = 10), semantic_overlap, word1 = "propose", word2 = "suggest")
