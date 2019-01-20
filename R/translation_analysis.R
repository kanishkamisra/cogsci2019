library(tidyverse)
library(wordVectors)
library(tictoc)

# translations <- read_csv("data/translated_no_spell_errors.csv")
translations <- read_csv("data/ms_translated_all.csv") %>%
  mutate(
    i_space = map_dbl(l1_i, str_count, pattern = " "),
    c_space = map_dbl(l1_c, str_count, pattern = " ")
  ) %>%
  filter(c_space == 0, i_space == 0) %>%
  select(-i_space, -c_space)

translations %>%
  filter(language == "Chinese")

## functions

closest_words <- function(word, vector_space, n = 10) {
  vectors <- vector_space[word, ]
  closest <- closest_to(vector_space, vectors, n = n + 1) %>%
    slice(-1)
  return(closest$word)
}

semantic_overlap <- function(word1, word2, n = 10, vector_space) {
  cat(paste0("word1: ", word1, ", word2: ", word2), sep = "\n")
  word1_neighbors = closest_words(word1, n = n, vector_space = vector_space)
  word2_neighbors = closest_words(word2, n = n, vector_space = vector_space)
  
  neighbor1_vectors <- vector_space[word1_neighbors, ]
  neighbor2_vectors <- vector_space[word2_neighbors, ]
  
  overlap1 = mean(cosineSimilarity(neighbor1_vectors, vector_space[word2, ]))
  overlap2 = mean(cosineSimilarity(neighbor2_vectors, vector_space[word1, ]))
  
  overlap = mean(c(overlap1, overlap2))
  
  return(overlap)
}

tic()
chinese_vectors <- read.vectors("data/pretrained_embeddings/cc.zh.vec", binary = F)
simplified_chinese_vectors <- read.vectors("data/pretrained_embeddings/wiki.zh_classical.vec", binary = F)
toc()

chinese_words <- unique(c(
  translations %>%
    filter(language == "Chinese") %>%
    pull(l1_i),
  translations %>%
    filter(language == "Chinese") %>%
    pull(l1_c)
))

simplified_chinese_vectors[chinese_words, ]

chinese_words %in% rownames(chinese_vectors)

chinese_words %in% rownames(simplified_chinese_vectors)

## experiments
translations %>%
  count(l1, sort = T)

swedish_overlaps <- translations %>%
  filter(l1 == "Swedish") %>%
  mutate(
    i_space = map_dbl(l1_i, str_count, pattern = " "),
    c_space = map_dbl(l1_c, str_count, pattern = " ")
  ) %>%
  filter(c_space == 0, i_space == 0) %>%
  mutate(l1_overlap = map2(l1_i, l1_c, semantic_overlap, vector_space = swedish_vectors))

swedish_overlaps %>% 
  unnest() %>% 
  select(-i_space, -c_space) %>%
  write_csv("data/swedish_10.csv")


swedish_overlaps %>% 
  unnest() %>% 
  select(-i_space, -c_space) %>%
  count(l1_overlap) %>%
  ggplot(aes(l1_overlap)) +
  geom_histogram()
