library(tidyverse)
library(wordVectors)

options(scipen = 99)

examples <- read_csv("data/ms_final_experiments.csv")

errors <- read_csv("data/experiment_final.csv")
english_vectors <- read.vectors("../data/pretrained_embeddings/cc.en.300.vec", binary = F)
russian_vectors <- read.vectors("../data/pretrained_embeddings/cc.ru.vec", binary = F)

# examples %>%
#   mutate(case_id = paste0("case_", str_pad(row_number(), width = 4, pad = "0"))) %>%
#   filter(language == "Russian") %>%
#   select(c, i, id, case_id, l1_c, l1_i, code, type, l2_sim_cc_10, l1_sim_cc_10)

example <- errors %>% filter(c == "acquiring", i == "getting")

eng_incorrect <- example$i
eng_correct <- example$c
ru_incorrect <- example$l1_i
ru_correct <- example$l1_c

closest_words <- function(word, vector_space, n = 10) {
  vectors <- vector_space[word, ]
  closest <- closest_to(vector_space, vectors, n = n + 1) %>%
    slice(-1)
  return(closest$word)
}

cosine_similarity <- function(word1, word2, vector_space = glove_vectors) {
  sim <- cosineSimilarity(vector_space[word1, ], vector_space[word2, ])
  return(as.double(sim))
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

english_vectors <- read.vectors("../../pretrained_vectors/fasttext/wiki-news-300d-1M-subword.vec", binary = F)

fasttex_wiki_eng <- tibble(
  word = c(eng_correct, eng_incorrect),
  type = c("c", "i")
) %>%
  mutate(nearest = map(word, closest_words, vector_space = english_vectors))

fasttex_wiki_eng %>% select(-type) %>% spread(word, nearest) %>% unnest() %>% write_csv("../data/examples/fasttext_wiki_eng.csv")

fasttex_cc_eng <- tibble(
  word = c(eng_correct, eng_incorrect),
  type = c("c", "i")
) %>%
  mutate(nearest = map(word, closest_words, vector_space = english_vectors))

fasttex_cc_eng %>% select(-type) %>% spread(word, nearest) %>% unnest() %>% write_csv("../data/examples/fasttext_cc_eng.csv")

rm(english_vectors)
english_vectors <- read.vectors("../../pretrained_vectors/GoogleNews-vectors-negative300.bin")

word2vec_cc_eng <- tibble(
  word = c(eng_correct, eng_incorrect),
  type = c("c", "i")
) %>%
  mutate(nearest = map(word, closest_words, vector_space = english_vectors))

word2vec_cc_eng %>% select(-type) %>% spread(word, nearest) %>% unnest() %>% write_csv("../data/examples/word2vec_cc_eng.csv")

rm(english_vectors)

russian_cc <- tibble(
  word = c(ru_correct, ru_incorrect),
  type = c("c", "i")
) %>%
  mutate(nearest = map(word, closest_words, vector_space = russian_vectors))

russian_cc %>% select(-type) %>% spread(word, nearest) %>% unnest() %>% write_csv("../data/examples/fasttext_cc_ru.csv")
