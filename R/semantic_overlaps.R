library(tidyverse)
library(wordVectors)
library(tictoc)
library(furrr)

plan(multicore(workers = 3L))
options(future.globals.maxSize = 10522669875)
# plan(sequential)

options(scien = 99)

translations <- read_csv("data/final_translations.csv") %>%
  mutate(c = str_to_lower(c), i = str_to_lower(i)) %>%
  filter(!c == "i've") %>%
  mutate(
    c = case_when(
      c == "hadn't" ~ "hadnt",
      c == "word's" ~ "words",
      c == "be-loved" ~ "beloved",
      TRUE ~ c
    ),
    i = case_when(
      i == "hadn't" ~ "hadnt",
      i == "word's" ~ "words",
      i == "be-loved" ~ "beloved",
      TRUE ~ i
    )
  )

translations %>%
  write_csv("data/experiment_final.csv")

english_words <- unique(c(translations$c, translations$i))
# english_words[which(!english_words %in% rownames(english_vectors))]

## I've -> ive, hadn't, word's, be-loved

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

# english_vectors <- read.vectors("data/pretrained_embeddings/cc.en.300.vec", binary = F)
english_vectors_wiki <- read.vectors("../pretrained_vectors/fasttext/wiki-news-300d-1M-subword.vec", binary= F)

closest_words("propose", english_vectors)

# english overlaps, n = 10
# plan(sequential)

# tic()
# translations %>%
#   slice(1:8) %>%
#   mutate(english_overlap = future_map2_dbl(i, c, semantic_overlap, vector_space = english_vectors))
# toc()

tic()
english_overlaps <- translations %>%
  mutate(
    english_overlap = map2(i, c, semantic_overlap, vector_space = english_vectors)
  )
toc()

english_overlaps %>% unnest() %>% write_csv("data/english_overlaps.csv")

english_overlaps %>% unnest() %>% arrange(-english_overlap) %>%
  View()

## WIKI WORD VECTORS
wiki_translations <- translations %>%
  mutate(
    c = str_replace_all(c, "'", ""),
    i = str_replace_all(i, "'", ""),
    i = case_when(
      i == "sun-burn" ~ "sunburn",
      TRUE ~ i
    )
  )

wiki_translation_words <- unique(c(wiki_translations$c, wiki_translations$i))
wiki_translation_words[which(!wiki_translation_words %in% rownames(english_vectors_wiki))]

tic()
english_overlaps_wiki <- wiki_translations %>%
  mutate(
    english_overlap = future_map2(i, c, semantic_overlap, vector_space = english_vectors_wiki)
  )
toc()

english_overlaps_wiki %>% unnest() %>% write_csv("data/english_overlaps_wiki.csv")

english_overlaps_wiki %>% unnest() %>% arrange(-english_overlap) %>%
  View()


tic()
cosine_similarity("propose", "suggest", english_vectors_wiki)
toc()

english_overlaps <- read_csv("data/english_overlaps.csv")
english_overlaps
