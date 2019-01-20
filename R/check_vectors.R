library(tidyverse)
library(wordVectors)
library(googleLanguageR)

languages <- "ca zh nl fr de el it ja ko pl pt ru es sv th tr" %>%
  str_split(" ", simplify = T) %>%
  as.character()

gl_langs <- gl_translate_languages() %>%
  filter(language %in% languages) %>%
  rename(code = "language", language = "name")

translations <- read_csv("data/ms_translated_all.csv") %>%
  mutate(
    language = case_when(
      language == "Chinese" ~ "Chinese (Simplified)",
      TRUE ~ language
    )
  ) %>%
  inner_join(gl_langs) %>%
  mutate(
    i_space = map_dbl(l1_i, str_count, pattern = " "),
    c_space = map_dbl(l1_c, str_count, pattern = " ")
  ) %>%
  filter(c_space == 0, i_space == 0) %>%
  select(-i_space, -c_space)

check_vectors <- function(words, code) {
  
  cat(paste0("reading vectors for: ", code), sep = "\n")
  vector_words <- rownames(read.vectors(paste0("data/pretrained_embeddings/cc.", code, ".vec"), binary = F))
  
  result <- words[which(!words %in% vector_words)]
  rm(vector_words)
  cat(glue::glue_collapse(result, sep = ","))
  return(result)
}

get_words <- function(code_lang, df) {
  
  df <- df %>%
    filter(code == code_lang)
  
  return(unique(c(df$l1_i, df$l1_c)))
}

checked_vectors <- gl_langs %>%
  filter(!code %in% c("nl", "it", "ko", "pt", "ru", "es", "sv")) %>%
  mutate(
    unique_words = map(code, get_words, df = translations),
    len = map_int(unique_words, length)
  ) %>%
  mutate(
    absent = map2(unique_words, code, check_vectors)
  )

absent <- checked_vectors %>%
  mutate(absent_percent = map_int(absent, length)/len) %>%
  select(-unique_words) %>%
  unnest() %>%
  pull(absent)

checked_vectors %>%
  mutate(absent_percent = map_int(absent, length)/len) %>%
  select(-unique_words) %>%
  unnest() %>%
  write_csv("data/absent.csv")

final_translations <- translations %>%
  filter(!l1_i %in% absent, !l1_c %in% absent)
  
final_translations %>%
  write_csv("data/final_translations.csv")

final_translations %>%
  count(language, sort = T)