library(tidyverse)
library(googleLanguageR)

errors <- read_csv("data/no_spell_errors.csv") %>%
  mutate(
    language = case_when(
      language == "Chinese" ~ "Chinese (Traditional)",
      TRUE ~ language
    )
  )

gl_translate_languages() %>%
  filter(name %in% unique(errors$language))

get_translation <- function(word, code) {
  translation <- gl_translate(word, target = code, source = "en")
  
  return(translation)
}

translated <- errors %>%
  inner_join(
    gl_translate_languages() %>% 
      rename(lang_code = "language", language = "name")
  ) %>%
  mutate(
    L1_i = map2(i, lang_code, get_translation),
    L1_c = map2(c, lang_code, get_translation)
  )


translated %>%
  unnest() %>%
  select(
    l1 = language, type, i, c, id, l1_i = translatedText, l1_c = translatedText1
  ) %>%
  write_csv("data/translated_no_spell_errors.csv")

translations %>%
  mutate(
    i_space = map_dbl(l1_i, str_count, pattern = " "),
    c_space = map_dbl(l1_c, str_count, pattern = " ")
  ) %>%
  filter(c_space == 0, i_space == 0) %>%
  select(-i_space, -c_space) %>%
  write_csv("data/translated_no_space.csv")
