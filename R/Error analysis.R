library(tidyverse)

errors <- read_csv("data/parsed.csv")

content_errors <- errors %>%
  filter(!is.na(i), !is.na(c)) %>%
  filter(type %in% c("RJ", "RN", "RV", "RY")) %>%
  select(language, type, i, c, file_name) %>%
  mutate(
    id = paste("person", str_extract(file_name, "(?<=doc)(.*)(?=\\.xml)"), sep = "_"),
    i_count = map_int(i, ~length(str_split(.x, " ", simplify = T))),
    c_count = map_int(c, ~length(str_split(.x, " ", simplify = T))),
  ) %>%
  select(-file_name)

single_word_errors <- content_errors %>%
  filter(i_count == 1, c_count == 1) %>%
  select(-i_count, -c_count)

multi_word_errors <- content_errors %>%
  filter(i_count > 1 | c_count > 1) %>%
  select(-i_count, -c_count)

content_errors %>%
  count(language, sort = T)

single_word_errors %>%
  count(id, language, sort = T)

# unique people making content word mistakes (single word only) = 1199

single_word_errors %>%
  count(language, sort = TRUE)

single_word_errors
  