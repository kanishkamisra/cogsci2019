library(tidyverse)
library(jsonlite)

fasttext_neighbors <- read_csv("data/fasttext_result_set.csv") %>%
  select(1:21) %>%
  gather(-case_id, key = "type", value = "neighbor") %>% 
  separate(type, into = c("type", "useless", "rank"), sep = "_") %>%
  select(-useless)
  
polyglot_neighbors <- read_csv("data/poly_result_set.csv") %>%
  select(1:21) %>%
  gather(-case_id, key = "type", value = "neighbor") %>% 
  separate(type, into = c("type", "useless", "rank"), sep = "_") %>%
  select(-useless)

fasttext_neighbors
polyglot_neighbors

errors <- read_csv("data/tidy_overlaps.csv") %>%
  filter(neighbors == 10)

answers <- read_csv("data/answers_parsed.csv") %>%
  mutate(
    person_id = paste("person", str_extract(file_name, "(?<=doc)(.*)(?=\\.xml)"), sep = "_")
  ) %>%
  select(-file_name)

get_answers <- function(df, person, option = "i") {
  df %>%
    filter(person_id == person) %>%
    select(i, c) %>%
    select_at(vars(option)) %>%
    pull()
}

errors %>% filter(language == "Russian")

## acquiring and getting

ex_case_id <- errors %>%
  filter(language == "Russian") %>%
  filter(c == "acquiring") %>%
  distinct(case_id) %>%
  pull()

fasttext_neighbors %>%
  filter(case_id == "case_1513") %>%
  filter(type == "c") %>% pull(neighbor) %>%
  glue::glue_collapse(sep = ", ")

polyglot_neighbors %>%
  filter(case_id == "case_1513") %>%
  filter(type == "c") %>% pull(neighbor) %>%
  glue::glue_collapse(sep = ", ")
