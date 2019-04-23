library(tidyverse)

overlaps <- read_csv("data/tidy_overlaps.csv") %>%
  filter(neighbors == 10)

tidy_overlaps <- read_csv("data/tidy_overlaps.csv") %>%
  filter(neighbors == 10) %>%
  filter(model == "polyglot")
fasttext_neighbors <- read_csv("python/fasttext_result_set_sim.csv")
polyglot_neighbors <- read_csv("python/poly_result_set_sim.csv")

answers <- read_csv("data/answers_parsed.csv") %>%
  mutate(
    person_id = paste("person", str_extract(file_name, "(?<=doc)(.*)(?=\\.xml)"), sep = "_")
  )

tidy_overlaps %>%
  filter(case_id == "case_0007")

polyglot_neighbors %>%
  filter(case_id == "case_2331") %>%
  select_at(vars(-contains("l1_"))) %>%
  select_at(vars(-contains("sim"))) %>%
  gather(i_nn_1:c_nn_10, key = "type", value = "sim")

polyglot_neighbors %>%
  filter(case_id == "case_2331") %>%
  select_at(vars(contains("l1_"))) %>%
  select_at(vars(-contains("sim"))) %>%
  gather(l1_i_nn_1:l1_c_nn_10, key = "type", value = "sim")


tidy_overlaps %>%
  mutate(difference = abs(l1-l2)) %>%
  arrange(-(difference)) %>%
  select(case_id, language, i, c, l1_i, l1_c, difference, l1, l2) %>%
  filter(between(l1, 0.5, 0.99), l2 > 0) %>% View()

neighborhoods <- function(case) {
  
  neighbors_l1 <- polyglot_neighbors %>%
    filter(case_id == case) %>%
    select_at(vars(contains("l1_"))) %>%
    gather(type, word, contains("_nn_")) %>%
    mutate(
      kind = str_extract(type, ".+?(?=_nn)"),
      type = case_when(
        str_detect(type, "sim") ~ "sim_l1",
        TRUE ~ "word_l1"
      )
    ) %>%
    group_by(type, kind) %>%
    mutate(
      extra = row_number()
    ) %>% 
    spread(type, word, drop = F) %>%
    select(-extra) %>%
    ungroup()
  
  neighbors_l2 <- polyglot_neighbors %>%
    filter(case_id == case) %>%
    select_at(vars(-contains("l1_"))) %>%
    gather(type, word, contains("_nn_")) %>%
    mutate(
      kind = str_extract(type, ".+?(?=_nn)"),
      type = case_when(
        str_detect(type, "sim") ~ "sim_english",
        TRUE ~ "word_english"
      )
    ) %>%
    group_by(type, kind) %>%
    mutate(
      extra = row_number()
    ) %>% 
    spread(type, word, drop = F) %>%
    select(-extra) %>%
    ungroup()
  
  return(bind_cols(neighbors_l1, neighbors_l2))
}

# get_answers <- function(case)

neighborhoods(case = "case_4340") %>%
  pull(word_l1) %>% 
  glue::glue_collapse(sep = ", ")
tidy_overlaps %>%
  filter(case_id == "case_4340")

tidy_overlaps %>%
  mutate(difference = abs(l1-l2)) %>%
  arrange(-(difference)) %>%
  select(case_id, language, i, c, l1_i, l1_c, difference, l1, l2) %>%
  filter(between(l1, 0.5, 0.99), l2 > 0, l1_i != l1_c, language == "Russian") %>% View()

tidy_overlaps %>%
  filter(case_id == "case_4340")

answers %>% filter(person_id == "person_514") %>%
  pull(i)


overlaps %>% ggplot(aes(l1, l2)) + geom_point() + geom_smooth(method = "lm") + facet_grid(model~language, scale = "free")
  
