library(tidyverse)

errors <- read_csv("data/parsed.csv")
answers <- read_csv("data/answers_parsed.csv")

errors %>%
  select(i, c, type) %>%
  View()

errors %>%
  filter(!is.na(i), !is.na(c)) %>%
  mutate(
    lv_dist = stringdist::stringdist(i, c, method = "lv")
  ) %>%
  select(language, age, type, i, c, lv_dist) %>%
  filter(lv_dist > 3) %>%
  View()

answers %>% View()

errors %>%
  filter(!is.na(i), !is.na(c)) %>%
  filter(type %in% c("RJ", "RN", "RV", "RY", "RQ")) %>%
  select(language, type, i, c, file_name) %>%
  mutate(
    i_count = map_int(i, ~length(str_split(.x, " ", simplify = T))),
    c_count = map_int(c, ~length(str_split(.x, " ", simplify = T))),
  ) %>%
  filter(i_count == 1, c_count == 1) %>%
  count(language, sort = T)

errors %>%
  filter(!is.na(i), !is.na(c)) %>%
  filter(str_detect(type, "R")) %>%
  filter(!(type %in% c("RP", "R"))) %>%
  count(language, sort = T)

