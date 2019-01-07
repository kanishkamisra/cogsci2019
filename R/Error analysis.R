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
  count(language, sort = T)

errors %>%
  filter(!is.na(i), !is.na(c)) %>%
  filter(str_detect(type, "R")) %>%
  filter(!(type %in% c("RP", "R"))) %>%
  count(language, sort = T)

