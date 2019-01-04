library(tidyverse)

errors <- read_csv("data/parsed.csv")
answers <- read_csv("data/answers_parsed.csv")

errors %>%
  select(i, c) %>%
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
