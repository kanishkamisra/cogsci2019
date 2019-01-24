library(tidyverse)

polyglot_experiment <- read_csv("data/ms_final_with_polyglot_2.csv")

polyglot_experiment %>%
  select_at(vars("c", "i", "l1_c", "l1_i", "id", "language", contains("polyglot_10"))) %>%
  add_count(language) %>%
  group_by(language) %>%
  mutate(
    missing = mean(l1_sim_polyglot_10 == -10 | l2_sim_polyglot_10 == -10)
  ) %>%
  distinct(language, n, missing) %>%
  arrange(-missing)

polyglot_experiment %>%
  select_at(vars("c", "i", "l1_c", "l1_i", "id", "language", contains("polyglot_10"))) %>%
  filter(language == "German") %>%
  filter(l1_sim_polyglot_10 == -10)

polyglot_experiment %>%
  filter(l1_sim_polyglot_10 != -10 | l2_sim_polyglot_10 != -10) %>%
  filter(language != "Japanese") %>%
  write_csv("data/polyglot_experiment_final.csv")
