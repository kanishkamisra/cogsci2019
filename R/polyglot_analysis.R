library(tidyverse)

polyglot_experiment <- read_csv("data/ms_final_polyglot_english.csv")

polyglot_experiment %>%
  select_at(vars("c", "i", "l1_c", "l1_i", "id", contains("polyglot"))) %>%
  filter(l2_sim_polyglot_5 == -10)
