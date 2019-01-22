library(tidyverse)

options(scipen = 99)

examples <- read_csv("data/ms_final_experiments.csv")


examples %>%
  mutate(case_id = paste0("case_", str_pad(row_number(), width = 4, pad = "0"))) %>%
  filter(language == "Russian") %>%
  select(c, i, id, case_id, l1_c, l1_i, code, type, l2_sim_cc_10, l1_sim_cc_10)


