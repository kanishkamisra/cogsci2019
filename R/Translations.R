library(tidyverse)
library(googleLanguageR)

errors <- read_csv("data/no_spell_errors.csv")

gl_translate_languages() %>%
  filter(name %in% unique(errors$language))
