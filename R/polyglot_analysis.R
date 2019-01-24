library(tidyverse)
library(boot)
library(philentropy)

set.seed(1234)

# polyglot_experiment <- read_csv("data/ms_final_with_polyglot_2.csv")
polyglot_experiment <- read_csv("data/polyglot_experiment_final.csv") %>%
  mutate(case_id = paste0("case_", str_pad(row_number(), width = 4, pad = "0")))

fasttext_experiment <- read_csv("data/ms_final_experiments.csv") %>%
  mutate(case_id = paste0("case_", str_pad(row_number(), width = 4, pad = "0")))
  

# polyglot_experiment %>%
#   select_at(vars("c", "i", "l1_c", "l1_i", "id", "language", contains("polyglot_10"))) %>%
#   add_count(language) %>%
#   group_by(language) %>%
#   mutate(
#     missing = mean(l1_sim_polyglot_10 == -10 | l2_sim_polyglot_10 == -10)
#   ) %>%
#   distinct(language, n, missing) %>%
#   arrange(-missing)
# 
# polyglot_experiment %>%
#   filter(l1_sim_polyglot_10 != -10 , l2_sim_polyglot_10 != -10) %>%
#   filter(language != "Japanese") %>%
#   write_csv("data/polyglot_experiment_final.csv")


polyglot_experiment %>%
  filter(language != "Dutch") %>%
  select(-c, -i, -l1_c, -l1_i, -code, -case_id) %>%
  select_at(vars("language", contains("polyglot"))) %>%
  gather(l2_sim_polyglot_5:l1_sim_polyglot_100, key = "neighbors", value = "overlap") %>%
  mutate(
    # neighbors = as.numeric(str_extract(neighbors, "(?<=cc_).+$*"))
    neighbors = str_replace(neighbors, "_sim_polyglot_", "_")
  ) %>%
  separate(neighbors, c("overlap_type", "neighbors"), sep = "_") %>%
  mutate(neighbors = as.numeric(neighbors)) %>%
  group_by(language, overlap_type, neighbors) %>%
  summarize(
    overlap = mean(overlap)
  ) %>%
  ggplot(aes(as.factor(neighbors), overlap, color = overlap_type, group = overlap_type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~language) +
  scale_y_continuous(limits = c(0, 0.6)) +
  theme_light(base_family = "CMU Sans Serif") +
  theme(
    legend.position = "top",
    strip.text = element_text(color = "black")
  ) +
  labs(
    color = "Overlap Language",
    x = "Nearest Neighbors"
  )


experiment1 <- polyglot_experiment %>%
  filter(language != "Dutch") %>%
  select(case_id, language, type, l2_sim_polyglot_10, l1_sim_polyglot_10)


spearman_correlation <- function(df, indices) {
  df <- df[indices, ]
  return(cor(df[, 1], df[, 2], method = "spearman"))
}

strap_ci <- function(x, f = spearman_correlation, n = 1000, type = "basic") {
  strapped <- boot(x, f, R = n)
  ci <- boot.ci(strapped, type = "basic")
  
  result <- tibble(
    low = ci$basic[1, 4],
    high = ci$basic[1, 5]
  )
  
  return(result)
}

results1 <- experiment1 %>%
  group_by(language) %>%
  nest(l2_sim_polyglot_10, l1_sim_polyglot_10) %>%
  mutate(
    n = map_dbl(data, nrow),
    correlation_test = map(data, function(x) {
      s <- cor.test(x$l1_sim_polyglot_10, x$l2_sim_polyglot_10, method = "spearman", exact = F)
      
      result <- broom::tidy(s) %>%
        select(estimate, p.value)
      
      return(result)
    }),
    bootstrapped_ci = map(data, strap_ci)
  )  

results1

results1 %>%
  select(-data) %>%
  unnest() %>%
  mutate(language = fct_reorder(language, estimate)) %>%
  ggplot(aes(estimate, language)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  scale_x_continuous(limit = c(-1, 1)) +
  theme_light(base_family = "CMU Sans Serif") +
  labs(
    x = "Spearman's Correlation"
  )

ggsave("figures/polyglot_correlation.png", height = 5, width =6)


## Correlation Experiment

nn_experiment <- polyglot_experiment %>%
  filter(language != "Dutch") %>%
  select(-c, -i, -l1_c, -l1_i, -code) %>%
  select_at(vars("language", "case_id", contains("polyglot"))) %>%
  gather(l2_sim_polyglot_5:l1_sim_polyglot_100, key = "neighbors", value = "overlap") %>%
  mutate(
    # neighbors = as.numeric(str_extract(neighbors, "(?<=cc_).+$*"))
    neighbors = str_replace(neighbors, "_sim_polyglot_", "_")
  ) %>%
  separate(neighbors, c("overlap_type", "neighbors"), sep = "_") %>%
  mutate(neighbors = as.numeric(neighbors)) %>% 
  spread(overlap_type, overlap)

results2 <- nn_experiment %>%
  select(-case_id) %>%
  group_by(language, neighbors) %>%
  nest() %>%
  mutate(
    n = map_dbl(data, nrow),
    correlation_test = map(data, function(x) {
      s <- cor.test(x$l1, x$l2, method = "spearman", exact = F)
      
      result <- broom::tidy(s) %>%
        select(estimate, p.value)
      
      return(result)
    }),
    bootstrapped_ci = map(data, strap_ci)
  )

results2 %>%
  select(-data) %>%
  unnest() %>%
  ggplot(aes(neighbors, estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = low, ymax = high)) +
  facet_wrap(~language)

ggsave("figures/polyglot_correlation_nearestneighbors.png", height = 6, width = 8)

polyglot_final <- polyglot_experiment %>%
  select_at(vars("case_id", "person_id" = "id","language", "type", "i", "c", "l1_i", "l1_c", contains("polyglot"))) %>%
  gather(l2_sim_polyglot_5:l1_sim_polyglot_100, key = "metric", value = "overlap") %>%
  mutate(metric = str_replace(metric, "sim_", "")) %>%
  separate(metric, into = c("type", "model", "neighbors"), sep = "_") %>%
  spread(type, overlap) %>%
  mutate(neighbors = as.numeric(neighbors))

fasttext_final <- fasttext_experiment %>%
  select_at(vars("case_id", "person_id" = "id","language", "type", "i", "c", "l1_i", "l1_c", contains("cc"))) %>%
  gather(l2_sim_cc_5:l1_sim_cc_100, key = "metric", value = "overlap") %>%
  mutate(metric = str_replace(metric, "sim_", "")) %>%
  separate(metric, into = c("type", "model", "neighbors"), sep = "_") %>%
  spread(type, overlap) %>%
  mutate(
    neighbors = as.numeric(neighbors),
    model = str_replace(model, "cc", "fasttext")
  )

tidy_overlaps <- bind_rows(
  fasttext_final, 
  polyglot_final
)

tidy_overlaps %>%
  write_csv("data/tidy_overlaps.csv")

tidy_overlaps %>%
  filter(model == "polyglot")

## Overlaps Between

tidy_overlaps <- read_csv("data/tidy_overlaps.csv")

tidy_overlaps %>%
  filter(language != "Dutch") %>%
  gather(l1, l2, key = "metric", value = "overlap") %>%
  group_by(language, model, neighbors, metric) %>%
  summarize(overlap = mean(overlap)) %>%
  ggplot(aes(neighbors, overlap, color = metric, linetype = model, group = interaction(metric, model))) +
  # geom_point() +
  geom_line(lwd = 0.7) +
  scale_x_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100)) +
  facet_wrap(~language) +
  scale_y_continuous(limits = c(0, 0.6)) +
  theme_light(base_family = "CMU Sans Serif") +
  theme(
    legend.position = "top",
    strip.text = element_text(color = "black")
  ) +
  labs(
    color = "Overlap Language",
    x = "Nearest Neighbors"
  )

ggsave("figures/fasttext vs polyglot overlaps.png", height = 7, width = 8)


results_experiment1 <- tidy_overlaps %>%
  filter(language != "Dutch") %>%
  select(language, model, neighbors, l1, l2) %>%
  group_by(language, neighbors, model) %>%
  nest() %>%
  mutate(
    n = map_dbl(data, nrow),
    correlation_test = map(data, function(x) {
      s <- cor.test(x$l1, x$l2, method = "spearman", exact = F)
      
      result <- broom::tidy(s) %>%
        select(estimate, p.value)
      
      return(result)
    }),
    bootstrapped_ci = map(data, strap_ci)
  )
  

results_experiment1 %>%
  select(-data) %>%
  unnest() %>%
  filter(neighbors == 10) %>%
  # group_by(model) %>%
  mutate(language = fct_reorder(language, estimate)) %>%
  ggplot(aes(estimate, language)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  scale_x_continuous(limits = c(-0.5, 1)) +
  facet_wrap(~ model) +
  theme_light(base_family = "CMU Sans Serif") +
  theme(
    strip.text = element_text(color = "black", face = "bold"), 
    axis.title.y = element_blank()
  ) +
  labs(
    x = "Spearman's Correlation",
    y = ""
  )

ggsave("figures/fasttext vs polyglot correlations.png")

