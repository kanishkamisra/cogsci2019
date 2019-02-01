library(tidyverse)
library(boot)
library(philentropy)
library(kableExtra)

set.seed(1234)

tidy_overlaps <- read_csv("data/tidy_overlaps.csv")
errors <- read_csv("data/ms_final_experiments.csv")

tidy_overlaps %>% 
  filter(model == "fasttext") %>% 
  group_by(neighbors) %>% 
  summarize_at(vars("l1", "l2"), mean) %>% 
  rename(L1 = "l1", L2 = "l2") %>% 
  gather(L1, L2, key = "vector_space", value = "Overlap") %>% 
  ggplot(aes(neighbors, Overlap, color = vector_space, group = vector_space)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 0.4)) +
  theme_light(base_family = "CMU Sans Serif") +
  theme(
    legend.position = "top"
  ) + 
  labs(
    color = "Vector Space"
  )

ggsave("figures/average_overlap.png", height = 6, width = 8)

spearman_correlation <- function(df, indices) {
  df <- df[indices, ]
  return(cor(df[, 1], df[, 2], method = "spearman"))
}

pearson_correlation <- function(df, indices) {
  df <- df[indices, ]
  return(cor(df[, 1], df[, 2], method = "pearson"))
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

ggsave("figures/fasttext vs polyglot overlaps2.png", height = 7, width = 8)


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

ggsave("figures/fasttext vs polyglot correlations2.png")


### TABLE
results_experiment1 %>%
  select(-data) %>%
  unnest() %>%
  filter(neighbors == 10) %>%
  select(language, estimate, model, p.value) %>%
  mutate(
    p.value = scales::pvalue(p.value),
    correlation = paste0(round(estimate, 3), " (", p.value, ")")
  ) %>%
  select(-estimate, -p.value) %>%
  spread(model, correlation) %>%
  # filter(language != "Japanese") %>%
  knitr::kable(format = "latex", booktabs = T) %>%
  footnote("test")

## Jensen Shannon Divergence over overlaps

softmax <- function(x) {
  return(exp(x)/sum(exp(x)))
}

experiment2 <- tidy_overlaps %>%
  mutate(
    group = case_when(
      language %in% c("Russian", "Polish") ~ "Slavic",
      language %in% c("German", "Swedish") ~ "Germanic",
      language %in% c("French", "Spanish", "Catalan", "Italian", "Portuguese") ~ "Romance",
      language %in% c("Chinese (Simplified)", "Japanese", "Korean", "Thai") ~ "Asian",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(group, model, neighbors) %>%
  nest(l1, l2)

experiment2_jsd <- experiment2 %>%
  mutate(
    divergence = map_dbl(data, function(x) {
      l1 = softmax(x$l1)
      l2 = softmax(x$l2)
      
      m <- rbind(l1, l2)
      
      return(JSD(m))
    })
  )

experiment2_jsd

experiment2_jsd %>%
  filter(neighbors == 10) %>%
  select(-neighbors, -data) %>%
  mutate(divergence = round(divergence, 5)) %>%
  spread(model, divergence) %>%
  arrange(fasttext, polyglot) %>%
  knitr::kable(format = "latex", booktabs = T, caption = "Jensen-Shannon divergence for Language groups based on overlaps computed in fasttext and polyglot vectors.") %>%
  kable_styling()

experiment2_summed <- experiment2 %>%
  mutate(
    divergence = map_dbl(data, function(x) {
      l1 = abs(x$l1)/sum(abs(x$l1))
      l2 = abs(x$l2)/sum(abs(x$l2))
      
      m <- rbind(l1, l2)
      
      return(JSD(m))
    })
  )

experiment2_summed
## test density estimates

test <- experiment2 %>%
  filter(group == "Romance", neighbors == 10) %>%
  pull(data) %>%
  .[[1]]

test_l1 <- density(test$l1)

test_l1$y %>% sum()


results_experiment1 %>%
  select(-data) %>%
  unnest() %>%
  filter(neighbors == 10) %>% select(language, model, estimate) %>% 
  spread(model, estimate) %>% filter(language != "Japanese") %>%
  mutate(
    correlation_test = map2(fasttext, polyglot, function(x, y) {
      s <- cor.test(x, y, method = "pearson", exact = F)
      
      result <- broom::tidy(s) %>%
        select(estimate, p.value)
      
      return(result)
    })
  ) %>%
  unnest()

within_models <- results_experiment1 %>%
  select(-data) %>%
  unnest() %>%
  filter(neighbors == 10) %>% select(language, model, estimate) %>% 
  spread(model, estimate) %>% filter(language != "Japanese")

s <- cor.test(within_models$fasttext, within_models$polyglot, method = "pearson")
s

s

fasttext_polyglot <- tidy_overlaps %>%
  select(case_id, language, model, neighbors, l1, l2) %>%
  gather(l1, l2, key = "type", value = "overlap") %>%
  unite(model_type, model, type) %>%
  group_by(case_id, language, neighbors) %>%
  spread(model_type, overlap) %>%
  ungroup() %>%
  filter(language != "Japanese", language != "Dutch") %>%
  filter(!is.na(polyglot_l1))

case_languages <- tidy_overlaps %>%
  distinct(case_id, language)

l1 <- tidy_overlaps %>% 
  select(case_id, model, neighbors, l1) %>% 
  spread(model, l1) %>% 
  filter(!is.na(polyglot)) %>%
  inner_join(case_languages) %>%
  select(-case_id) %>%
  group_by(language, neighbors) %>%
  nest() 

l2 <- tidy_overlaps %>% 
  select(case_id, model, neighbors, l2) %>% 
  spread(model, l2) %>% 
  filter(!is.na(polyglot)) %>%
  inner_join(case_languages) %>%
  select(-case_id) %>%
  group_by(language, neighbors) %>%
  nest() 

l1_correlation <- l1 %>%
  filter(language != "Dutch") %>%
  mutate(
    n = map_dbl(data, nrow),
    correlation_test = map(data, function(x) {
      s <- cor.test(x$fasttext, x$polyglot, method = "spearman", exact = F)
      
      result <- broom::tidy(s) %>%
        select(estimate, p.value)
      
      return(result)
    }),
    bootstrapped_ci = map(data, strap_ci)
  )

l1_correlation %>% 
  select(-data) %>% 
  unnest() %>% 
  filter(neighbors == 10) %>% 
  arrange(-estimate)

l2_correlation <- l2 %>%
  filter(language != "Dutch") %>%
  mutate(
    n = map_dbl(data, nrow),
    correlation_test = map(data, function(x) {
      s <- cor.test(x$fasttext, x$polyglot, method = "spearman", exact = F)
      
      result <- broom::tidy(s) %>%
        select(estimate, p.value)
      
      return(result)
    }),
    bootstrapped_ci = map(data, strap_ci)
  )

l2_correlation %>% 
  select(-data) %>% 
  unnest() %>% 
  filter(neighbors == 10) %>% 
  arrange(-estimate)


tidy_overlaps %>%
  filter(model == "fasttext", neighbors == "10") %>%
  select(language, l1, l2) %>%
  mutate(
    l1_distribution = softmax(l1),
    l2_distribution = softmax(l2)
  ) %>%
  # gather(l1, l2, key = "type", value = "overlap") %>%
  ggplot(aes(l1, l1_distribution)) +
  geom_line() +
  facet_wrap(~language)


tidy_overlaps %>%
  mutate(
    group = case_when(
      language %in% c("Russian", "Polish") ~ "Slavic",
      language %in% c("German", "Swedish") ~ "Germanic",
      language %in% c("French", "Spanish", "Catalan", "Italian", "Portuguese") ~ "Romance",
      language %in% c("Chinese (Simplified)", "Japanese", "Korean", "Thai") ~ "Asian",
      TRUE ~ "Other"
    )
  ) %>%
  distinct(language, group) %>%
  group_by(group) %>%
  nest() %>%
  mutate(
    languages = map_chr(data, function(x) {
      x %>%
        pull(language) %>%
        glue::glue_collapse(sep = ", ") %>%
        as.character()
    })
  ) %>%
  select(-data) %>%
  knitr::kable(format = "latex", booktabs = T, caption = "Language Groups and the languages that they contain.")

