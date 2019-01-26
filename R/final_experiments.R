library(tidyverse)
library(boot)
library(philentropy)

set.seed(1234)

tidy_overlaps <- read_csv("data/tidy_overlaps.csv")

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


## Jensen Shannon Divergence over overlaps

