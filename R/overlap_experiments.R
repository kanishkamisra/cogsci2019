library(tidyverse)
library(boot)

overlaps <- read_csv("data/ms_final_experiments.csv") %>%
  mutate(case_id = paste0("case_", str_pad(row_number(), width = 4, pad = "0")))

cases <- overlaps %>%
  select(case_id, person_id = id, language, type)

overlaps %>%
  select(-c, -i, -l1_c, -l1_i, -code, -case_id) %>%
  gather(l2_sim_cc_5:l1_sim_cc_100, key = "neighbors", value = "overlap") %>%
  mutate(
    # neighbors = as.numeric(str_extract(neighbors, "(?<=cc_).+$*"))
    neighbors = str_replace(neighbors, "_sim_cc_", "_")
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
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_light(base_family = "CMU Sans Serif") +
  theme(
    legend.position = "top",
    strip.text = element_text(color = "black")
  ) +
  labs(
    color = "Overlap Language",
    x = "Nearest Neighbors"
  )

ggsave("figures/nearest_neighbor_overlap2.png")  

experiment1 <- overlaps %>%
  select(case_id, language, type, l2_sim_cc_10, l1_sim_cc_10)

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
  nest(l2_sim_cc_10, l1_sim_cc_10) %>%
  mutate(
    n = map_dbl(data, nrow),
    correlation_test = map(data, function(x) {
      s <- cor.test(x$l1_sim_cc_10, x$l2_sim_cc_10, method = "spearman", exact = F)
      
      result <- broom::tidy(s) %>%
        select(estimate, p.value)
      
      return(result)
    }),
    bootstrapped_ci = map(data, strap_ci)
  )

experiment1 %>%
  group_by(language) %>%
  nest(l2_sim_cc_10, l1_sim_cc_10) %>%
  mutate(
    sds = map(data, function(x) {
      x %>%
        mutate_all(sd)
    })
  ) %>%
  unnest(sds) %>%
  distinct()


test1 <- experiment2 %>% pull(data) %>% .[[2]]

x <- abs(test1$l1_sim_cc_10)/sum(abs(test1$l1_sim_cc_10))
y <- abs(test1$l2_sim_cc_10)/sum(abs(test1$l2_sim_cc_10))

JSD(rbind(x, y))
