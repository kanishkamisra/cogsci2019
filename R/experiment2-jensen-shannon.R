library(tidyverse)
library(boot)
library(philentropy)
library(kableExtra)

set.seed(1234)

tidy_overlaps <- read_csv("data/tidy_overlaps.csv")

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

experiment2$data[[1]] %>%
  mutate(
    l1_norm = l1/norm(as.matrix(l1)),
    l1_softmax = softmax(l1)
  ) %>%
  gather(l1_norm, l1_softmax, key = "prob", value = "value") %>%
  ggplot(aes(l1, value, color = prob, fill = prob)) +
  geom_line()

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
test <- experiment2$data[[1]]

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

wilcox_test <- function(df, indices) {
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

compute_difference <- function(df, n) {
  resamples <- sample_n(df, size = n, replace = T)
  
  difference <- mean(abs(resamples$l1 - resamples$l2))
  
  return(difference)
}
set.seed(1234)
replicated <- experiment2 %>%
  filter(neighbors == 10) %>%
  mutate(
    d = map(data, function(x) {
      return(replicate(compute_difference(x, 1000), n = 10000))
    })
  )

replicated %>% select(-data) %>% unnest() %>%
  ggplot(aes(group, d, color = group)) +
  geom_boxplot() + 
  facet_wrap(~model)

fasttext <- replicated %>%
  filter(model == "fasttext") %>%
  mutate(group = as.factor(group)) %>%
  select(-data) %>% unnest()

fasttext_aov <- aov(d ~ group, data = fasttext)

TukeyHSD(fasttext_aov)

plot(fasttext_aov)

polyglot <- replicated %>%
  filter(model == "polyglot") %>%
  mutate(group = as.factor(group)) %>%
  select(-data) %>% unnest()

polyglot_aov <- aov(d ~ group, data = polyglot)

polyglot_aov %>% summary()

TukeyHSD(polyglot_aov)

replicated %>% 
  mutate(
    real_d = map_dbl(data, function(x) {
      return(mean(abs(x$l1 - x$l2)))
    })
  ) %>%
  mutate(mean_d = map_dbl(d, mean)) %>%
  select(group, model, mean_d) %>%
  spread(model, mean_d)

group_correlation <- experiment2 %>%
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

