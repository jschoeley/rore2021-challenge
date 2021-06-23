# Plot distribution of country rankings of population size
# Jonas Schöley

library(tidyverse)

rank_sim <- read_csv('./dat/simulated_data.csv')

n_countries <- length(unique(rank_sim$name))
n_sim <- 1000

rank_mean <-
  rank_sim %>%
  group_by(name) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(meanrank = rank(mean)) %>%
  select(name, mean, meanrank)

rank_prob <- expand_grid(name = unique(rank_sim$name), rank = 1:n_countries)
rank_prob <-
  rank_sim %>%
  group_by(id) %>%
  mutate(rank = rank(value)) %>%
  group_by(name, rank) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  right_join(rank_prob) %>%
  arrange(name, rank) %>%
  mutate(
    n = ifelse(is.na(n), 0, n),
    p = n/n_sim
  ) %>%
  left_join(rank_mean)

rank_prob %>%
  ggplot() +
  geom_tile(
    aes(y = reorder(name, meanrank), x = rank, fill = p),
    color = 'white', size = 0.5
  ) +
  scale_x_continuous(breaks = 1:n_countries) +
  scale_fill_viridis_c() +
  labs(x = 'Rank', y = NULL) +
  theme_minimal() +
  labs(
    title = 'Distribution of each countries rank in projected population size',
    fill = 'Probability\nof rank'
  )
