# Animate country rankings of population size
# Jonas Schöley

library(tidyverse)
library(gganimate)

rank_sim <- read_csv('./dat/simulated_data.csv')

rank_fig <-
  rank_sim %>%
  ggplot() +
  geom_point(
    aes(x = reorder(name, -value), y = value)
  ) +
  transition_states(
    id,
    transition_length = 2,
    state_length = 1
  ) +
  coord_flip() +
  labs(x = NULL, y = 'Population size')

rank_fig
