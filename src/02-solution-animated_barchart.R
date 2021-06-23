################################################################################
###
### DATE CREATED: 2021-06-21
###
### AUTHOR: Group A1, Rostock Retreat 2021
###
### PROJECT: https://www.rostock-retreat.org/
###          Visualizing Uncertainty in Ordering of Values Subject to Uncertainty
###
### DESCRIPTION: Animations to visualize the uncertainty in the rank
###     order of observational units when the indicator value is
###     subject to uncertainty.
###
### REFENCES:
###     Data downloaded from https://population.un.org/dataportal/home
###     Animation code based on
###     https://typethepipe.com/vizs-and-tips/reorder-bars-r-ggplot-gganimate/
###
###-----------------------------------------------------------------------------
###
################################################################################


library(tidyverse)
library(gganimate)

## Original data
orig_data <- read_csv('dat/female_pop_2050_Western_Africa.csv')
orig_data <- filter(orig_data, name != "Nigeria")
                                # no ambiguity about order of Nigeria
                                # so don't include for the sake of the
                                # example

## Simulated data
rank_sim <- read_csv('dat/simulated_data.csv')

## Create ranks within simulations
rank_sim <-
    rank_sim %>%
    group_by(id) %>%
    mutate(rank = rank(value))

## Merge on original medians
orig_medians <-
    orig_data %>%
    filter(stat == "median") %>%
    select(name, value) %>%
    rename(median = value)
rank_sim <-
    left_join(rank_sim, orig_medians)


## Rescale
rank_sim <-
    rank_sim %>%
    mutate(value = value / 1e6,
           median = median / 1e6)


## Animate plot with original medians, no colour.
##
## This plot shows country orderings according to the simulated value
## but the heights of the bars are fixed at the medians.

rank_sim %>%
    filter(id %in% 11:20) %>%
    ggplot() +
    geom_col(aes(x = rank, y = median, group = name)) +
    geom_text(aes(x = rank, y = -0.1, label = name, group = name),
              hjust = "outward") +
    theme_minimal() + ylab("value (millions)") +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(5,5,5,5),
                             'lines')) +
    coord_flip(clip = "off") +
    transition_states(id,
                      transition_length = 1,
                      state_length = 1)


## Animate plot with simulated values, no colour
##
## This plot sets the bar heights to the simulated values. This shows
## uncertainty in both the values and the orders.

rank_sim %>%
    filter(id %in% 11:20) %>%
    ggplot() +
    geom_col(aes(x = rank, y = value, group = name)) +
    geom_text(aes(x = rank, y = -0.1, label = name, group = name),
              hjust = "outward") +
    theme_minimal() + ylab("value (millions)") +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(5,5,5,5),
                             'lines')) +
    coord_flip(clip = "off") +
    transition_states(id,
                      transition_length = 1,
                      state_length = 1)


## Animate plot with simulated values, colour by name. This is the
## chosen version.
##
## This plot uses colour to better identy the countries.

anim <-
    rank_sim %>%
    mutate(name = reorder(name, mean)) %>%
    filter(id %in% 1:10) %>%
    ggplot() +
    geom_col(aes(x = rank, y = value, group = name, fill = name)) +
    geom_hline(yintercept = 0, size = 1) +
    geom_text(aes(x = rank, y = -0.3, label = name, group = name),
              hjust = "outward", size = 7, family = 'Roboto Slab') +
    theme_minimal(base_size = 20) + ylab("Population size in millions") +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(5,5,5,5),
                             'lines')) +
    scale_y_continuous(limits = c(-3, 18)) +
    scale_x_continuous(name = NULL, breaks = NULL) +
    scale_fill_viridis_d(option = 'C') +
    guides(fill = 'none') +
    coord_flip(clip = "off") +
    ggdark::dark_theme_minimal(base_family = 'Roboto Slab', base_size = 18) +
    theme(panel.grid.major = element_line(colour = 'grey90', linetype = 3)) +
    transition_states(id,
                      transition_length = 1,
                      state_length = 1) +
    labs(
        title = 'Projected population with uncertainty',
        subtitle = 'Females ages 15 to 49 in 2050'
    )

animate(anim, height = 800, width = 800)
anim_save(filename = "out/animated_barchart_colours.gif")
