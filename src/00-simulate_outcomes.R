# code by Mark Wheldon

set.seed(1987)
## Load
data1 <- read.csv(file = "dat/female_pop_2050_Western_Africa.csv")
data1 <- subset(data1, name != "Nigeria")
## Simulate
sims <- lapply(unique(data1$name), function(x, nsim = 1000) {
  z <- subset(data1, name == x)
  mean <- subset(z, stat == "median", select = value, drop = TRUE)
  # choose SD so that the normal distribution roughly matches the
  # given quantiles. in a normal distribution approximately 95%
  # of the probability mass is within 4 SD of the mean
  sd <- 0.25 * (subset(z, stat == "U95", select = value, drop = TRUE) -
                  subset(z, stat == "L95", select = value, drop = TRUE))
  data.frame(value = rnorm(n = nsim, mean = mean, sd = sd),
             id = 1:nsim,
             name = x,
             mean = mean,
             sd = sd)
})
sim_data <- do.call("rbind", sims)
write.csv(sim_data, file = 'dat/simulated_data.csv', row.names = FALSE)
