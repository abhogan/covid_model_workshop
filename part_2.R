##########################################################################
# Run some more nuanced simulations using a wrapper function for nimue::run, which allows us to set some more flexible options:
# - income setting
# - transmission over time
# - vaccine dose strategy
# - ...


# Load the functions script
source("R/functions.R")

# set some parameters
R0 <- 2.5
Rt1 <- 0.9
Rt2 <- 3
reduction1 <- 1-Rt1/R0
reduction2 <- 1-Rt2/R0
timing1 <- 50
timing2 <- 350
income_group <- "HIC"
duration_R <- 365
coverage <- 0
vaccine_period <- 30
vaccine_start <- 365

# set up a dataframe of scenarios
scenarios <- expand_grid(R0 = R0,
                         reduction1 = reduction1,
                         reduction2 = reduction2,
                         coverage = coverage,
                         income_group = income_group,
                         duration_R = duration_R,
                         vaccine_period = vaccine_period,
                         vaccine_start = vaccine_start,
                         timing1 = timing1,
                         timing2 = timing2)
# run the model
system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})

# format the output
df <- bind_cols(scenarios, bind_rows(out)) %>%
  unnest(output) %>%
  filter(compartment %in% c("deaths", "infections"))

# create plot
ggplot(data = df, aes(x = t, y = value)) +
  geom_line(size = 0.8) +
  facet_wrap(~compartment, scales = "free") +
  theme_bw()
