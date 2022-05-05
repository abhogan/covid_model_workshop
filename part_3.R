##########################################################################
# Run some more nuanced simulations using a wrapper function for nimue::run, which allows us to set some more flexible options:
# - income setting (or change demography/contact patterns/demography)
# - transmission over time
# - vaccine coverage
# - vaccine dose strategy (age groups targeted and prioritisation)
# - vaccines delivered per day
# - vaccine efficacy
# - whether vaccine targets infection, disease, or both
# - time period over which simulation is run
# - hospital capacity

##########################################################################
# Now we will look at an example with some different vaccine options

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
coverage <- c(0.8,0.6, 0)
vaccine_coverage_mat <- "Elderly"
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
                         vaccine_coverage_mat = vaccine_coverage_mat,
                         timing1 = timing1,
                         timing2 = timing2)
# run the model
system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})

# format the output
df <- bind_cols(scenarios, bind_rows(out)) %>%
  unnest(output) %>%
  filter(compartment %in% c("deaths", "infections"))

# create plot
ggplot(data = df, aes(x = t, y = value, col = factor(coverage))) +
  geom_line(size = 0.8) +
  facet_wrap(~compartment, scales = "free") +
  theme_bw() +
  scale_color_viridis_d(end = 0.8)

# what about total events (deaths or infections) over the time period?
df_summary <- df %>%
  group_by(coverage, compartment) %>%
  summarise(value = sum(value, na.rm = T))

ggplot(data = df_summary, aes(x = factor(coverage), y = value, fill = factor(coverage))) +
  geom_bar(stat = "identity") +
  facet_wrap(~compartment, scales = "free") +
  theme_bw() +
  scale_fill_viridis_d(end = 0.8) +
  labs(x = "coverage", y = "total events")

###########################################################################################
# what about impact of changing vaccine efficacy?
coverage <- 0.6
efficacy <- seq(0.3, 1, by = 0.05)

scenarios <- expand_grid(R0 = R0,
                         reduction1 = reduction1,
                         reduction2 = reduction2,
                         coverage = coverage,
                         efficacy = efficacy,
                         income_group = income_group,
                         duration_R = duration_R,
                         vaccine_period = vaccine_period,
                         vaccine_start = vaccine_start,
                         vaccine_coverage_mat = vaccine_coverage_mat,
                         timing1 = timing1,
                         timing2 = timing2)
# run the model
system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})

# format the output
df <- bind_cols(scenarios, bind_rows(out)) %>%
  unnest(output) %>%
  filter(compartment %in% c("deaths", "infections"))

# create plot
ggplot(data = df, aes(x = t, y = value, col = factor(efficacy))) +
  geom_line(size = 0.8) +
  facet_wrap(~compartment, scales = "free") +
  theme_bw() +
  scale_color_viridis_d(end = 0.8)

# what about total events (deaths or infections) over the time period?
df_summary <- df %>%
  group_by(efficacy, compartment) %>%
  summarise(value = sum(value, na.rm = T))

ggplot(data = df_summary, aes(x = factor(efficacy), y = value, fill = factor(efficacy))) +
  geom_bar(stat = "identity") +
  facet_wrap(~compartment, scales = "free") +
  theme_bw() +
  scale_fill_viridis_d(end = 0.8) +
  labs(x = "coverage", y = "total events")

###########################################################################################
# what about impact in different income settings?
