# Install and load the packages we need!
install.packages("tidyverse")
install.packages("nimue")
install.packages("purrr")
install.packages("furrr")

library(tidyverse)
library(purrr)
library(nimue)
library(furrr)

# Load the functions script
source("R/functions.R")

##########################################################################
# in the "Help" window, open the nimue pacakge, and the function "run" to see the options

#########################################################################
# Run the model with an example population and no vaccination
no_vaccine <- nimue::run(country = "United Kingdom", 
                  max_vaccine = 0,
                  R0 = 2.5)

# Format the output selecting infection and deaths
out1 <- format(no_vaccine, compartments = NULL, summaries = c("infections", "deaths")) %>%
  mutate(Name = "No vaccine")

# Plot outputs
ggplot(data = out1, aes(x = t, y = value, group = Name, col = Name)) +
  geom_line(size = 1) +
  facet_wrap(~ compartment, scales = "free_y", ncol = 2) +
  xlim(0, 200) +
  xlab("Time") + 
  theme_bw()

#########################################################################
