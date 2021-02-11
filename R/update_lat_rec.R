source("R/ngs_functions.R")
source("R/save_lateral_yards.R")

save_lateral_yards(most_recent_season())

# purrr::walk(2001:2020, save_lateral_yards)
