# ==========================================
# 00_run_project.R
# Main entry point
# ==========================================

rm(list = ls())
cat("\014")

source("00_config.R")

# Run the pipeline
source(file.path(DO, "parties.R"), echo = TRUE, max = 1000)
source(file.path(DO, "historical.R"), echo = TRUE, max = 1000)
source(file.path(DO, "national.R"), echo = TRUE, max = 1000)
source(file.path(DO, "administrative.R"), echo = TRUE, max = 1000)
source(file.path(DO, "controls_first.R"), echo = TRUE, max = 1000)
source(file.path(DO, "controls_second.R"), echo = TRUE, max = 1000)
source(file.path(DO, "analysis.R"), echo = TRUE, max = 1000)

# Optional extras (do NOT run by default)
# source(file.path(DO, "bayesian_stan.R"), echo = TRUE, max = 1000)
# source(file.path(DO, "bayesian_stan2.R"), echo = TRUE, max = 1000)
