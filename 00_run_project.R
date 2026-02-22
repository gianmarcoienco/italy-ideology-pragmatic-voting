# ==========================================
# 00_run_project.R
# Entry point for the project pipeline
# ==========================================

rm(list = ls())

# Project root assumed to be the working directory
HOME <- normalizePath(getwd(), winslash = "/")

DO  <- file.path(HOME, "c_program")
ADO <- file.path(HOME, "00_r_userwritten")

# --- Intro / setup ---
source(file.path(DO, "00_intro.R"), echo = TRUE, max = 1000)

# --- Main pipeline ---
source(file.path(DO, "parties.R"), echo = TRUE, max = 1000)
source(file.path(DO, "historical.R"), echo = TRUE, max = 1000)
source(file.path(DO, "national.R"), echo = TRUE, max = 1000)
source(file.path(DO, "administrative.R"), echo = TRUE, max = 1000)
source(file.path(DO, "controls_first.R"), echo = TRUE, max = 1000)
source(file.path(DO, "controls_second.R"), echo = TRUE, max = 1000)
source(file.path(DO, "analysis.R"), echo = TRUE, max = 1000)
