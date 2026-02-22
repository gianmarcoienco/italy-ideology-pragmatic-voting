# ==========================================
# 00_config.R
# One-time configuration for the whole repo
# Run this before any script in c_program/
# ==========================================

# Repo root
HOME <- normalizePath(getwd(), winslash = "/")

# Match your original folder names
ADO <- file.path(HOME, "00_r_userwritten")
A   <- file.path(HOME, "a_microdata")
D   <- file.path(HOME, "d_results")
DO  <- file.path(HOME, "c_program")

# Create output folders if missing
dir.create(A, showWarnings = FALSE, recursive = TRUE)
dir.create(D, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(A, "temp"), showWarnings = FALSE, recursive = TRUE)

# Minimal fallbacks so scripts don't crash outside RStudio
if (!exists("current_filename")) {
  current_filename <- function() NA_character_
}
if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  rstudioapi <- NULL
}

# Load packages + your functions
source(file.path(ADO, "00_PROGRAMS.R"), echo = FALSE, max = 1000)
