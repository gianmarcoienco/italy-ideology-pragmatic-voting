# ==========================================
# 00_run_project.R
# Entry point for the project pipeline
# ==========================================

#clear console
cat("\014")

#clear all globals in memory
rm(list = ls()) 

library(rstudioapi)
MAINNAME <- "run_project"
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  ctx <- try(rstudioapi::getActiveDocumentContext(), silent = TRUE)
  if (!inherits(ctx, "try-error") && nchar(ctx$path) > 0) {
    MAINNAME <- basename(ctx$path)
    MAINNAME <- sub("\\.R$", "", MAINNAME)
  }
}

# Project root assumed to be the working directory
HOME <- normalizePath(getwd(), winslash = "/")
DO <- paste0(HOME, "/c_program/")


######################+
# launch set-up scripts #####
input <- '00_intro.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)
#DEBUG <- T


################################ SCRIPT #########################################

input <- 'parties.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- 'historical.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- 'national.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- 'administrative.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- 'controls_first.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- 'controls_second.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- 'analysis.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)
