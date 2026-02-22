# Ideological Polarization and Local Pragmatism in Italy

This repository contains code for a municipality-level empirical analysis studying the effect of ideological alignment on non-partisan (civic list) voting in Italy.

The project combines historical electoral data (1948–1981) with contemporary national and municipal elections to construct measures of ideological persistence and test their causal impact using an instrumental variable strategy.

The project involves substantial data harmonization, including manual resolution of municipality name changes, mergers, and inconsistencies across time.

## Repository structure

- `00_config.R`  
  Defines paths and loads required packages and user-written functions.

- `00_run_project.R`  
  Entry point to run the full data construction pipeline.

- `c_program/`  
  Core scripts for data construction and analysis:
  - party classification
  - historical data processing and harmonization
  - national and municipal ideology measures
  - control variables construction
  - IV analysis and robustness checks

- `00_r_userwritten/`  
  Custom functions used throughout the project (e.g. municipality harmonization, ideology construction, data I/O).

- `a_microdata/`  
  Intended location for raw and intermediate datasets (not included).

- `d_results/`  
  Output folder for tables, figures, and logs.

## Data availability

The original datasets are not included in this repository.

The code documents the full data construction process, including:
- harmonization of municipality boundaries over time
- classification of political parties into ideological categories
- construction of historical and contemporary ideology indices

Due to research considerations, the final dataset is not publicly shared.

## How to run

To run the project:

1. Place the required input data in the `a_microdata/` folder  
2. From the repository root, run:

```r
source("00_run_project.R")
