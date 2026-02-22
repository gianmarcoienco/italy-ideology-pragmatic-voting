# ==========================================================
# controls_second.R
# Prepares municipality-level control variables for the
# second-stage regression and merges with administrative data.
# Output: SECOND_STAGE dataset
# ==========================================================

stopifnot(exists("A"))

# Demographics
pop <- read.csv("1_population.csv")
colnames(pop)[4] <- "PROVINCIA"
foreigners <- read.csv("1_foreigners.csv")
imm_flows <- read.csv("1_imm_flows.csv")
index_old <- read.csv("1_index_old.csv")
index_depOld <- read.csv("1_index_depOld.csv")

# Education
tert_edu <- read.csv("2_tert_edu.csv")
sec_edu <- read.csv("2_sec_edu.csv")

# Labor Market
emp <- read.csv("3_emp_rate.csv")
unemp <- read.csv("3_unemp_rate.csv")
inactivity <- read.csv("3_inactivity.csv")

# Income
irpef <- read.csv("4_irpef.csv")
income <- read.csv("4_income.csv")

# Politics
women <- read.csv("5_women.csv")
admin_age <- read.csv("5_admin_age.csv")
turnout <- read.csv("5_turnout.csv")

# Culture
libraries <- read.csv("6_libraries.csv")

# Social
social_exp <- read.csv("7_social_exp.csv")

# Territory
cars_poll <- read.csv("8_cars_poll.csv")
soil_cons <- read.csv("8_soil_cons.csv")
waste <- read.csv("8_waste.csv")

# Economy
entrepr_rate <- read.csv("9_entrepr_rate.csv")
firm_density <- read.csv("9_firm_density.csv")
agriculture <- read.csv("9_agriculture_share.csv")
industry <- read.csv("9_industry_share.csv")
servicesB <- read.csv("9_servicesB.csv")
services <- read.csv("9_services.csv")
high_tech <- read.csv("9_high_tech.csv")


#### CONTROLS SECOND-STAGE 2018-2021 ####

clean_numeric_column <- function(df, colname) {
  df %>%
    mutate(
      !!sym(colname) := as.numeric(gsub(",", "", as.character(.data[[colname]])))
    )
}

prepare_control_all_years <- function(df, varname) {
  df <- df %>%
    mutate(across(starts_with("X"), as.character)) %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "YEAR",
      values_to = varname
    ) %>%
    mutate(
      YEAR = as.numeric(gsub("X", "", YEAR))
    ) %>%
    filter(YEAR %in% 2018:2021) %>%
    rename(COMUNE = 5) %>%
    clean_numeric_column(varname)
}

# DEMOGRAPHICS
pop_long         <- prepare_control_all_years(pop, "POPULATION")
foreigners_long  <- prepare_control_all_years(foreigners, "FOREIGNERS")
imm_flows_long   <- prepare_control_all_years(imm_flows, "IMM_FLOWS")
index_old_long   <- prepare_control_all_years(index_old, "INDEX_OLD")
index_depOld_long<- prepare_control_all_years(index_depOld, "INDEX_DEP_OLD")

# EDUCATION
tert_edu_long    <- prepare_control_all_years(tert_edu, "TERTIARY_EDU")
sec_edu_long     <- prepare_control_all_years(sec_edu, "SECONDARY_EDU")

# LABOR MARKET
emp_long         <- prepare_control_all_years(emp, "EMPLOYMENT")
unemp_long       <- prepare_control_all_years(unemp, "UNEMPLOYMENT")
inact_long       <- prepare_control_all_years(inactivity, "INACTIVITY")

# INCOME
irpef_long       <- prepare_control_all_years(irpef, "POOR_SHARE")
income_long      <- prepare_control_all_years(income, "AVG_INCOME")

# POLITICS
women_long       <- prepare_control_all_years(women, "WOMEN_SHARE")
admin_age_long   <- prepare_control_all_years(admin_age, "ADMIN_AGE")
turnout_long     <- prepare_control_all_years(turnout, "LOCAL_TURNOUT")

# CULTURE
libraries_long   <- prepare_control_all_years(libraries, "LIBRARIES")

# SOCIAL
social_exp_long  <- prepare_control_all_years(social_exp, "SOCIAL_EXP")

# TERRITORY
cars_poll_long   <- prepare_control_all_years(cars_poll, "CARS_POLLUTING")
soil_cons_long   <- prepare_control_all_years(soil_cons, "SOIL_CONSUMPTION")
waste_long       <- prepare_control_all_years(waste, "WASTE_SORTING")

# ECONOMY
entrepr_long     <- prepare_control_all_years(entrepr_rate, "ENTREPRENEURSHIP")
firm_density_long<- prepare_control_all_years(firm_density, "FIRM_DENSITY")
agri_long        <- prepare_control_all_years(agriculture, "AGRI_SHARE")
industry_long    <- prepare_control_all_years(industry, "IND_SHARE")
servicesB_long   <- prepare_control_all_years(servicesB, "SERV_B2B_SHARE")
services_long    <- prepare_control_all_years(services, "SERV_SHARE")
high_tech_long   <- prepare_control_all_years(services, "HIGH_TECH")

# Merge #

multi_merge <- function(dfs) {
  Reduce(function(x, y) merge(x, y, by = intersect(names(x), names(y)), all = TRUE), dfs)
}

controls_all_years <- multi_merge(list(
  pop_long, foreigners_long, imm_flows_long, index_old_long, index_depOld_long,
  tert_edu_long, sec_edu_long,
  emp_long, unemp_long, inact_long,
  irpef_long, income_long,
  women_long, admin_age_long, turnout_long,
  libraries_long, social_exp_long,
  cars_poll_long, soil_cons_long, waste_long,
  entrepr_long, firm_density_long,
  agri_long, industry_long, servicesB_long, services_long, high_tech_long
))

controls_all_years <- controls_all_years %>%
  filter(!is.na(LOCAL_TURNOUT))

summary(controls_all_years$POPULATION)

# Variables #

controls_all_years <- controls_all_years %>%
  mutate(
    POP_LOG = log(POPULATION),
    FOREIGN_SHARE = FOREIGNERS / POPULATION,
    INCOME_LOG = log(AVG_INCOME),
    FIRM_LOG = log(FIRM_DENSITY),
    ENTREPR_LOG = log(ENTREPRENEURSHIP),
    MUNICIPAL_SIZE = case_when(
      POPULATION < 2000 ~ "VERY SMALL",
      POPULATION < 10000 ~ "SMALL",
      POPULATION < 60000 ~ "MEDIUM",
      POPULATION < 250000 ~ "LARGE",
      TRUE ~ "METROPOLITAN"
    ),
    MUNICIPAL_SIZE = factor(MUNICIPAL_SIZE, levels = c("VERY SMALL", "SMALL", "MEDIUM", "LARGE", "METROPOLITAN"))
  )


# Adjusting #

controls_all_years <- controls_all_years %>%
  mutate(
    RIPARTIZIONE = toupper(stri_trans_general(RIPARTIZIONE, "Latin-ASCII")),
    REGIONE = toupper(stri_trans_general(REGIONE, "Latin-ASCII")),
    COD_REG = as.integer(COD_REG),
    PROVINCIA = toupper(stri_trans_general(PROVINCIA, "Latin-ASCII")),
    COMUNE = toupper(stri_trans_general(COMUNE, "Latin-ASCII")),
    COD_COMUNE = as.integer(COD_COMUNE)
  )

controls_all_years <- controls_all_years %>% RENAME() %>% NORMALIZE()

controls_all_years <- controls_all_years %>%
  group_by(COMUNE) %>%
  filter(YEAR == max(YEAR, na.rm = TRUE)) %>%
  ungroup()

controls_all_years %>%
  count(COMUNE) %>%
  filter(n > 1)

controls_all_years <- controls_all_years %>%
  mutate(across(where(is.character), ~na_if(.x, "NaN"))) %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.x), NA, .x)))      

# Merging with ADMIN #

ADMIN_IDEOLOGY <- LOAD(dfinput = "administrative", pattdir = A)

not_matched <- anti_join(controls_all_years, ADMIN_IDEOLOGY, by = "COMUNE")
not_matched2 <- anti_join(ADMIN_IDEOLOGY, controls_all_years, by = "COMUNE")

lost_summary <- function(var) {
  not_matched %>%
    group_by({{ var }}) %>%
    summarise(
      num_unmatched = n(),
      .groups = "drop"
    ) %>%
    rename(Category = {{ var }}) %>%
    left_join(
      controls_all_years %>%
        group_by({{ var }}) %>%
        summarise(total = n(), .groups = "drop") %>%
        rename(Category = {{ var }}),
      by = "Category"
    ) %>%
    mutate(share_lost = num_unmatched / total) %>%
    arrange(desc(share_lost))
}

# Generate summaries for each variable
lost_by_size <- lost_summary(MUNICIPAL_SIZE)
lost_by_ripartizione <- lost_summary(RIPARTIZIONE)
lost_by_year <- lost_summary(YEAR)

# Combine into a named list or print each one
list(
  By_Size = lost_by_size,
  By_Ripartizione = lost_by_ripartizione,
  By_Year = lost_by_year
)

not_matched %>%
  select(COMUNE, MUNICIPAL_SIZE, YEAR, REGIONE) %>%
  arrange(MUNICIPAL_SIZE, COMUNE, REGIONE, YEAR)

df <- left_join(controls_all_years, ADMIN_IDEOLOGY, by = c("COMUNE", "PROVINCIA", "YEAR"))

df <- df %>%
  filter(!is.na(ABS_INDEX_ADMIN))

SECOND_STAGE <- df %>%
  select(ID, COD_COMUNE, COMUNE, PROVINCIA, COD_PROV, COD_UTS, REGIONE, COD_REG, RIPARTIZIONE, ABS_INDEX_ADMIN, REL_INDEX_ADMIN, ABS_POLAR_ADMIN,
        ABS_POLAR_SQRT_ADMIN, REL_POLAR_ADMIN, REL_POLAR_SQRT_ADMIN, PRAGMATIC_SHARE, PROG_LISTA, CONS_LISTA, YEAR, YEAR_DIFF, everything())

colnames(SECOND_STAGE)[2] <- "COD_COM"

SAVE(dfx = SECOND_STAGE, pattdir = A)

write.csv(SECOND_STAGE, file.path(A, "SECOND_STAGE.csv"), row.names = F)

