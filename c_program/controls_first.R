# ==========================================================
# controls_first.R
# Prepares municipality-level control variables for the
# first-stage regression using ISTAT data.
# Output: FIRST_STAGE dataset
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


#### CONTROLS FIRST-STAGE 2018 ####

clean_numeric_column <- function(df, colname) {
  df %>%
    mutate(
      !!sym(colname) := as.numeric(gsub(",", "", as.character(.data[[colname]])))
    )
}

prepare_control <- function(df, varname) {
  # Force all year columns to character first
  df <- df %>%
    mutate(across(starts_with("X"), as.character))
  
  df %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "YEAR",
      values_to = varname
    ) %>%
    mutate(
      YEAR = as.numeric(gsub("X", "", YEAR))
    ) %>%
    filter(YEAR == 2018) %>%
    rename(COMUNE = 5) %>%
    clean_numeric_column(varname)
}

# DEMOGRAPHICS
pop_2018         <- prepare_control(pop, "POPULATION")
foreigners_2018  <- prepare_control(foreigners, "FOREIGNERS")
imm_flows_2018   <- prepare_control(imm_flows, "IMM_FLOWS")
index_old_2018   <- prepare_control(index_old, "INDEX_OLD")
index_depOld_2018<- prepare_control(index_depOld, "INDEX_DEP_OLD")

# EDUCATION
tert_edu_2018    <- prepare_control(tert_edu, "TERTIARY_EDU")
sec_edu_2018     <- prepare_control(sec_edu, "SECONDARY_EDU")

# INCOME
irpef_2018       <- prepare_control(irpef, "POOR_SHARE")
income_2018      <- prepare_control(income, "AVG_INCOME")

# POLITICS
women_2018       <- prepare_control(women, "WOMEN_SHARE")
admin_age_2018   <- prepare_control(admin_age, "ADMIN_AGE")
turnout_2018     <- prepare_control(turnout, "LOCAL_TURNOUT")

# CULTURE
libraries_2018   <- prepare_control(libraries, "LIBRARIES")

# SOCIAL
social_exp_2018  <- prepare_control(social_exp, "SOCIAL_EXP")

# TERRITORY
cars_poll_2018   <- prepare_control(cars_poll, "CARS_POLLUTING")
soil_cons_2018   <- prepare_control(soil_cons, "SOIL_CONSUMPTION")
waste_2018       <- prepare_control(waste, "WASTE_SORTING")

# ECONOMY
entrepr_2018     <- prepare_control(entrepr_rate, "ENTREPRENEURSHIP")
firm_density_2018<- prepare_control(firm_density, "FIRM_DENSITY")
agri_2018        <- prepare_control(agriculture, "AGRI_SHARE")
industry_2018    <- prepare_control(industry, "IND_SHARE")
servicesB_2018   <- prepare_control(servicesB, "SERV_B2B_SHARE")
services_2018    <- prepare_control(services, "SERV_SHARE")
high_tech_2018   <- prepare_control(services, "HIGH_TECH")


## Merge ##

multi_merge <- function(dfs) {
  Reduce(function(x, y) merge(x, y), dfs)
}

controls_2018 <- multi_merge(list(
  pop_2018, foreigners_2018, imm_flows_2018, index_old_2018, index_depOld_2018,
  tert_edu_2018, sec_edu_2018,
  irpef_2018, income_2018,
  women_2018, admin_age_2018, turnout_2018,
  libraries_2018,
  social_exp_2018,
  cars_poll_2018, soil_cons_2018, waste_2018,
  entrepr_2018, firm_density_2018, agri_2018,
  industry_2018, servicesB_2018, services_2018, high_tech_2018
))

# Adjusting #

controls_2018 <- controls_2018 %>%
  mutate(
    RIPARTIZIONE = toupper(stri_trans_general(RIPARTIZIONE, "Latin-ASCII")),
    REGIONE = toupper(stri_trans_general(REGIONE, "Latin-ASCII")),
    COD_REG = as.integer(COD_REG),
    PROVINCIA = toupper(stri_trans_general(PROVINCIA, "Latin-ASCII")),
    COMUNE = toupper(stri_trans_general(COMUNE, "Latin-ASCII")),
    COD_COMUNE = as.integer(COD_COMUNE)
  )

controls_2018 <- controls_2018 %>% RENAME() %>% NORMALIZE()

controls_2018 <- controls_2018 %>%
  group_by(COMUNE) %>%
  summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    RIPARTIZIONE = first(RIPARTIZIONE),
    REGIONE = first(REGIONE),
    COD_REG = first(COD_REG),
    PROVINCIA = first(PROVINCIA),
    COD_COMUNE = first(COD_COMUNE),
    YEAR = first(YEAR),
    .groups = "drop"
  )

# Variables #

controls_2018 <- controls_2018 %>%
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

controls_2018 <- controls_2018 %>%
  mutate(across(where(is.character), ~na_if(.x, "NaN"))) %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.x), NA, .x)))   

# Merging with NAT #

NAT_IDEOLOGY <- LOAD(dfinput = "national", pattdir = A)

map_not_matched <- anti_join(controls_2018, NAT_IDEOLOGY, by = "COMUNE")
map_not_matched %>% select(COMUNE)

map_not_matched2 <- anti_join(controls_2018, NAT_IDEOLOGY, by = "COMUNE")
map_not_matched2 %>% select(COMUNE)

df <- left_join(controls_2018, NAT_IDEOLOGY, by = c("COMUNE", "PROVINCIA"))

# Merging with HIST #

HIST_IDEOLOGY <- read.csv("HIST_IDEOLOGY.csv")

df1 <- left_join(df, HIST_IDEOLOGY, by = c("COMUNE", "PROVINCIA", "ID", "COD_PROV", "COD_UTS"))

FIRST_STAGE <- df1 %>%
  select(ID, COD_COMUNE, COMUNE, PROVINCIA, COD_PROV, COD_UTS, REGIONE, COD_REG, RIPARTIZIONE, ABS_INDEX, ABS_INDEX_NAT, ABS_POLAR, ABS_POLAR_SQRT,
         ABS_POLAR_NAT, ABS_POLAR_SQRT_NAT, REL_INDEX, REL_INDEX_NAT, REL_POLAR, REL_POLAR_SQRT, REL_POLAR_NAT, REL_POLAR_SQRT_NAT,
         everything(), -YEAR, -SYNTHETIC_ORIGIN, -SYNTHETIC_NOTE)

colnames(FIRST_STAGE)[2] <- "COD_COM"

SAVE(dfx = FIRST_STAGE, pattdir = A)

write.csv(FIRST_STAGE, file.path(A,"FIRST_STAGE.csv"), row.names = F)
