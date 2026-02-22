# ==========================================================
# administrative.R
# Processes municipal election data, classifies lists into
# ideological/pragmatic categories, and constructs measures
# of local ideological alignment and pragmatic voting.
# Output: ADMIN_IDEOLOGY dataset
# ==========================================================

stopifnot(exists("A"), exists("ADO"))

a2018 <- read.csv("2018_A.csv")
a2019 <- read.csv("2019_A.csv")
a2020 <- read.csv("2020_A.csv")
a2021 <- read.csv("2021_A.csv")
HIST_IDEOLOGY <- read.csv("HIST_IDEOLOGY.csv")

a2018 <- a2018 %>% mutate(local_year = 2018)
a2019 <- a2019 %>% mutate(local_year = 2019)
a2020 <- a2020 %>% mutate(local_year = 2020)
a2021 <- a2021 %>% mutate(local_year = 2021)

admin_elections <- bind_rows(a2018, a2019, a2020, a2021)

admin_elections <- admin_elections %>%
  mutate(year_diff = local_year - 2018)

admin_elections <- admin_elections %>%
  mutate(
    LISTA = ifelse(LISTA == "#NAME?", "+EUROPA", LISTA)
  )

admin_elections <- admin_elections %>%
  filter(TURNO == 1)

admin_elections <- admin_elections %>% RENAME() %>% NORMALIZE()

admin_elections <- admin_elections %>%
  mutate(CANDIDATO = paste(NOME, COGNOME))

# Classify lists based on names
classify_list_ideology <- function(lista_name) {
  lista_upper <- toupper(lista_name)
  
  # Exclude false positives
  if (grepl("\\bCOLLI VERDI\\b", lista_upper)) {
    return("Pragmatic")
  }
  
  if (grepl("\\b(LEGA|SALVINI|FRATELLI D'ITALIA|MELONI|FORZA ITALIA|BERLUSCONI|UDC|POPOLO DELLA FAMIGLIA|CASAPOUND|FORZA NUOVA|FIAMMA TRICOLORE|DESTRA|LIBERTAS|CRISTIAN[IAO]|CATTOLIC[IAO]|LIBERALE|M\\.S\\.I\\.|DESTRA NAZIONALE|CENTRODESTRA)\\b", lista_upper)) {
    return("Conservative")
  } else if (grepl("\\b(PARTITO DEMOCRATICO|MOVIMENTO 5 STELLE|M5S|SOCIALIST[AI]?|COMUNIST[AI]?|SINISTRA|VERDI|ECOLOGIST[AI]?|EUROPA|LIBERI E UGUALI|POTERE AL POPOLO|RIFONDAZIONE|PROGRESSIST[AI]?|EUROPEAN|COMMUNIST|SOCIALIST|CENTROSINISTRA|REPUBBLICANO|PARTITO VALORE UMANO|10 VOLTE MEGLIO)\\b", lista_upper)) {
    return("Progressive")
  } else {
    return("Pragmatic")
  }
}

# Classify each list
admin_elections <- admin_elections %>%
  mutate(IDEOLOGY = sapply(LISTA, classify_list_ideology))

# Identify mixed-motivation municipalities
mixed_comuni <- admin_elections %>%
  group_by(COMUNE) %>%
  summarise(
    has_ideological = any(IDEOLOGY %in% c("Progressive", "Conservative")),
    has_pragmatic   = any(IDEOLOGY == "Pragmatic"),
    .groups = "drop"
  ) %>%
  filter(has_ideological & has_pragmatic) %>%
  pull(COMUNE)

# Filter data to only those municipalities
admin_filtered <- admin_elections %>%
  filter(COMUNE %in% mixed_comuni)

# Compute ideological measures (one row per municipality)
ideology_admin <- IDEOLOGY2(admin_filtered)

ideology_admin <- ideology_admin %>%
  left_join(admin_elections %>% select(COMUNE, local_year, year_diff) %>% distinct(), 
            by = "COMUNE")

ideology_admin <- ideology_admin %>%
  left_join(HIST_IDEOLOGY %>% select(COMUNE, ID, PROVINCIA, COD_PROV, COD_UTS), by = "COMUNE")

skewness(ideology_admin$pragmatic_share)
skewness(ideology_admin$net_absolute_ideology)
skewness(ideology_admin$net_relative_ideology)
skewness(ideology_admin$abs_polarization)
skewness(ideology_admin$rel_polarization)
skewness(ideology_admin$abs_pol_sqrt)
skewness(ideology_admin$rel_pol_sqrt)

ADMIN_IDEOLOGY <- ideology_admin %>%
  select(ID, COMUNE, PROVINCIA, COD_PROV, COD_UTS, net_absolute_ideology, net_relative_ideology, abs_polarization, abs_pol_sqrt,
         rel_polarization, rel_pol_sqrt, pragmatic_share, PROGRESSIVE_PRESENT, CONSERVATIVE_PRESENT, local_year, year_diff) %>%
  rename(
    ABS_INDEX_ADMIN = net_absolute_ideology,
    REL_INDEX_ADMIN = net_relative_ideology,
    ABS_POLAR_ADMIN = abs_polarization,
    ABS_POLAR_SQRT_ADMIN = abs_pol_sqrt,
    REL_POLAR_ADMIN = rel_polarization,
    REL_POLAR_SQRT_ADMIN = rel_pol_sqrt,
    PRAGMATIC_SHARE = pragmatic_share,
    PROG_LISTA = PROGRESSIVE_PRESENT,
    CONS_LISTA = CONSERVATIVE_PRESENT,
    YEAR = local_year,
    YEAR_DIFF = year_diff
  )

ADMIN_IDEOLOGY <- ADMIN_IDEOLOGY %>%
  group_by(COMUNE) %>%
  filter(YEAR == max(YEAR)) %>%
  ungroup()

ADMIN_IDEOLOGY %>%
  count(COMUNE) %>%
  filter(n > 1)

SAVE(dfx = ADMIN_IDEOLOGY, pattdir = A)

write.csv(ADMIN_IDEOLOGY, file.path(A, "ADMIN_IDEOLOGY.csv"), row.names = F)


