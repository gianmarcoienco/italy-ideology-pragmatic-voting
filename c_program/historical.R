# ==========================================================
# historical.R
# Processes historical electoral data (1948–1981), harmonizes
# municipality names, and constructs ideology and polarization
# indices at the municipality level.
# Output: HIST_IDEOLOGY dataset
# ==========================================================

stopifnot(exists("A"), exists("ADO"))

h1948 <- read.csv("1948.csv")
h1953 <- read.csv("1953.csv")
h1958 <- read.csv("1958.csv")
h1963 <- read.csv("1963.csv")
h1968 <- read.csv("1968.csv")
h1972 <- read.csv("1972.csv")
r1974 <- read.csv("1974_R.csv")
r1981 <- read.csv("1981_R.csv")

parties <- LOAD(dfinput = "parties", pattdir = A)

h1948 <- h1948 %>% RENAME() %>% NORMALIZE()
h1953 <- h1953 %>% RENAME() %>% NORMALIZE()
h1958 <- h1958 %>% RENAME() %>% NORMALIZE()
h1963 <- h1963 %>% RENAME() %>% NORMALIZE()
h1968 <- h1968 %>% RENAME() %>% NORMALIZE()
h1972 <- h1972 %>% RENAME() %>% NORMALIZE()
r1974 <- r1974 %>% RENAME() %>% NORMALIZE()
r1981 <- r1981 %>% RENAME() %>% NORMALIZE()


### 1948 ###

h1948 <- h1948 %>%
  left_join(parties, by = c("LISTA" = "PARTY")) %>%
  rename(IDEOLOGY = IDEOLOGY)

ideology_1948 <- IDEOLOGY(h1948)

ideology_1948 %>%
  count(COMUNE) %>%
  filter(n > 1)

### 1953 ###

h1953 <- h1953 %>%
  left_join(parties, by = c("LISTA" = "PARTY")) %>%
  rename(IDEOLOGY = IDEOLOGY)

ideology_1953 <- IDEOLOGY(h1953)

ideology_1953 %>%
  count(COMUNE) %>%
  filter(n > 1)

### 1958 ###

h1958 <- h1958 %>%
  left_join(parties, by = c("LISTA" = "PARTY")) %>%
  rename(IDEOLOGY = IDEOLOGY)

ideology_1958 <- IDEOLOGY(h1958)

ideology_1958 %>%
  count(COMUNE) %>%
  filter(n > 1)

### 1963 ###

h1963 <- h1963 %>%
  left_join(parties, by = c("LISTA" = "PARTY")) %>%
  rename(IDEOLOGY = IDEOLOGY)

ideology_1963 <- IDEOLOGY(h1963)

ideology_1963 %>%
  count(COMUNE) %>%
  filter(n > 1)

### 1968 ###

h1968 <- h1968 %>%
  left_join(parties, by = c("LISTA" = "PARTY")) %>%
  rename(IDEOLOGY = IDEOLOGY)

ideology_1968 <- IDEOLOGY(h1968)

ideology_1968 %>%
  count(COMUNE) %>%
  filter(n > 1)

### 1972 ###

h1972 <- h1972 %>%
  left_join(parties, by = c("LISTA" = "PARTY")) %>%
  rename(IDEOLOGY = IDEOLOGY)

ideology_1972 <- IDEOLOGY(h1972)

ideology_1972 %>%
  count(COMUNE) %>%
  filter(n > 1)

### 1974 DIVORCE ###

# 1: Collapse and clean
r1974_collapsed <- r1974 %>%
  mutate(
    COMUNE = toupper(COMUNE),
    PROVINCIA = toupper(PROVINCIA)
  ) %>%
  group_by(COMUNE) %>%
  summarise(
    ELETTORI = sum(ELETTORI, na.rm = TRUE),
    VOTANTI = sum(VOTANTI, na.rm = TRUE),
    SI = sum(SI, na.rm = TRUE),
    NO = sum(NO, na.rm = TRUE),
    SCHEDE_BIANCHE = sum(SCHEDE_BIANCHE, na.rm = TRUE),
    PROVINCIA = first(PROVINCIA),
    .groups = "drop"
  )

# 2: Clean and compute municipality-level ideology
r1974_clean <- r1974_collapsed %>%
  filter(!is.na(SI), !is.na(NO), (SI + NO) > 0) %>%
  mutate(
    valid_votes = SI + NO,
    abs_ideology_1974 = (SI - NO) / valid_votes,     # YES = Conservative, NO = Progressive
    abs_polarization_1974 = abs(SI - NO) / valid_votes  # Distance from 50-50
  )

# 3: Compute province-level ideology and polarization
province_1974 <- r1974_clean %>%
  group_by(PROVINCIA) %>%
  summarise(
    prov_valid_votes = sum(valid_votes),
    prov_right_votes = sum(SI),
    prov_left_votes = sum(NO),
    prov_abs_ideology_1974 = (prov_right_votes - prov_left_votes) / prov_valid_votes,
    prov_abs_polarization_1974 = abs(prov_right_votes - prov_left_votes) / prov_valid_votes,
    .groups = "drop"
  )

# 4: Compute relative measures
r1974_processed <- r1974_clean %>%
  left_join(province_1974, by = "PROVINCIA") %>%
  mutate(
    rel_ideology_1974 = abs_ideology_1974 - prov_abs_ideology_1974,
    rel_polarization_1974 = abs(abs_polarization_1974 - prov_abs_polarization_1974)
  ) %>%
  select(
    COMUNE,
    abs_ideology_1974, rel_ideology_1974,
    abs_polarization_1974, rel_polarization_1974
  )


### 1981 ABORTION ###

# 1: Collapse and clean
r1981_collapsed <- r1981 %>%
  filter(NUM_REFERENDUM == 5) %>%  # 5 = Abortion Referendum
  mutate(
    COMUNE = toupper(COMUNE),
    PROVINCIA = toupper(PROVINCIA)
  ) %>%
  group_by(COMUNE) %>%
  summarise(
    ELETTORI = sum(ELETTORI, na.rm = TRUE),
    VOTANTI = sum(VOTANTI, na.rm = TRUE),
    SI = sum(SI, na.rm = TRUE),
    NO = sum(NO, na.rm = TRUE),
    SCHEDE_BIANCHE = sum(SCHEDE_BIANCHE, na.rm = TRUE),
    PROVINCIA = first(PROVINCIA),
    .groups = "drop"
  )

# 2: Compute municipality-level ideology
r1981_clean <- r1981_collapsed %>%
  filter(!is.na(SI), !is.na(NO), (SI + NO) > 0) %>%
  mutate(
    valid_votes = SI + NO,
    abs_ideology_1981 = (SI - NO) / valid_votes,  # YES = Conservative, NO = Progressive
    abs_polarization_1981 = abs(SI - NO) / valid_votes
  )

# 3: Compute province-level averages
province_1981 <- r1981_clean %>%
  group_by(PROVINCIA) %>%
  summarise(
    prov_valid_votes = sum(valid_votes),
    prov_right_votes = sum(SI),
    prov_left_votes = sum(NO),
    prov_abs_ideology_1981 = (prov_right_votes - prov_left_votes) / prov_valid_votes,
    prov_abs_polarization_1981 = abs(prov_right_votes - prov_left_votes) / prov_valid_votes,
    .groups = "drop"
  )

# 4: Merge and compute relative values
r1981_processed <- r1981_clean %>%
  left_join(province_1981, by = "PROVINCIA") %>%
  mutate(
    rel_ideology_1981 = abs_ideology_1981 - prov_abs_ideology_1981,
    rel_polarization_1981 = abs(abs_polarization_1981 - prov_abs_polarization_1981)
  ) %>%
  select(
    COMUNE,
    abs_ideology_1981, rel_ideology_1981,
    abs_polarization_1981, rel_polarization_1981
  )

# Duplicate check
ideology_1948 %>% count(COMUNE) %>% filter(n > 1)
ideology_1953 %>% count(COMUNE) %>% filter(n > 1)
ideology_1958 %>% count(COMUNE) %>% filter(n > 1)
ideology_1963 %>% count(COMUNE) %>% filter(n > 1)
ideology_1968 %>% count(COMUNE) %>% filter(n > 1)
ideology_1972 %>% count(COMUNE) %>% filter(n > 1)
r1974_processed %>% count(COMUNE) %>% filter(n > 1)
r1981_processed %>% count(COMUNE) %>% filter(n > 1)


### Merge ###
ideology_referenda <- full_join(
  r1974_processed,
  r1981_processed,
  by = "COMUNE",
  suffix = c("_1974", "_1981")
)

summary(ideology_referenda$rel_ideology_1974)


#### FINAL HISTORICAL IDEOLOGY ###

suffix_year <- function(df, year) {
  df %>%
    rename_with(~ paste0(., "_", year), .cols = -COMUNE)
}

i1948 <- suffix_year(ideology_1948, "1948")
i1953 <- suffix_year(ideology_1953, "1953")
i1958 <- suffix_year(ideology_1958, "1958")
i1963 <- suffix_year(ideology_1963, "1963")
i1968 <- suffix_year(ideology_1968, "1968")
i1972 <- suffix_year(ideology_1972, "1972")
iref  <- ideology_referenda  # already contains year suffixes (1974, 1981)

ideology_full <- list(i1958, i1963, i1968, i1972, iref) %>%
  reduce(full_join, by = "COMUNE")

ideology_full <- ideology_full %>%
  rename(
    net_relative_ideology_1974    = rel_ideology_1974,
    net_relative_ideology_1981    = rel_ideology_1981,
    net_absolute_ideology_1974    = abs_ideology_1974,
    net_absolute_ideology_1981    = abs_ideology_1981,
    rel_polarization_1974         = rel_polarization_1974,
    rel_polarization_1981         = rel_polarization_1981,
    abs_polarization_1974         = abs_polarization_1974,
    abs_polarization_1981         = abs_polarization_1981
  )

ideology_final <- ideology_full %>%
  rowwise() %>%
  mutate(
    rel_index = mean(c_across(matches("net_relative_ideology_(1958|1963|1968|1972|1974|1981)$")), na.rm = TRUE),
    abs_index = mean(c_across(matches("net_absolute_ideology_(1958|1963|1968|1972|1974|1981)$")), na.rm = TRUE),
    
    rel_polarization_index = mean(c_across(matches("rel_polarization_(1958|1963|1968|1972|1974|1981)")), na.rm = TRUE),
    abs_polarization_index = mean(c_across(matches("abs_polarization_(1958|1963|1968|1972|1974|1981)")), na.rm = TRUE),
    
    valid_years = sum(!is.na(c_across(matches("net_relative_ideology_(1958|1963|1968|1972|1974|1981)$"))))
  ) %>%
  ungroup()

# Various checks

check <- ideology_final %>%
  filter(valid_years <= 6)

summary(check$valid_years)

check %>%
  count(valid_years)

names <- read.csv("comuni_names.csv")
suppressed <- read.csv("comuni_sup.csv")

names <- names %>%
  mutate(Denominazione.precedente = toupper(trimws(Denominazione.precedente)))
suppressed <- suppressed %>%
  mutate(Denominazione.Comune = toupper(trimws(Denominazione.Comune)))

check_names <- check %>%
  select(COMUNE, valid_years)
check_sup <- check %>%
  select(COMUNE, valid_years)

check_names <- check_names %>%
  inner_join(names, by = c("COMUNE" = "Denominazione.precedente"))
check_sup <- check_sup %>%
  inner_join(suppressed, by = c("COMUNE" = "Denominazione.Comune"))

check_names <- check_names %>%
  filter(Anno.Evento > 1947) %>%
  filter(Anno.Evento < 2022) %>%
  select(COMUNE, valid_years, Anno.Evento, Data.Evento, Sigla.Provincia.Uts, Nuova.denominazione, Sigla.Provincia.Uts.associata)
check_sup <- check_sup %>%
  filter(Anno > 1947) %>%
  filter(Anno < 2022) %>%
  select(COMUNE, valid_years, Anno, Data.evento, Sigla.Automobilistica, Denominazione.Comune.associato.alla.variazione, Sigla.Automobilistica.associata.alla.variazione)

ideology_final %>%
  count(COMUNE) %>%
  filter(n > 1)

## Synthetic Municipalities ##

synthetic_municipalities <- tibble::tibble(
  new_comune = c("BARANZATE", "BELLIZZI", "CARDEDU", "CASTIADAS", "CAVALLINO-TREPORTI", "ELMAS", "ERULA", "FIUMICINO", "FONTE NUOVA",
                 "LODINE", "MANIACE", "MAPPANO", "MASSA DI SOMMA", "MONSERRATO", "PADRU", "PISCINAS", "QUARTUCCIU", "SAN CESAREO",
                 "SANTA MARIA COGHINAS", "VALLEDORIA", "STATTE", "STINTINO", "TORRENOVA", "RAGALNA", "AVIGLIANO UMBRO", "CASTRO (LE)", "CELLOLE",
                 "CIAMPINO", "ALBAREDO ARNABOLDI", "AMBLAR-DON", "ARMO", "AROSIO", "BARNI", "BENE LARIO", "BIDONI", "BORGIALLO", "BORGO LARES",
                 "BOSCHI SANT'ANNA", "BOSIO", "BRAONE", "BREMBATE DI SOPRA", "BRIENNO", "BULGAROGRASSO", "BUSETO PALIZZOLO", "CAMPO CALABRO",
                 "CAMPODENNO", "CAMPOFELICE DI FITALIA", "CANISCHIO", "CARISOLO", "CASSINA VALSASSINA", "CASTELNUOVO BOZZENTE", "CERRETTO LANGHE",
                 "CERRO VERONESE", "CESINALI", "CHIESANUOVA", "CHIGNOLO D'ISOLA", "CIRO MARINA", "COLLE DI TORA", "COLLERETTO CASTELNUOVO",
                 "COLLERETTO GIACOSA", "COLONNO", "CONCAMARISE", "CONIOLO", "CORTINA SULLA STRADA DEL VINO", "CROVIANA", "CUSTONACI",
                 "DAMBEL", "DORIO", "DOSSO DEL LIRO", "FAEDO", "FAI DELLA PAGANELLA", "FALCONARA MARITTIMA", "FIAVE", "FIEROZZO", "FIESSE", "FILIANO",
                 "FIUMARA", "FONDACHELLI-FANTINA", "FONTENO", "FRASSILONGO", "GABY", "GALLINARO", "GALLODORO", "GERENZANO", "GIUSTINO",
                 "GRINZANE CAVOUR", "IMBERSAGO", "INDUNO OLONA", "LAINO CASTELLO", "LAUREGNO", "LIVO (CO)", "LOCATE VARESINO", "LONA-LASES",
                 "LOSINE", "MAGLIONE", "ACQUEDOLCI", "AGLIENTU", "ALBAGIARA", "ALGUA", "COSTA SERINA", "ARDEA", "ARTA TERME", "BADESI", "BARADILI",
                 "BASSANO IN TEVERINA", "BESANO", "BLUFI", "BORGARELLO", "BORONEDDU", "BOTRUGNO", "BUDONI", "BUGGERRU", "CAPALBIO", "CARAPELLE",
                 "CASAPESENNA", "CAZZANO SANT'ANDREA", "CHIESINA UZZANESE", "COLLI SUL VELINO", "CORNALBA", "CURCURIS", "DUGENTA", "ELINI",
                 "FALCIANO DEL MASSICO", "FALLO", "GENURI", "GIAVERA DEL MONTELLO", "GINESTRA", "GOLFO ARANCI", "GORO", "INARZO", "LA VALLE",
                 "LADISPOLI", "LARIANO", "LIGNANO SABBIADORO", "LOIRI PORTO SAN PAOLO", "LUCINASCO", "MAGLIANO ROMANO", "MARTINSICURO",
                 "MASAINAS", "SANT'ANNA ARRESI", "MASI TORELLO", "MAZZARRONE", "MONTEROTONDO MARITTIMO", "MONTEVECCHIA", "OLTRESSENDA ALTA",
                 "ONORE", "ORDONA", "PALAU", "PATERNO (PZ)", "PERDAXIUS", "PETROSINO", "PIANCOGNO", "PIETRAFERRAZZANA", "POGGIO A CAIANO",
                 "POGGIORSINI", "POLICORO", "POMPU", "SIRIS", "PORTO CESAREO", "PORTOPALO DI CAPO PASSERO", "PREDOI", "PRIOLO GARGALLO",
                 "SAN CASSIANO", "SAN FERDINANDO", "SAN MARCO EVANGELISTA", "SAN PANCRAZIO", "SAN TEODORO", "SANGIANO", "SANT'ANTONIO DI GALLURA",
                 "SANTA MARIA LA CARITA", "SCANZANO JONICO", "SCARLINO", "SCILLATO", "SEMPRONIANO", "SETZU", "SODDI", "SOLZA", "TADASUNI",
                 "TELTI", "TERGU", "TERME VIGLIATORE", "TORRE CANAVESE", "TRECASE", "TREISO", "TRINITA D'AGULTU E VIGNOLA", "VAJONT",
                 "VALLESACCARDA", "VANZAGHELLO", "VELTURNO", "VENEGONO INFERIORE", "VIDDALBA", "VILLA CORTESE", "VILLAPERUCCIO",
                 "VOGHIERA", "ZAPPONETA", "PIARIO", "PRAROSTINO"),
  
  original_comune = c("BOLLATE", "MONTECORVINO ROVELLA", "GAIRO", "SAN VITO", "VENEZIA", "CAGLIARI", "PERFUGAS", "ROMA", "GUIDONIA MONTECELIO",
                      "GAVOI", "BRONTE", "BORGARO TORINESE", "CERCOLA", "CAGLIARI", "BUDDUSO", "GIBA", "CAGLIARI", "ZAGAROLO",
                      "SEDINI", "SEDINI", "TARANTO", "SASSARI", "SAN MARCO D'ALUNZIO", "PATERNO (CT)", "MONTECASTRILLI", "DISO",
                      "SESSA AURUNCA", "MARINO", "CAMPOSPINOSO", "ROMENO", "PIEVE DI TECO", "CARUGO", "BELLAGIO", "GRANDOLA ED UNITI",
                      "NUGHEDU SANTA VITTORIA", "CASTELLAMONTE", "TIONE DI TRENTO", "BEVILACQUA", "PARODI LIGURE", "CERVENO", "PONTE SAN PIETRO",
                      "LAGLIO", "APPIANO GENTILE", "ERICE", "VILLA SAN GIOVANNI", "DENNO", "MEZZOJUSO", "ALPETTE", "CADERZONE TERME", "CREMENO",
                      "BEREGAZZO CON FIGLIARO", "SERRAVALLE LANGHE", "GREZZANA", "AIELLO DEL SABATO", "CASTELLAMONTE", "MADONE", "CIRO",
                      "CASTEL DI TORA", "CASTELLAMONTE", "LORANZE", "SALA COMACINA", "SANGUINETTO", "CASALE MONFERRATO", "TRENTO", "MALE", "ERICE",
                      "SANZENO", "DERVIO", "GRAVEDONA ED UNITI", "SAN MICHELE ALL'ADIGE", "ANDALO", "CHIARAVALLE", "COMANO TERME",
                      "SANT'ORSOLA TERME", "GAMBARA", "AVIGLIANO", "VILLA SAN GIOVANNI", "NOVARA DI SICILIA", "PARZANICA", "SANT'ORSOLA TERME",
                      "ISSIME", "SAN DONATO VAL DI COMINO", "LETOJANNI", "SARONNO", "PINZOLO", "ALBA", "ROBBIATE", "VARESE", "LAINO BORGO",
                      "PROVES", "DOMASO", "MOZZATE", "BEDOLLO", "BRENO", "BORGOMASINO", "SAN FRATELLO", "TEMPIO PAUSANIA", "USELLUS", "BRACCA",
                      "BRACCA", "POMEZIA", "ZUGLIO", "AGGIUS", "BARESSA", "ORTE", "PORTO CERESIO", "PETRALIA SOPRANA", "CERTOSA DI PAVIA",
                      "GHILARZA", "NOCIGLIA", "POSADA", "FLUMINIMAGGIORE", "ORBETELLO", "ORTA NOVA", "SAN CIPRIANO D'AVERSA", "CASNIGO",
                      "UZZANO", "LABRO", "ZOGNO", "ALES", "MELIZZANO", "ILBONO", "CARINOLA", "CIVITALUPARELLA", "TUILI", "ARCADE", "RIPACANDIDA",
                      "OLBIA", "MESOLA", "CASALE LITTA", "SAN MARTINO IN BADIA", "CERVETERI", "VELLETRI", "LATISANA", "TEMPIO PAUSANIA",
                      "CHIUSAVECCHIA", "CAMPAGNANO DI ROMA", "COLONNELLA", "GIBA", "GIBA", "PORTOMAGGIORE", "CALTAGIRONE", "MASSA MARITTIMA",
                      "CERNUSCO LOMBARDONE", "VILLA D'OGNA", "CASTIONE DELLA PRESOLANA", "ORTA NOVA", "TEMPIO PAUSANIA", "MARSICO NUOVO",
                      "NARCAO", "MAZARA DEL VALLO", "BORNO", "COLLEDIMEZZO", "CARMIGNANO", "GRAVINA IN PUGLIA", "MONTALBANO JONICO", "MASULLAS",
                      "MASULLAS", "NARDO", "PACHINO", "VALLE AURINA", "SIRACUSA", "NOCIGLIA", "ROSARNO", "CASERTA", "ULTIMO", "POSADA",
                      "LEGGIUNO", "CALANGIANUS", "GRAGNANO", "MONTALBANO JONICO", "GAVORRANO", "COLLESANO", "ROCCALBEGNA", "TUILI",
                      "GHILARZA", "MEDOLAGO", "GHILARZA", "TEMPIO PAUSANIA", "NULVI", "CASTROREALE", "BAIRO", "BOSCOTRECASE", "BARBARESCO",
                      "AGGIUS", "MANIAGO", "TREVICO", "MAGNAGO", "BRESSANONE", "VENEGONO SUPERIORE", "AGGIUS", "BUSTO GAROLFO", "SANTADI",
                      "PORTOMAGGIORE", "MANFREDONIA", "VILLA D'OGNA", "SAN SECONDO DI PINEROLO"),
  
  note = c("Split from Bollate in 2004", "Split from Montecorvino Rovella in 1990", "Split from Gairo in 1984",
"Split from San Vito and other villages in 1986", "Split from Venezia in 1999", "Split from Cagliari in 1989",
"Split from Perfugas and other territories in 1988", "Split from Roma in 1992", "Split from Guidonia and other territories in 2001",
"Split from Gavoi in 1988", "Split from Bronte in 1981", "Formed from Borgaro, Settimo, and other territories in 2013",
"Split from Cercola in 1988", "Split from Cagliari in 1991", "Split from Budduso in 1996", "Split from Giba in 1988", "Split from Cagliari in 1983",
"Split from Zagarolo in 1990", "Split from Sedini in 1983", "Split from Sedini in 1960", "Split from Taranto in 1993", "Split from Sassari in 1988",
"Split from San Marco in 1984", "Split from Paterno in 1985", "Split from Montecastrilli in 1975", "Split from Diso in 1975",
"Split from Sessa Aurunca in 1975", "Split from Marino in 1974", "Split from Campospinoso in 1948", "Split from Romeno in 1952",
"Split from Pieve di Teco in 1949", "Split from Carugo in 1951", "Split from Bellagio in 1950", "Split from Grandola in 1950",
"Split from Nughedu and other territories in 1949", "Split from Castellamonte and other territories in 1950",
"Split from Tione in 1950", "Split from Bevilaqua in 1948", "Split from Parodi in 1948", "Split from Cerveno and other territories in 1949",
"Split from Ponte San Pietro and other territories in 1950", "Split from Laglio in 1948", "Split from Appiano in 1950", "Split from Erice in 1950",
"Split from Villa San Giovanni in 1950", "Split from Denno in 1952", "Split from Mezzojuso in 1952", "Split from Alpette in 1949",
"Split from Caderzone in 1950", "Split from Cremeno in 1950", "Split from Beregazzo in 1949", "Split from Serravalle in 1950",
"Split from Grezzana in 1948", "Split from Aiello in 1950", "Split from Castellamonte in 1948", "Split from Madone in 1948", "Split from Ciro in 1952",
"Split from Castel di Tora in 1948", "Split from Castellamonte in 1950", "Split from Pedanea (Loranze) in 1948", "Split from Isola (Sala) Comacina",
"Split from Sanguinetto in 1948", "Split from Casale in 1948", "Split from Trento in 1948", "Split from Male in 1948", "Split from Erice in 1948",
"Split from Sanzeno in 1949", "Split from Dervio in 1950", "Split from Gravedon in 1950", "Split from San Michele in 1950", "Split from Andalo in 1949",
"Split from Chiaravalle in 1948", "Split from Lomaso (Comano Terme) in 1952", "Formed from Sant'Orsola Terme and other territories in 1949",
"Split from Gambara in 1948", "Split from Avigliano in 1951", "Split from Villa San Giovanni in 1948", "Split from Novara di Sicilia in 1950",
"Split from Parzanica and other territories in 1948", "Split from Sant'Orsola Terme in 1948", "Split from Issime in 1952",
"Split from San Donato in 1948", "Split from Letojanni in 1952", "Split from Saronno in 1950", "Split from Pinzolo in 1950", "Split from Alba in 1948",
"Split from Robbiate in 1950", "Split from Varese in 1950", "Split from Laino Borgo in 1948", "Split from Proves in 1948",
"Split from Domaso in 1951", "Split from Seprio (Mozzate) in 1950", "Split from Bedollo in 1948", "Split from Breno in 1951",
"Formed from Borgomasino and other territories in 1948", "Split from San Fratello in 1969", "Split from Tempio Pausania in 1959",
"Split from Usellus in 1959", "Split from Bracca di Costa Serina (Bracca) in 1961", "Split from Bracca di Costa Serina (Bracca) in 1961",
"Split from Pomezia in 1970", "Data error with Zuglio acquisition in 1959", "Split from Aggius in 1969", "Split from Baressa in 1958",
"Split from Orte in 1958", "Split from Porto in 1960", "Split from Petralia Soprana in 1972", "Split from Certosa di Pavia in 1958",
"Split from Ghilarza in 1958", "Split from Nociglia in 1958", "Split from Posada in 1958", "Split from Fluminimaggiore in 1960",
"Split from Orbetello in 1960", "Split from Orta Nova in 1958", "Split from San Cirpirano in 1973", "Split from Casnigo in 1959",
"Split from Uzzano in 1963", "Split from Labro in 1958", "Split from Zogno in 1967", "Split from Ales in 1979", "Split from Melizzano in 1958",
"Split from Ilbono in 1958", "Split from Carinola in 1964", "Split from Civitaluparella in 1964", "Split from Tuili in 1958",
"Split from Arcade in 1960", "Split from Ripacandida in 1965", "Split from Olbia in 1979", "Split from Mesola in 1962",
"Split from Casale Litta in 1958", "Split from San Martino in 1965", "Split from Cerveteri in 1970", "Split from Velletri in 1967",
"Split from Latisana in 1959", "Split from Tempio Pausania in 1979", "Split from Chiusavecchia in 1958", "Split from Campagnano in 1958",
"Split from Colonnella in 1963", "Split from Giba (Villarios) in 1975", "Split from Giba (Villarios) in 1964", "Split from Portomaggiore in 1959",
"Split from Caltagirone in 1976", "Split from Massa Marittima in 1961", "Split from Cernusco in 1966", "Split from Villa d'Ogna in 1958",
"Split from Castione in 1958", "Split from Orta Nova in 1975", "Split from Tempio Pausania in 1959", "Split from Marsico Nuovo in 1973",
"Split from Narcao in 1958", "Formed from Mazara del Vallo and Marsala in 1980", "Split from Borno in 1963", "Split from Colledimezzo in 1963",
"Split from Carmignano in 1963", "Split from Gravina in Puglia 1958", "Split from Montalbano in 1959", "Split from Masullas in 1961",
"Split from Masullas in 1961", "Split from Nardo in 1975", "Split from Pachino in 1975", "Split from Valle Aurina in 1958",
"Split from Siracusa in 1979", "Split from Nociglia in 1975", "Split from Rosarno in 1977", "Split from Caserta and Maddaloni in 1977",
"Split from Ultimo in 1960", "Split from Posada in 1959", "Split from Leggiuno in 1970", "Split from Calangianus in 1979",
"Split from Gragnano in 1978", "Split from Montalbano in 1974", "Split from Gavorrano in 1960", "Split from Collesano in 1961",
"Split from Roccalbegna in 1963", "Split from Tuili in 1958", "Split from Ghilarza in 1979", "Split from Rviera d'Adda (Medolago) in 1970",
"Split from Ghilarza in 1958", "Split from Tempio Pausania in 1963", "Formed from Nulvi and other territories in 1980",
"Split from Castroreale in 1966", "Split from Bairo in 1961", "Split from Boscotrecase in 1980", "Split from Barbaresco in 1958",
"Split from Aggius in 1958", "Split from Maniago in 1971", "Split from Trevico in 1958", "Split from Magnago in 1968", "Split from Bressanone in 1958",
"Split from Venegono (Superiore) in 1960", "Split from Aggius in 1975", "Split from Busto Garolfo in 1966", "Split from Santadi in 1979",
"Split from Portomaggiore in 1960", "Split from Manfredonia in 1975", "Split from Villa d'Ogna in 1958", "Split from San Secondo in 1959"

)
)

# Extract the year from the 'note' field
synthetic_municipalities2 <- synthetic_municipalities %>%
  mutate(
    split_year = stringr::str_extract(note, "\\d{4}"), # Extract 4-digit year
    split_year = as.numeric(split_year)                # Convert to numeric
  )

# Filter only those splitted after or during 1958
synthetic_municipalities2 <- synthetic_municipalities2 %>%
  filter(split_year >= 1958)


ideology_final <- SYNTH(synthetic_municipalities2, ideology_final)

ideology_final2 <- ideology_final %>%
  rowwise() %>%
  mutate(
    rel_index = mean(c_across(matches("net_relative_ideology_(1958|1963|1968|1972|1974|1981)$")), na.rm = TRUE),
    abs_index = mean(c_across(matches("net_absolute_ideology_(1958|1963|1968|1972|1974|1981)$")), na.rm = TRUE),
    
    rel_polarization_index = mean(c_across(matches("rel_polarization_(1958|1963|1968|1972|1974|1981)")), na.rm = TRUE),
    abs_polarization_index = mean(c_across(matches("abs_polarization_(1958|1963|1968|1972|1974|1981)")), na.rm = TRUE),
    
    valid_years = sum(!is.na(c_across(matches("net_relative_ideology_(1958|1963|1968|1972|1974|1981)$"))))
  ) %>%
  ungroup()

check2 <- ideology_final2 %>%
  filter(valid_years <= 5)

summary(ideology_final2$rel_index)
summary(ideology_final2$abs_index)

skewness(ideology_final2$abs_index)
skewness(ideology_final2$abs_index)
skewness(ideology_final2$abs_polarization_index)
skewness(ideology_final2$rel_polarization_index)


ggplot(ideology_final2, aes(x = rel_index)) +
  geom_histogram(bins = 60, fill = "gray60", color = "white") +
  labs(title = "Distribution of Relative Ideology Index", x = "rel_index", y = "Count")
ggplot(ideology_final2, aes(x = abs_index)) +
  geom_histogram(bins = 60, fill = "gray60", color = "white") +
  labs(title = "Distribution of Absolute Ideology Index", x = "abs_index", y = "Count")
ggplot(ideology_final2, aes(x = abs_polarization_index)) +
  geom_histogram(bins = 60, fill = "gray60", color = "white") +
  labs(title = "Distribution of Absolute Ideology Index", x = "abs_index", y = "Count")
ggplot(ideology_final2, aes(x = rel_polarization_index)) +
  geom_histogram(bins = 60, fill = "gray60", color = "white") +
  labs(title = "Distribution of Absolute Ideology Index", x = "abs_index", y = "Count")

### MAPPING ####

italy_muni_2019 <- read_sf(file.path(
  A,
  "mapItaly19",
  "Com01012019",
  "Com01012019_WGS84.shp"
))

codes <- read.csv("CODES_ISTAT.csv")

codes <- codes %>%
  mutate(
    COMUNE = toupper(stri_trans_general(DEN_COM, "Latin-ASCII")),
    PROVINCIA = toupper(stri_trans_general(DEN_PROV, "Latin-ASCII"))
  )

codes <- codes %>% RENAME() %>% NORMALIZE()

codes_prov <- codes %>%
  select(COD_PROV, PROVINCIA) %>%
  distinct() %>%
  arrange(COD_PROV)

colnames(codes_prov)[1] <- "COD_UTS"

italy_muni_2019 <- italy_muni_2019 %>%
  left_join(codes_prov, by = "COD_UTS")

italy_muni_2019 <- italy_muni_2019 %>%
  mutate(
    COMUNE = toupper(stri_trans_general(COMUNE, "Latin-ASCII")),
    COD_PROV = as.integer(COD_PROV)
  )

italy_muni_2019 <- italy_muni_2019 %>% RENAME() %>% NORMALIZE()

italy_muni_2019 %>%
  count(COMUNE) %>%
  filter(n > 1)

# Group and merge renamed municipalities
map_merged <- italy_muni_2019 %>%
  group_by(COMUNE) %>%
  summarise(
    geometry = st_union(geometry),  # combine shapes
    SHAPE_AREA = sum(SHAPE_AREA, na.rm = TRUE),
    SHAPE_LEN = sum(SHAPE_LEN, na.rm = TRUE),  # or SHAPE_LENG
    COD_PROV = first(COD_PROV),
    COD_CM = first(COD_CM),
    COD_UTS = first(COD_UTS),
    PRO_COM = paste(unique(PRO_COM), collapse = "; "),
    PRO_COM_T = paste(unique(PRO_COM_T), collapse = "; "),
    COMUNE_A = first(COMUNE_A),
    PROVINCIA = first(PROVINCIA),
    .groups = "drop"
  ) %>%
  mutate(
    SHAPE_AREA = as.numeric(SHAPE_AREA),
    SHAPE_LEN = as.numeric(SHAPE_LEN)
  )

map_merged %>%
  count(COMUNE) %>%
  filter(n > 1)

map_not_matched <- anti_join(map_merged, ideology_final2, by = "COMUNE")
map_not_matched %>% select(COMUNE)

map_not_matched2 <- anti_join(ideology_final2, map_merged, by = "COMUNE")
map_not_matched2 %>% select(COMUNE)

map_ready <- left_join(map_merged, ideology_final2, by = "COMUNE")

map_ready <- map_ready %>%
  mutate(
    abs_index_sqrt = sqrt(abs(abs_index)) * sign(abs_index),
    rel_index_sqrt = sqrt(abs(rel_index)) * sign(rel_index),
    abs_pol_sqrt = sqrt(abs(abs_polarization_index)),
    rel_pol_sqrt = sqrt(abs(rel_polarization_index))
  )

HIST_IDEOLOGY <- map_ready %>%
  st_drop_geometry() %>%
  select(COMUNE, PROVINCIA, COD_PROV, COD_UTS, abs_index, rel_index, abs_polarization_index, abs_pol_sqrt, rel_polarization_index, rel_pol_sqrt, 
         synthetic_origin, synthetic_note) %>%
  mutate(ID = sprintf("COM_%04d", row_number())) %>%                 
  relocate(ID) %>%                                
  rename_with(toupper)

HIST_IDEOLOGY <- HIST_IDEOLOGY %>%
  rename(
    ABS_POLAR = ABS_POLARIZATION_INDEX,
    REL_POLAR = REL_POLARIZATION_INDEX,
    ABS_POLAR_SQRT = ABS_POL_SQRT,
    REL_POLAR_SQRT = REL_POL_SQRT
  )

summary(HIST_IDEOLOGY$ABS_POLAR)

SAVE(dfx = map_ready, pattdir = A)

write.csv(
  HIST_IDEOLOGY,
  file.path(A, "HIST_IDEOLOGY.csv"),
  row.names = FALSE
)

# # 1. Absolute Ideology Map
# ggplot(map_ready) +
#   geom_sf(aes(fill = abs_index), color = NA) +
#   scale_fill_gradient2(
#     low = "red",    # More progressive (negative)
#     mid = "white",  # Neutral
#     high = "blue",  # More conservative (positive)
#     midpoint = 0,
#     limits = c(-max(abs(map_ready$abs_index), na.rm = TRUE),
#                max(abs(map_ready$abs_index), na.rm = TRUE)),
#     name = "Conservative - Progressive\n(Absolute Index)"
#   ) +
#   theme_minimal() +
#   labs(
#     title = "Absolute Ideological Orientation of Municipalities",
#     subtitle = "Red = Progressive, Blue = Conservative",
#     caption = "Map: 2019 boundaries"
#   ) +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(size = 16, face = "bold"),
#     plot.subtitle = element_text(size = 13),
#     plot.caption = element_text(size = 9)
#   )
# 
# # 3. Absolute Polarization Map
# ggplot(map_ready) +
#   geom_sf(aes(fill = abs_polarization_index), color = NA) +
#   scale_fill_gradient(
#     low = "white",
#     high = "green4",
#     name = "Absolute Ideological\nPolarization"
#   ) +
#   theme_minimal() +
#   labs(
#     title = "Municipal Ideological Polarization (Absolute)",
#     subtitle = "White = Centrist, Dark Red = Highly Polarized",
#     caption = "Map: 2019 boundaries"
#   ) +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(size = 16, face = "bold"),
#     plot.subtitle = element_text(size = 13),
#     plot.caption = element_text(size = 9)
#   )


