CLEARCOND()

MAINNAME <- current_filename()
if(is.null(MAINNAME)){
  MAINNAME <- rstudioapi::getActiveDocumentContext()$path 
}
MAINNAME <- sub(".*/|^[^/]*$", "", MAINNAME)
MAINNAME <- substr(MAINNAME,1,nchar(MAINNAME)-2)
gc()

################################################################################################################+
# MAIN PART ####

setwd(A)

## Adjusting ##

FIRST_STAGE <- LOAD(dfinput = "controls_first", pattdir = A)
SECOND_STAGE <- LOAD(dfinput = "controls_second", pattdir = A)

IV_DATA <- SECOND_STAGE %>%
  left_join(FIRST_STAGE %>% select(ID, COD_COM, COMUNE, PROVINCIA, COD_PROV, COD_UTS, REGIONE, COD_REG, RIPARTIZIONE, ABS_INDEX, ABS_INDEX_NAT,
                                   ABS_POLAR, ABS_POLAR_SQRT, ABS_POLAR_NAT, ABS_POLAR_SQRT_NAT, REL_INDEX, REL_INDEX_NAT, REL_POLAR, REL_POLAR_SQRT,
                                   REL_POLAR_NAT, REL_POLAR_SQRT_NAT))

IV_DATA <- IV_DATA %>%
  mutate(
    MACROAREA = case_when(
      RIPARTIZIONE %in% c("NORD-OVEST", "NORD-EST") ~ "NORTH",
      RIPARTIZIONE == "CENTRO" ~ "CENTER",
      RIPARTIZIONE %in% c("SUD", "ISOLE") ~ "SOUTH"
    ),
    MACROAREA = factor(MACROAREA, levels = c("NORTH", "CENTER", "SOUTH"))
  )

IV_DATA <- IV_DATA %>%
  select(ID, COD_COM, COMUNE, PROVINCIA, COD_PROV, COD_UTS, REGIONE, COD_REG, RIPARTIZIONE, MACROAREA, ABS_INDEX, ABS_INDEX_NAT, ABS_INDEX_ADMIN,
         ABS_POLAR, ABS_POLAR_SQRT, ABS_POLAR_NAT, ABS_POLAR_SQRT_NAT, ABS_POLAR_ADMIN, ABS_POLAR_SQRT_ADMIN, REL_INDEX, REL_INDEX_NAT, REL_INDEX_ADMIN,
         REL_POLAR, REL_POLAR_SQRT, REL_POLAR_NAT, REL_POLAR_SQRT_NAT, REL_POLAR_ADMIN, REL_POLAR_SQRT_ADMIN, PRAGMATIC_SHARE, PROG_LISTA, CONS_LISTA,
         YEAR, YEAR_DIFF, MUNICIPAL_SIZE, POPULATION, POP_LOG, FOREIGNERS, FOREIGN_SHARE, AVG_INCOME, INCOME_LOG, FIRM_DENSITY, FIRM_LOG,
         ENTREPRENEURSHIP, ENTREPR_LOG, everything())

SAVE(dfx = IV_DATA, pattdir = A)


IV_DATA_A1 <- FIRST_STAGE %>%
  left_join(
    SECOND_STAGE %>%
      select(COMUNE, PRAGMATIC_SHARE, PROG_LISTA, CONS_LISTA, REL_INDEX_ADMIN, REL_POLAR_ADMIN, REL_POLAR_SQRT_ADMIN, ABS_INDEX_ADMIN,
             ABS_POLAR_ADMIN, ABS_POLAR_SQRT_ADMIN, YEAR),
    by = "COMUNE"
  )

IV_DATA_A1 <- IV_DATA_A1 %>%
  mutate(
    MACROAREA = case_when(
      RIPARTIZIONE %in% c("NORD-OVEST", "NORD-EST") ~ "NORTH",
      RIPARTIZIONE == "CENTRO" ~ "CENTER",
      RIPARTIZIONE %in% c("SUD", "ISOLE") ~ "SOUTH"
    ),
    MACROAREA = factor(MACROAREA, levels = c("NORTH", "CENTER", "SOUTH"))
  )

IV_DATA_A1 <- IV_DATA_A1 %>%
  select(ID, COD_COM, COMUNE, PROVINCIA, COD_PROV, COD_UTS, REGIONE, COD_REG, RIPARTIZIONE, MACROAREA, ABS_INDEX, ABS_INDEX_NAT, ABS_INDEX_ADMIN,
         ABS_POLAR, ABS_POLAR_SQRT, ABS_POLAR_NAT, ABS_POLAR_SQRT_NAT, ABS_POLAR_ADMIN, ABS_POLAR_SQRT_ADMIN, REL_INDEX, REL_INDEX_NAT, REL_INDEX_ADMIN,
         REL_POLAR, REL_POLAR_SQRT, REL_POLAR_NAT, REL_POLAR_SQRT_NAT, REL_POLAR_ADMIN, REL_POLAR_SQRT_ADMIN, PRAGMATIC_SHARE, PROG_LISTA, CONS_LISTA,
         YEAR, MUNICIPAL_SIZE, POPULATION, POP_LOG, FOREIGNERS, FOREIGN_SHARE, AVG_INCOME, INCOME_LOG, FIRM_DENSITY, FIRM_LOG, ENTREPRENEURSHIP,
         ENTREPR_LOG, everything())

IV_DATA_A1 <- IV_DATA_A1 %>%
  filter(!is.na(PRAGMATIC_SHARE))

vars <- c("ABS_INDEX", "ABS_POLAR", "ABS_INDEX_NAT", 
          "ABS_POLAR_NAT", "ABS_INDEX_ADMIN", "ABS_POLAR_ADMIN", "PRAGMATIC_SHARE")
selected_data <- IV_DATA[ , vars]

cor_matrix <- cor(selected_data, use = "complete.obs", method = "pearson")
print(cor_matrix)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

IV_DATA %>%
  count(MUNICIPAL_SIZE)

IV_DATA$INDEX_OLD <- IV_DATA$INDEX_OLD / 100

IV_DATA_small <- IV_DATA %>%
  filter(!MUNICIPAL_SIZE %in% c("SMALL"))

IV_DATA_small %>%
  count(MACROAREA)

sd(IV_DATA_small$ABS_INDEX_NAT)
quantile(IV_DATA_small$ABS_INDEX_NAT)

sd(IV_DATA_small$PRAGMATIC_SHARE)
quantile(IV_DATA_small$PRAGMATIC_SHARE)

sd(IV_DATA_small$ABS_POLAR_NAT)
quantile(IV_DATA_small$ABS_POLAR_NAT)


IV_DATA %>%
  count(PROG_LISTA, CONS_LISTA)
IV_DATA %>%
  count(CONS_LISTA, MACROAREA)
IV_DATA %>%
  count(PROG_LISTA, MACROAREA)

IV_DATA_small %>%
  count(PROG_LISTA, CONS_LISTA)
IV_DATA_small %>%
  count(CONS_LISTA, MACROAREA)
IV_DATA_small %>%
  count(PROG_LISTA, MACROAREA)


IV_DATA_small %>%
  group_by(MACROAREA, MUNICIPAL_SIZE) %>%
  summarise(average_civic = mean(PRAGMATIC_SHARE, na.rm=T))


IV_DATA_small %>%
group_by(PROVINCIA, MACROAREA, MUNICIPAL_SIZE) %>%
summarise(average_civic = mean(PRAGMATIC_SHARE, na.rm=T))


IV_DATA %>%
  group_by(MACROAREA, MUNICIPAL_SIZE) %>%
  summarise(average_civic = mean(PRAGMATIC_SHARE, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = MUNICIPAL_SIZE, y = average_civic, fill = MACROAREA)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_grey(start = 0.8, end = 0.3) +
  labs(title = "Average Civic Lists Support by Area and Municipality Size",
       x = "Municipality Size",
       y = "Average Civic List Support") +
  geom_text(aes(label = round(average_civic, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.ticks = element_line(color = "gray40"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 8))
  )


dd <- IV_DATA_small %>%
  group_by(PROVINCIA) %>%
  summarize(n_years = n_distinct(YEAR),
            total_municipalities = n()) %>%
  mutate(share_multiple_years = ifelse(n_years > 1, 1, 0)) %>%
  summarise(
    total_provinces = n(),
    provinces_multiple_years = sum(share_multiple_years),
    percentage = (provinces_multiple_years / total_provinces) * 100
  )

write.csv(IV_DATA_small, "/Users/gianmarcoienco/Desktop/personal/projects/project_govt/a_microdata/IV_DATA_small.csv", row.names = F)


#### NESTED IVs ####

# Model 0 – Instrument only
iv_model_0 <- ivreg(PRAGMATIC_SHARE ~ ABS_INDEX_NAT | ABS_INDEX, data = IV_DATA_small)

# Model 1 – Basic controls
iv_model_1 <- ivreg(PRAGMATIC_SHARE ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG |
                      ABS_INDEX + POP_LOG + INCOME_LOG,
                    data = IV_DATA_small)

# Model 2 – Add demographics and macroarea
iv_model_2 <- ivreg(PRAGMATIC_SHARE ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD |
                      ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD,
                    data = IV_DATA_small)

# Model 3 – Full (adds year_diff)
iv_model_3 <- ivreg(PRAGMATIC_SHARE ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
                      ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                    data = IV_DATA_small)

# Model 4 – Robust Full (adds economic + political controls)
iv_model_4 <- ivreg(PRAGMATIC_SHARE ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA +
                      HIGH_TECH + LOCAL_TURNOUT |
                      ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA +
                      HIGH_TECH + LOCAL_TURNOUT,
                    data = IV_DATA_small)

# Export all models to a stargazer table
stargazer(iv_model_0, iv_model_1, iv_model_2, iv_model_3, iv_model_4,
          type = "text",
          title = "Nested IV Models with ABS_INDEX_NAT",
          column.labels = c("No Controls", "Basic", "Extended", "Full", "Robust Full"),
          dep.var.labels = "Pragmatic Share",
          keep.stat = c("n", "rsq", "adj.rsq", "f"),
          digits = 3)

# Diagnostics
summary(iv_model_4, diagnostics = TRUE)
summary(iv_model_3, diagnostics = TRUE)


#### CLUSTERING ####

### PROVINCE LEVEL ###

# Models list
iv_models <- list(iv_model_0, iv_model_1, iv_model_2, iv_model_3, iv_model_4)
model_names <- c("No Controls", "Basic", "Extended", "Full", "Robust Full")

# Cluster variable
cl_prov <- IV_DATA_small$COD_PROV

# Clustered SEs and coeftest summaries
clustered_ses <- lapply(iv_models, vcovCL, cluster = cl_prov)
clustered_summaries <- Map(coeftest, iv_models, clustered_ses)

# Optional: name the outputs
names(clustered_summaries) <- model_names

# Access individual result
clustered_summaries[["Full"]]

coef_map <- c(
  "(Intercept)" = "Intercept",
  "ABS_POLAR_NAT" = "Polarization Index",
  "POP_LOG" = "Log Population",
  "INCOME_LOG" = "Log Income",
  "FOREIGN_SHARE" = "Foreign Share",
  "TERTIARY_EDU" = "Tertiary Education",
  "INDEX_OLD" = "Aging Index",
  "YEAR_DIFF" = "Years Since 2018",
  "MACROAREACENTER" = "Macro-Area: Center",
  "MACROAREASOUTH"  = "Macro-Area: South",
  "HIGH_TECH" = "High-Tech Employment Share",
  "LOCAL_TURNOUT" = "Local Turnout"
)

# Show as a table
modelsummary(
  models = iv_models,
  vcov = clustered_ses,
  coef_map = coef_map,
  stars = TRUE,
  output = "latex", # or "markdown" for console view
  title = "IV Estimates with Clustered Standard Errors",
  gof_omit = "AIC|BIC|Log.Lik"
)

### Correct F-test ###

# Function to extract F-stat, p-value, and significance from first stage
get_fstat_info <- function(controls_formula) {
  fml <- as.formula(paste("ABS_POLAR_NAT ~ ABS_POLAR", controls_formula))
  fs_model <- lm(fml, data = IV_DATA_small)
  fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
  
  wtest <- waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)
  
  f_val <- round(wtest[2, "F"], 2)
  p_val <- round(wtest[2, "Pr(>F)"], 3)
  
  # Significance stars
  stars <- if (p_val < 0.001) {
    "***"
  } else if (p_val < 0.01) {
    "**"
  } else if (p_val < 0.05) {
    "*"
  } else if (p_val < 0.1) {
    "."
  } else {
    ""
  }
  
  return(data.frame(
    F_statistic = f_val,
    p_value = p_val,
    stars = stars
  ))
}

# Control sets for each model
controls_list <- list(
  "",  # Model 0: No controls
  "+ POP_LOG + INCOME_LOG",
  "+ POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD",
  "+ POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA",
  "+ POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA + HIGH_TECH + LOCAL_TURNOUT"
)

# Apply to all models
fstat_table <- do.call(rbind, lapply(controls_list, get_fstat_info))
row.names(fstat_table) <- model_names

# Show results
fstat_table

### Alternative clustering: PROVINCE-YEAR LEVEL ###

# Create province-year cluster ID
IV_DATA_small$cluster_prov_year <- interaction(IV_DATA_small$COD_PROV, IV_DATA_small$YEAR)

# Clustered SEs using province-year clusters
clustered_ses_prov_year <- lapply(iv_models, vcovCL, cluster = IV_DATA_small$cluster_prov_year)

# Compute robust t-tests with clustered SEs
clustered_summaries_prov_year <- Map(coeftest, iv_models, clustered_ses_prov_year)

# Optional: name the outputs for clarity
names(clustered_summaries_prov_year) <- model_names

# Access one result
clustered_summaries_prov_year[["Full"]]





#### PROV-YEAR CLUSTERING ####################

# Create province-year cluster ID
IV_DATA$cluster_prov_year <- interaction(IV_DATA$COD_PROV, IV_DATA$YEAR)
IV_DATA_small$cluster_prov_year <- interaction(IV_DATA_small$COD_PROV, IV_DATA_small$YEAR)


# Define controls (2018-2021 varying)
controls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF"
controls_full <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE"

# Model A.1 – PRAGMATIC_SHARE ~ ABS_INDEX_NAT
iv_model_1 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_INDEX_NAT +", controls, "| ABS_INDEX +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_1, vcov = vcovCL(iv_model_1, cluster = IV_DATA_small$cluster_prov_year)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$cluster_prov_year)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A.2 – PRAGMATIC_SHARE ~ ABS_POLAR_NAT
iv_model_2 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_POLAR_NAT +", controls, "| ABS_POLAR +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_2, vcov = vcovCL(iv_model_2, cluster = IV_DATA_small$cluster_prov_year)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$cluster_prov_year)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

# Model A.3 – PRAGMATIC_SHARE ~ ABS_INDEX_NAT
iv_model_3 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_INDEX_NAT +", controls_full, "| ABS_INDEX +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_3, vcov = vcovCL(iv_model_3, cluster = IV_DATA$cluster_prov_year)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$cluster_prov_year)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A.4 – PRAGMATIC_SHARE ~ ABS_POLAR_NAT
iv_model_4 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_POLAR_NAT +", controls_full, "| ABS_POLAR +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_4, vcov = vcovCL(iv_model_4, cluster = IV_DATA$cluster_prov_year)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$cluster_prov_year)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

models <- list(
  "Pragmatic ~ Index"       = iv_model_1,
  "Pragmatic ~ Polar"       = iv_model_2,
  "Pragmatic ~ Index"     = iv_model_3,
  "Pragmatic ~ Polar"     = iv_model_4
)

modelsummary(models,
             vcov = ~ cluster_prov_year,
             title = "Model comparison",
             output = "latex",
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik")







#### PREDICTION PLOT ####

# Step 1: Generate sequence of ABS_INDEX_NAT values
abs_seq <- seq(min(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               max(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               length.out = 100)

# Step 2: Get average controls
controls_means <- IV_DATA_small %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), \(x) mean(x, na.rm = TRUE)))

# Step 3: Choose a macroarea (e.g., "NORTH")
macroarea <- "NORTH"

# Step 4: Create prediction data frame
pred_df <- data.frame(
  ABS_INDEX_NAT = abs_seq,
  ABS_INDEX = abs_seq,  # for compatibility if needed
  POP_LOG = controls_means$POP_LOG,
  INCOME_LOG = controls_means$INCOME_LOG,
  FOREIGN_SHARE = controls_means$FOREIGN_SHARE,
  TERTIARY_EDU = controls_means$TERTIARY_EDU,
  INDEX_OLD = controls_means$INDEX_OLD,
  YEAR_DIFF = controls_means$YEAR_DIFF,
  MACROAREA = factor(macroarea, levels = levels(IV_DATA_small$MACROAREA))
)

# Step 5: Predict using IV model
pred_df$predicted <- predict(iv_model_3, newdata = pred_df)

# Step 6: Cluster-robust standard errors
robust_vcov <- vcovCL(iv_model_3, cluster = IV_DATA_small$COD_PROV)

# Step 7: Design matrix for robust prediction intervals
X_mat <- model.matrix(~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                        TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df)

# Step 8: Compute standard errors and confidence bands
se_pred <- sqrt(diag(X_mat %*% robust_vcov %*% t(X_mat)))
pred_df$conf.low <- pred_df$predicted - 1.96 * se_pred
pred_df$conf.high <- pred_df$predicted + 1.96 * se_pred

# Step 9: Plot actual data + model predictions
pred_plot_baseline <- ggplot() +
  # Points
  geom_point(data = IV_DATA_small,
             aes(x = ABS_INDEX_NAT, y = PRAGMATIC_SHARE),
             alpha = 0.15, size = 1, color = "gray50") +
  
  # Confidence ribbon and prediction line
  geom_ribbon(data = pred_df,
              aes(x = ABS_INDEX_NAT, ymin = conf.low, ymax = conf.high),
              fill = "gray65", alpha = 0.4) +
  geom_line(data = pred_df,
            aes(x = ABS_INDEX_NAT, y = predicted),
            color = "gray10", linewidth = 1.2) +
  
  # Vertical mean line and annotation
  geom_vline(xintercept = mean(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
             linetype = "dotted", color = "gray20") +
  annotate("text", x = mean(IV_DATA_small$ABS_INDEX_NAT), y = 0.10,
           label = "Sample mean", angle = 90, vjust = -0.5, size = 3.2, family = "serif", color = "gray40") +
  
  # Labels
  labs(
    title = "Predicted Pragmatic Voting by Ideological Orientation",
    subtitle = "IV estimates with average controls (Macro-area = North)",
    x = "National Ideology (Progressive to Conservative)",
    y = "Pragmatic Vote Share"
  ) +
  
  # Scales
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = pretty(IV_DATA_small$ABS_INDEX_NAT, n = 6)) +
  
  # Grayscale serif theme
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.ticks = element_line(color = "gray40"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 8))
  )

# ggsave("pred_plot_baseline_final.png", plot = pred_plot_baseline, width = 6.5, height = 4.5, dpi = 300)

###### ABS_INDEX_ADMIN ~ ABS_INDEX_NAT #########

# Model 0 – Instrument only
iv_model_0 <- ivreg(ABS_INDEX_ADMIN ~ ABS_INDEX_NAT | ABS_INDEX, data = IV_DATA)

# Model 1 – Basic controls
iv_model_1 <- ivreg(ABS_INDEX_ADMIN ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG |
                      ABS_INDEX + POP_LOG + INCOME_LOG,
                    data = IV_DATA)

# Model 2 – Add demographics and macroarea
iv_model_2 <- ivreg(ABS_INDEX_ADMIN ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD |
                      ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD,
                    data = IV_DATA)

# Model 3 – Full (adds year_diff)
iv_model_3 <- ivreg(ABS_INDEX_ADMIN ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA + MUNICIPAL_SIZE |
                      ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA + MUNICIPAL_SIZE,
                    data = IV_DATA)

# Model 4 – Robust Full (adds economic + political controls)
iv_model_4 <- ivreg(ABS_INDEX_ADMIN ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA + MUNICIPAL_SIZE +
                      HIGH_TECH + LOCAL_TURNOUT |
                      ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA + MUNICIPAL_SIZE +
                      HIGH_TECH + LOCAL_TURNOUT,
                    data = IV_DATA)

# Export all models to a stargazer table
stargazer(iv_model_0, iv_model_1, iv_model_2, iv_model_3, iv_model_4,
          type = "text",
          title = "Nested IV Models with ABS_INDEX_NAT",
          column.labels = c("No Controls", "Basic", "Extended", "Full", "Robust Full"),
          dep.var.labels = "Ideological Behavior",
          keep.stat = c("n", "rsq", "adj.rsq", "f"),
          digits = 3)


ggplot(IV_DATA_small, aes(x = ABS_POLAR_NAT, y = PRAGMATIC_SHARE)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess") +
  theme_minimal()


####### PRAGMATIC_SHARE ~ ABS_POLAR_NAT #######################

# Model 0 – Instrument only
iv_model_0 <- ivreg(PRAGMATIC_SHARE ~ ABS_POLAR_NAT | ABS_POLAR, data = IV_DATA_small)

# Model 1 – Basic controls
iv_model_1 <- ivreg(PRAGMATIC_SHARE ~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG |
                      ABS_POLAR + POP_LOG + INCOME_LOG,
                    data = IV_DATA_small)

# Model 2 – Add demographics and macroarea
iv_model_2 <- ivreg(PRAGMATIC_SHARE ~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD |
                      ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD,
                    data = IV_DATA_small)

# Model 3 – Full (adds year_diff)
iv_model_3 <- ivreg(PRAGMATIC_SHARE ~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
                      ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                    data = IV_DATA_small)

# Model 4 – Robust Full (adds economic + political controls)
iv_model_4 <- ivreg(PRAGMATIC_SHARE ~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA +
                      HIGH_TECH + LOCAL_TURNOUT |
                      ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA +
                      HIGH_TECH + LOCAL_TURNOUT,
                    data = IV_DATA_small)

# Export all models to a stargazer table
stargazer(iv_model_0, iv_model_1, iv_model_2, iv_model_3, iv_model_4,
          type = "text",
          title = "Nested IV Models with ABS_INDEX_NAT",
          column.labels = c("No Controls", "Basic", "Extended", "Full", "Robust Full"),
          dep.var.labels = "Pragmatic Share",
          keep.stat = c("n", "rsq", "adj.rsq", "f"),
          digits = 3)

summary(iv_model_4, diagnostics = TRUE)
summary(iv_model_3, diagnostics = TRUE)


#### PREDICTION PLOT (POLARIZATON) ##################

# Step 1: Generate sequence of ABS_POLAR_NAT values
abs_seq <- seq(min(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
               max(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
               length.out = 100)

# Step 2: Get average controls
controls_means <- IV_DATA_small %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), \(x) mean(x, na.rm = TRUE)))

# Step 3: Choose a macroarea (e.g., "NORTH")
macroarea <- "NORTH"

# Step 4: Create prediction data frame
pred_df <- data.frame(
  ABS_POLAR_NAT = abs_seq,
  ABS_POLAR = abs_seq,  # for compatibility if needed
  POP_LOG = controls_means$POP_LOG,
  INCOME_LOG = controls_means$INCOME_LOG,
  FOREIGN_SHARE = controls_means$FOREIGN_SHARE,
  TERTIARY_EDU = controls_means$TERTIARY_EDU,
  INDEX_OLD = controls_means$INDEX_OLD,
  YEAR_DIFF = controls_means$YEAR_DIFF,
  MACROAREA = factor(macroarea, levels = levels(IV_DATA_small$MACROAREA))
)

# Step 5: Predict using IV model
pred_df$predicted <- predict(iv_model_3, newdata = pred_df)

# Step 6: Cluster-robust standard errors
robust_vcov <- vcovCL(iv_model_3, cluster = IV_DATA_small$COD_PROV)

# Step 7: Design matrix for robust prediction intervals
X_mat <- model.matrix(~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                        TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df)

# Step 8: Compute standard errors and confidence bands
se_pred <- sqrt(diag(X_mat %*% robust_vcov %*% t(X_mat)))
pred_df$conf.low <- pred_df$predicted - 1.96 * se_pred
pred_df$conf.high <- pred_df$predicted + 1.96 * se_pred

# Step 9: Plot actual data + model predictions
pred_plot_polar <- ggplot() +
  # Points
  geom_point(data = IV_DATA_small,
             aes(x = ABS_POLAR_NAT, y = PRAGMATIC_SHARE),
             alpha = 0.15, size = 1, color = "gray50") +
  
  # Confidence ribbon and prediction line
  geom_ribbon(data = pred_df,
              aes(x = ABS_POLAR_NAT, ymin = conf.low, ymax = conf.high),
              fill = "gray65", alpha = 0.4) +
  geom_line(data = pred_df,
            aes(x = ABS_POLAR_NAT, y = predicted),
            color = "gray10", linewidth = 1.2) +
  
  # Vertical mean line and annotation
  geom_vline(xintercept = mean(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
             linetype = "dotted", color = "gray20") +
  annotate("text", x = mean(IV_DATA_small$ABS_POLAR_NAT), y = 0.10,
           label = "Sample mean", angle = 90, vjust = -0.5, size = 3.2, family = "serif", color = "gray40") +
  
  # Labels
  labs(
    title = "Predicted Pragmatic Voting by National Polarization",
    subtitle = "IV estimates with average controls (Macro-area = North)",
    x = "National Polarization (Balanced to Dominated)",
    y = "Pragmatic Vote Share"
  ) +
  
  # Scales
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = pretty(IV_DATA_small$ABS_POLAR_NAT, n = 6)) +
  
  # Grayscale serif theme
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.ticks = element_line(color = "gray40"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 8))
  )

# ggsave("pred_plot_polar_final.png", plot = pred_plot_polar, width = 6.5, height = 4.5, dpi = 300)
# ggsave("pred_plot_polar_final.png", plot = pred_plot_polar, width = 8, height = 5, dpi = 300)


###### ABS_POLAR_ADMIN ~ ABS_INDEX_NAT ########################

# Model 0 – Instrument only
iv_model_0 <- ivreg(ABS_POLAR_ADMIN ~ ABS_INDEX_NAT | ABS_INDEX, data = IV_DATA)

# Model 1 – Basic controls
iv_model_1 <- ivreg(ABS_POLAR_ADMIN ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG |
                      ABS_INDEX + POP_LOG + INCOME_LOG,
                    data = IV_DATA)

# Model 2 – Add demographics and macroarea
iv_model_2 <- ivreg(ABS_POLAR_ADMIN ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD |
                      ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD,
                    data = IV_DATA)

# Model 3 – Full (adds year_diff)
iv_model_3 <- ivreg(ABS_POLAR_ADMIN ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA + MUNICIPAL_SIZE |
                      ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA + MUNICIPAL_SIZE,
                    data = IV_DATA)

# Model 4 – Robust Full (adds economic + political controls)
iv_model_4 <- ivreg(ABS_POLAR_ADMIN ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA + MUNICIPAL_SIZE +
                      HIGH_TECH + LOCAL_TURNOUT |
                      ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA + MUNICIPAL_SIZE +
                      HIGH_TECH + LOCAL_TURNOUT,
                    data = IV_DATA)

# Export all models to a stargazer table
stargazer(iv_model_0, iv_model_1, iv_model_2, iv_model_3, iv_model_4,
          type = "text",
          title = "Nested IV Models with ABS_INDEX_NAT",
          column.labels = c("No Controls", "Basic", "Extended", "Full", "Robust Full"),
          dep.var.labels = "Ideological Polarization",
          keep.stat = c("n", "rsq", "adj.rsq", "f"),
          digits = 3)




##################### MODEL COMPARISON ###################

# Define controls (2018-2021 varying)
controls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF"
controls_full <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE"

# Model A.1 – PRAGMATIC_SHARE ~ ABS_INDEX_NAT
iv_model_1 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_INDEX_NAT +", controls, "| ABS_INDEX +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_1, vcov = vcovCL(iv_model_1, cluster = IV_DATA_small$COD_PROV)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A.2 – PRAGMATIC_SHARE ~ ABS_POLAR_NAT
iv_model_2 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_POLAR_NAT +", controls, "| ABS_POLAR +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_2, vcov = vcovCL(iv_model_2, cluster = IV_DATA_small$COD_PROV)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

# Model A.3 – ABS_INDEX_ADMIN ~ ABS_INDEX_NAT
iv_model_3 <- ivreg(
  formula = as.formula(paste("ABS_INDEX_ADMIN ~ ABS_INDEX_NAT +", controls_full, "| ABS_INDEX +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_3, vcov = vcovCL(iv_model_3, cluster = IV_DATA$COD_PROV)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$COD_PROV)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A.4 – ABS_POLAR_ADMIN ~ ABS_POLAR_NAT
iv_model_4 <- ivreg(
  formula = as.formula(paste("ABS_POLAR_ADMIN ~ ABS_POLAR_NAT +", controls_full, "| ABS_POLAR +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_4, vcov = vcovCL(iv_model_4, cluster = IV_DATA$COD_PROV)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$COD_PROV)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

models <- list(
  "Pragmatic ~ Index"       = iv_model_1,
  "Pragmatic ~ Polar"       = iv_model_2,
  "Index_Admin ~ Index"     = iv_model_3,
  "Polar Admin ~ Polar"     = iv_model_4
)

modelsummary(models,
             vcov = ~ COD_PROV,
             title = "Model comparison",
             output = "markdown",
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik")


##################### MODEL COMPARISON 2 ###########################

# Define controls (2018-2021 varying)
controls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF"
controls_full <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE"

# Model A.1 – PRAGMATIC_SHARE ~ ABS_INDEX_NAT
iv_model_1 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_INDEX_NAT +", controls, "| ABS_INDEX +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_1, vcov = vcovCL(iv_model_1, cluster = IV_DATA_small$COD_PROV)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A.2 – PRAGMATIC_SHARE ~ ABS_POLAR_NAT
iv_model_2 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_POLAR_NAT +", controls, "| ABS_POLAR +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_2, vcov = vcovCL(iv_model_2, cluster = IV_DATA_small$COD_PROV)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

# Model A.3 – PRAGMATIC_SHARE ~ ABS_INDEX_NAT
iv_model_3 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_INDEX_NAT +", controls_full, "| ABS_INDEX +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_3, vcov = vcovCL(iv_model_3, cluster = IV_DATA$COD_PROV)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$COD_PROV)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A.4 – PRAGMATIC_SHARE ~ ABS_POLAR_NAT
iv_model_4 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_POLAR_NAT +", controls_full, "| ABS_POLAR +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_4, vcov = vcovCL(iv_model_4, cluster = IV_DATA$COD_PROV)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$COD_PROV)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

models <- list(
  "Pragmatic ~ Polar"       = iv_model_2,
  "Pragmatic ~ Index"       = iv_model_1,
  "Pragmatic ~ Polar"     = iv_model_4,
  "Pragmatic ~ Index"     = iv_model_3
)

modelsummary(models,
             vcov = ~ COD_PROV,
             title = "Model comparison",
             output = "markdown",
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik")



###################### ROBUSTNESS CHECK: RECLASSIFYING M5S AS "OTHER" ###########################

# In this robustness check, we reclassify the Movimento 5 Stelle (M5S) as a non-ideological "Other" actor.
# This affects both national and local classifications:
# - M5S votes are excluded from progressive totals at the national level (ABS_INDEX_NAT)
# - M5S local lists are reclassified as Pragmatic rather than Progressive

# This reclassification results in moderate attenuation of the IV estimates, particularly in:
# - The first-stage: national ideology explains slightly less local ideological variation
# - The second-stage: effect of ABS_INDEX_NAT on pragmatic voting drops from ~0.30 to ~0.20

# The IV coefficient remains positive and statistically significant, suggesting that 
# ideological alignment still reduces pragmatic voting — even when M5S is excluded.

# These results suggest that the core identification strategy is robust to different treatments 
# of ambiguous political actors, though the strength of the estimated relationship is somewhat sensitive.

# Note: no codes to show here as the classification happened in 'parties.R', and the whole code was re-run


######################### ROBUSTNESS CHECK: RELATIVE IDEOLOGY INDICES ############################

# Define controls (2018-2021 varying)
controls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF"
controls_full <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE"

# Model R1 – PRAGMATIC_SHARE ~ REL_INDEX_NAT
iv_model_1_R <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ REL_INDEX_NAT +", controls, "| REL_INDEX +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_1_R, vcov = vcovCL(iv_model_1_R, cluster = IV_DATA_small$COD_PROV)))
fs_model <- lm(REL_INDEX_NAT ~ REL_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
waldtest(fs_model, . ~ . - REL_INDEX, vcov = fs_vcov)

# Model R2 – PRAGMATIC_SHARE ~ REL_INDEX_NAT
iv_model_2_R <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ REL_INDEX_NAT +", controls_full, "| REL_INDEX +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_2_R, vcov = vcovCL(iv_model_2_R, cluster = IV_DATA$COD_PROV)))
fs_model <- lm(REL_INDEX_NAT ~ REL_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$COD_PROV)
waldtest(fs_model, . ~ . - REL_INDEX, vcov = fs_vcov)

# Model R3 – REL_INDEX_ADMIN ~ REL_INDEX_NAT
iv_model_3_R <- ivreg(
  formula = as.formula(paste("REL_INDEX_ADMIN ~ REL_INDEX_NAT +", controls, "| REL_INDEX +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_3_R, vcov = vcovCL(iv_model_3_R, cluster = IV_DATA_small$COD_PROV)))
fs_model <- lm(REL_INDEX_NAT ~ REL_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
waldtest(fs_model, . ~ . - REL_INDEX, vcov = fs_vcov)

# Model R4 – REL_INDEX_ADMIN ~ REL_INDEX_NAT
iv_model_4_R <- ivreg(
  formula = as.formula(paste("REL_INDEX_ADMIN ~ REL_INDEX_NAT +", controls_full, "| REL_INDEX +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_4_R, vcov = vcovCL(iv_model_4_R, cluster = IV_DATA$COD_PROV)))
fs_model <- lm(REL_INDEX_NAT ~ REL_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$COD_PROV)
waldtest(fs_model, . ~ . - REL_INDEX, vcov = fs_vcov)

models <- list(
  "Pragmatic ~ Index"       = iv_model_1_R,
  "Pragmatic ~ Index"       = iv_model_2_R,
  "Index_Admin ~ Index"     = iv_model_3_R,
  "Index_Admin ~ Index"     = iv_model_4_R
)

modelsummary(models,
             vcov = ~ COD_PROV,
             title = "IV Estimates Using REL INDICES",
             output = "latex",
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik")


########################## ROBUSTNESS CHECK: PARTY SUPPLY AS OUTCOME ######################

### PROGRESSIVE LISTS ###

# Define controls (2018-2021 varying)
controls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF"
controls_full <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE"

# Model A.1 – PROG_LISTA ~ ABS_INDEX_NAT
iv_model_1 <- ivreg(
  formula = as.formula(paste("PROG_LISTA ~ ABS_INDEX_NAT +", controls, "| ABS_INDEX +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_1, vcov = vcovCL(iv_model_1, cluster = IV_DATA_small$COD_PROV)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A.2 – PROG_LISTA ~ ABS_POLAR_NAT
iv_model_2 <- ivreg(
  formula = as.formula(paste("PROG_LISTA ~ ABS_POLAR_NAT +", controls, "| ABS_POLAR +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_2, vcov = vcovCL(iv_model_2, cluster = IV_DATA_small$COD_PROV)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

# Model A.3 – PROG_LISTA ~ ABS_INDEX_NAT
iv_model_3 <- ivreg(
  formula = as.formula(paste("PROG_LISTA ~ ABS_INDEX_NAT +", controls_full, "| ABS_INDEX +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_3, vcov = vcovCL(iv_model_3, cluster = IV_DATA$COD_PROV)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$COD_PROV)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A.4 – PROG_LISTA ~ ABS_POLAR_NAT
iv_model_4 <- ivreg(
  formula = as.formula(paste("PROG_LISTA ~ ABS_POLAR_NAT +", controls_full, "| ABS_POLAR +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_4, vcov = vcovCL(iv_model_4, cluster = IV_DATA$COD_PROV)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$COD_PROV)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

models <- list(
  "Prog_list ~ Polar"       = iv_model_2,
  "Prog_list ~ Index"       = iv_model_1,
  "Prog_list ~ Polar"     = iv_model_4,
  "Prog_list ~ Index"     = iv_model_3
)

modelsummary(models,
             vcov = ~ COD_PROV,
             title = "Model comparison",
             output = "markdown",
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik")

### CONSERVATIVE LISTS ###

# Define controls (2018-2021 varying)
controls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF"
controls_full <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE"

# Model A.1 – CONS_LISTA ~ ABS_INDEX_NAT
iv_model_1 <- ivreg(
  formula = as.formula(paste("CONS_LISTA ~ ABS_INDEX_NAT +", controls, "| ABS_INDEX +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_1, vcov = vcovCL(iv_model_1, cluster = IV_DATA_small$COD_PROV)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A.2 – CONS_LISTA ~ ABS_POLAR_NAT
iv_model_2 <- ivreg(
  formula = as.formula(paste("CONS_LISTA ~ ABS_POLAR_NAT +", controls, "| ABS_POLAR +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_2, vcov = vcovCL(iv_model_2, cluster = IV_DATA_small$COD_PROV)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

# Model A.3 – CONS_LISTA ~ ABS_INDEX_NAT
iv_model_3 <- ivreg(
  formula = as.formula(paste("CONS_LISTA ~ ABS_INDEX_NAT +", controls_full, "| ABS_INDEX +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_3, vcov = vcovCL(iv_model_3, cluster = IV_DATA$COD_PROV)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$COD_PROV)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A.4 – CONS_LISTA ~ ABS_POLAR_NAT
iv_model_4 <- ivreg(
  formula = as.formula(paste("CONS_LISTA ~ ABS_POLAR_NAT +", controls_full, "| ABS_POLAR +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_4, vcov = vcovCL(iv_model_4, cluster = IV_DATA$COD_PROV)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$COD_PROV)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

models <- list(
  "Cons_list ~ Polar"       = iv_model_2,
  "Cons_list ~ Index"       = iv_model_1,
  "Cons_list ~ Polar"     = iv_model_4,
  "Cons_list ~ Index"     = iv_model_3
)

modelsummary(models,
             vcov = ~ COD_PROV,
             title = "Model comparison",
             output = "markdown",
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik")



######################### ROBUSTNESS CHECK: PARTY SUPPLY AND THE PRAGMATIC CHANNEL #############################

# This section explores whether the positive association between national conservatism 
# (ABS_INDEX_NAT) and local civic voting (PRAGMATIC_SHARE) is driven by an asymmetry 
# in party supply, particularly the absence of progressive lists in conservative municipalities.
# I (1) describe the prevalence of progressive lists, (2) check for correlation with ideology,
# and (3) test whether progressive list presence moderates or explains the main result
# through interaction and control regressions.

table(IV_DATA_small$CONS_LISTA)
table(IV_DATA_small$PROG_LISTA)
tapply(IV_DATA_small$PRAGMATIC_SHARE, IV_DATA_small$PROG_LISTA, mean, na.rm = TRUE)

cor(IV_DATA_small$ABS_INDEX_NAT, IV_DATA_small$PROG_LISTA, use = "complete.obs")

model_control <- lm(PRAGMATIC_SHARE ~ ABS_INDEX_NAT + PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA, data = IV_DATA_small)
summary(model_control)

model_interact <- lm(PRAGMATIC_SHARE ~ ABS_INDEX_NAT * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                       TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA, data = IV_DATA_small)
summary(model_interact)

iv_model_interact <- ivreg(
  PRAGMATIC_SHARE ~ ABS_INDEX_NAT * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
    ABS_INDEX * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_small
)
summary(iv_model_interact)

model_interact_conservative <- lm(PRAGMATIC_SHARE ~ ABS_INDEX_NAT * CONS_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                                    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA, data = IV_DATA_small)
summary(model_interact_conservative)

ggplot(IV_DATA_small, aes(x = ABS_INDEX_NAT, y = PRAGMATIC_SHARE, color = factor(PROG_LISTA))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Pragmatic Voting vs. National Ideology",
       subtitle = "Colored by Progressive List Presence",
       x = "ABS_INDEX_NAT", y = "PRAGMATIC_SHARE",
       color = "Progressive List Present") +
  theme_minimal()

# The figure shows the relationship between national ideological alignment (ABS_INDEX_NAT) and local pragmatic voting (PRAGMATIC_SHARE),
# conditioned on the presence of progressive lists in municipal elections. In municipalities where no progressive list is present (red),
# higher national conservatism predicts lower pragmatic voting, consistent with ideological alignment. In contrast, where progressive lists
# are present (blue), the relationship is flat or slightly reversed. This suggests that in ideologically conservative areas, pragmatic voting may
# function as a strategic fallback for voters lacking strong partisan alternatives.


### POLARIZATION ###

cor(IV_DATA_small$ABS_POLAR_NAT, IV_DATA_small$PROG_LISTA, use = "complete.obs")

model_control <- lm(PRAGMATIC_SHARE ~ ABS_POLAR_NAT + PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA, data = IV_DATA_small)
summary(model_control)

model_interact <- lm(PRAGMATIC_SHARE ~ ABS_POLAR_NAT * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                       TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA, data = IV_DATA_small)
summary(model_interact)

iv_model_interact <- ivreg(
  PRAGMATIC_SHARE ~ ABS_POLAR_NAT * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
    ABS_POLAR * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_small
)
summary(iv_model_interact)


ggplot(IV_DATA_small, aes(x = ABS_POLAR_NAT, y = PRAGMATIC_SHARE, color = factor(PROG_LISTA))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Pragmatic Voting vs. National Ideology",
       subtitle = "Colored by Progressive List Presence",
       x = "ABS_INDEX_NAT", y = "PRAGMATIC_SHARE",
       color = "Progressive List Present") +
  theme_minimal()



############################ PREDICTION PLOT 2 ########################################

iv_model_interact <- ivreg(
  PRAGMATIC_SHARE ~ ABS_INDEX_NAT * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
    ABS_INDEX * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_small
)

# --- Step 2: Define ABS_INDEX_NAT sequence for prediction ---
abs_seq <- seq(min(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               max(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               length.out = 100)

# --- Step 3: Calculate mean control values ---
controls_means <- IV_DATA_small %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), ~ mean(.x, na.rm = TRUE)))

# --- Step 4: Create prediction data for PROG_LISTA = 0 and 1 ---
pred_list <- lapply(c(0, 1), function(p) {
  df <- data.frame(
    ABS_INDEX_NAT = abs_seq,
    POP_LOG = controls_means$POP_LOG,
    INCOME_LOG = controls_means$INCOME_LOG,
    FOREIGN_SHARE = controls_means$FOREIGN_SHARE,
    TERTIARY_EDU = controls_means$TERTIARY_EDU,
    INDEX_OLD = controls_means$INDEX_OLD,
    YEAR_DIFF = controls_means$YEAR_DIFF,
    MACROAREA = factor("NORTH", levels = levels(IV_DATA_small$MACROAREA)),
    PROG_LISTA = p
  )
  df$GROUP <- ifelse(p == 1, "Prog List Present", "No Prog List")
  return(df)
})

pred_df_interact <- bind_rows(pred_list)

# --- Step 5: Add instrument terms to avoid prediction error ---
pred_df_interact$ABS_INDEX <- pred_df_interact$ABS_INDEX_NAT
pred_df_interact$ABS_INDEX_PROG <- pred_df_interact$ABS_INDEX * pred_df_interact$PROG_LISTA

# --- Step 6: Build model matrix for robust SE calculation ---
X_mat <- model.matrix(~ ABS_INDEX_NAT * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                        TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df_interact)

vcov_robust <- vcovCL(iv_model_interact, cluster = IV_DATA_small$COD_PROV)

# --- Step 7: Predict and compute confidence intervals ---
pred_df_interact$predicted <- predict(iv_model_interact, newdata = pred_df_interact)
se_pred <- sqrt(diag(X_mat %*% vcov_robust %*% t(X_mat)))
pred_df_interact$conf.low <- pred_df_interact$predicted - 1.96 * se_pred
pred_df_interact$conf.high <- pred_df_interact$predicted + 1.96 * se_pred

# --- Step 8: Plot the prediction with observed points ---
ggplot() +
  geom_point(data = IV_DATA_small, aes(x = ABS_INDEX_NAT, y = PRAGMATIC_SHARE, color = factor(PROG_LISTA)), alpha = 0.2) +
  geom_ribbon(data = pred_df_interact, aes(x = ABS_INDEX_NAT, ymin = conf.low, ymax = conf.high, fill = GROUP), alpha = 0.2) +
  geom_line(data = pred_df_interact, aes(x = ABS_INDEX_NAT, y = predicted, color = GROUP), linewidth = 1.2) +
  labs(
    title = "Causal Effect of National Ideology on Pragmatism by Party Supply",
    subtitle = "IV-predicted pragmatic share from interaction model",
    x = "ABS_INDEX_NAT (Progressive → Conservative)",
    y = "Pragmatic Share",
    color = "Progressive List Present",
    fill = "Progressive List Present"
  ) +
  ylim(0, 1) +
  theme_minimal()


# Compute mean line (optional)
mean_val <- mean(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE)


IV_DATA_small$PROG_LISTA <- factor(IV_DATA_small$PROG_LISTA,
                                   levels = c(0, 1),
                                   labels = c("No Progressive List", "Progressive List Present"))

pred_df_interact$GROUP <- factor(pred_df_interact$GROUP,
                                 levels = c("No Prog List", "Prog List Present"),
                                 labels = c("No Progressive List", "Progressive List Present"))

# Plot with custom colors
INTER_PLOT <- ggplot() +
  # Raw data points
  geom_point(data = IV_DATA_small,
             aes(x = ABS_INDEX_NAT, y = PRAGMATIC_SHARE, color = PROG_LISTA),
             alpha = 0.25, size = 1.2) +
  
  # Confidence intervals (shaded ribbon)
  geom_ribbon(data = pred_df_interact,
              aes(x = ABS_INDEX_NAT, ymin = conf.low, ymax = conf.high, fill = GROUP),
              alpha = 0.3) +
  
  # Prediction lines
  geom_line(data = pred_df_interact,
            aes(x = ABS_INDEX_NAT, y = predicted, color = GROUP),
            linewidth = 1.2) +
  
  # Vertical sample mean line
  geom_vline(xintercept = mean(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
             linetype = "dotted", color = "gray40") +
  annotate("text", x = mean(IV_DATA_small$ABS_INDEX_NAT), y = 0.08,
           label = "Sample mean", angle = 90, vjust = -0.5,
           size = 3.2, family = "serif", color = "gray40") +
  
  # Labels and legends
  labs(
    # title = "Predicted Pragmatic Voting by National Ideology and Party Supply",
    # subtitle = "IV estimates with interaction by presence of Progressive List",
    title = "(b) National Ideology",
    x = "National Ideology (Progressive to Conservative)",
    y = "Pragmatic Vote Share",
    color = "Party Supply",
    fill = "Party Supply"
  ) +
  
  # Color and fill mapping
  scale_color_manual(values = c("No Progressive List" = "#1f78b4",  # Blue
                                "Progressive List Present" = "#e31a1c")) +  # Red
  scale_fill_manual(values = c("No Progressive List" = "#1f78b4",
                               "Progressive List Present" = "#e31a1c")) +
  
  # Axis formatting
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = pretty(IV_DATA_small$ABS_INDEX_NAT, n = 6)) +
  
  # Theme
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.ticks = element_line(color = "gray40"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 8)),
    legend.position = "bottom"
  )

print(INTER_PLOT)

ggsave("interaction_plot.png", plot = interaction_plot, width = 6.5, height = 4.5, dpi = 300)




### CONSERVATIVE ###

iv_model_interact_cons <- ivreg(
  PRAGMATIC_SHARE ~ ABS_INDEX_NAT * CONS_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
    ABS_INDEX * CONS_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_small
)

# --- Step 2: Define ABS_INDEX_NAT sequence for prediction ---
abs_seq <- seq(min(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               max(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               length.out = 100)

# --- Step 3: Calculate mean control values ---
controls_means <- IV_DATA_small %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), ~ mean(.x, na.rm = TRUE)))

# --- Step 4: Create prediction data for PROG_LISTA = 0 and 1 ---
pred_list <- lapply(c(0, 1), function(p) {
  df <- data.frame(
    ABS_INDEX_NAT = abs_seq,
    POP_LOG = controls_means$POP_LOG,
    INCOME_LOG = controls_means$INCOME_LOG,
    FOREIGN_SHARE = controls_means$FOREIGN_SHARE,
    TERTIARY_EDU = controls_means$TERTIARY_EDU,
    INDEX_OLD = controls_means$INDEX_OLD,
    YEAR_DIFF = controls_means$YEAR_DIFF,
    MACROAREA = factor("SOUTH", levels = levels(IV_DATA_small$MACROAREA)),
    CONS_LISTA = p
  )
  df$GROUP <- ifelse(p == 1, "Cons List Present", "No Cons List")
  return(df)
})

pred_df_interact <- bind_rows(pred_list)

# --- Step 5: Add instrument terms to avoid prediction error ---
pred_df_interact$ABS_INDEX <- pred_df_interact$ABS_INDEX_NAT
pred_df_interact$ABS_INDEX_PROG <- pred_df_interact$ABS_INDEX * pred_df_interact$CONS_LISTA

# --- Step 6: Build model matrix for robust SE calculation ---
X_mat <- model.matrix(~ ABS_INDEX_NAT * CONS_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                        TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df_interact)

vcov_robust <- vcovCL(iv_model_interact_cons, cluster = IV_DATA_small$COD_PROV)

# --- Step 7: Predict and compute confidence intervals ---
pred_df_interact$predicted <- predict(iv_model_interact_cons, newdata = pred_df_interact)
se_pred <- sqrt(diag(X_mat %*% vcov_robust %*% t(X_mat)))
pred_df_interact$conf.low <- pred_df_interact$predicted - 1.96 * se_pred
pred_df_interact$conf.high <- pred_df_interact$predicted + 1.96 * se_pred

# --- Step 8: Plot the prediction with observed points ---
ggplot() +
  geom_point(data = IV_DATA_small, aes(x = ABS_INDEX_NAT, y = PRAGMATIC_SHARE, color = factor(CONS_LISTA)), alpha = 0.2) +
  geom_ribbon(data = pred_df_interact, aes(x = ABS_INDEX_NAT, ymin = conf.low, ymax = conf.high, fill = GROUP), alpha = 0.2) +
  geom_line(data = pred_df_interact, aes(x = ABS_INDEX_NAT, y = predicted, color = GROUP), linewidth = 1.2) +
  labs(
    title = "Causal Effect of National Ideology on Pragmatism by Party Supply",
    subtitle = "IV-predicted pragmatic share from interaction model",
    x = "ABS_INDEX_NAT (Progressive → Conservative)",
    y = "Pragmatic Share",
    color = "Consevative List Present",
    fill = "Conservative List Present"
  ) +
  ylim(0, 1) +
  theme_minimal()


### NON-LINEAR PROG ###

# --- Step 0: Center the ideology variable and compute squared terms ---
IV_DATA_small <- IV_DATA_small %>%
  mutate(
    ABS_INDEX_NAT_C = scale(ABS_INDEX_NAT, scale = FALSE),
    ABS_INDEX_C = scale(ABS_INDEX, scale = FALSE),
    ABS_INDEX_NAT_C_SQ = ABS_INDEX_NAT_C^2,
    ABS_INDEX_C_SQ = ABS_INDEX_C^2
  )

# --- Step 1: Fit the quadratic interaction IV model ---
iv_model_interact_quad <- ivreg(
  PRAGMATIC_SHARE ~ ABS_INDEX_NAT_C * PROG_LISTA + ABS_INDEX_NAT_C_SQ * PROG_LISTA +
    POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
    ABS_INDEX_C * PROG_LISTA + ABS_INDEX_C_SQ * PROG_LISTA +
    POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_small
)

summary(iv_model_interact_quad)

# --- Step 2: Define a sequence of ABS_INDEX_NAT_C for prediction ---
abs_seq <- seq(min(IV_DATA_small$ABS_INDEX_NAT_C, na.rm = TRUE),
               max(IV_DATA_small$ABS_INDEX_NAT_C, na.rm = TRUE),
               length.out = 100)

# --- Step 3: Calculate mean control values ---
controls_means <- IV_DATA_small %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), ~ mean(.x, na.rm = TRUE)))

# --- Step 4: Create prediction dataset for both PROG_LISTA = 0 and 1 ---
pred_list <- lapply(c(0, 1), function(p) {
  df <- data.frame(
    ABS_INDEX_NAT_C = abs_seq,
    POP_LOG = controls_means$POP_LOG,
    INCOME_LOG = controls_means$INCOME_LOG,
    FOREIGN_SHARE = controls_means$FOREIGN_SHARE,
    TERTIARY_EDU = controls_means$TERTIARY_EDU,
    INDEX_OLD = controls_means$INDEX_OLD,
    YEAR_DIFF = controls_means$YEAR_DIFF,
    MACROAREA = factor("NORTH", levels = levels(IV_DATA_small$MACROAREA)),
    PROG_LISTA = p
  )
  df$ABS_INDEX_NAT_C_SQ <- df$ABS_INDEX_NAT_C^2
  df$GROUP <- ifelse(p == 1, "Prog List Present", "No Prog List")
  return(df)
})

pred_df_interact <- bind_rows(pred_list)

# --- Step 5: Add instrument terms to avoid prediction error ---
pred_df_interact$ABS_INDEX_C <- pred_df_interact$ABS_INDEX_NAT_C
pred_df_interact$ABS_INDEX_C_SQ <- pred_df_interact$ABS_INDEX_C^2

# --- Step 6: Build model matrix for robust SE calculation ---
X_mat_quad <- model.matrix(~ ABS_INDEX_NAT_C * PROG_LISTA + ABS_INDEX_NAT_C_SQ * PROG_LISTA +
                             POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                             INDEX_OLD + YEAR_DIFF + MACROAREA,
                           data = pred_df_interact)

# --- Step 7: Predict values and compute robust confidence intervals ---
vcov_robust <- vcovCL(iv_model_interact_quad, cluster = IV_DATA_small$COD_PROV)

pred_df_interact$predicted <- predict(iv_model_interact_quad, newdata = pred_df_interact)
se_pred_quad <- sqrt(diag(X_mat_quad %*% vcov_robust %*% t(X_mat_quad)))

pred_df_interact$conf.low <- pred_df_interact$predicted - 1.96 * se_pred_quad
pred_df_interact$conf.high <- pred_df_interact$predicted + 1.96 * se_pred_quad

# --- Step 8: Plot the prediction with observed points ---
ggplot() +
  geom_point(data = IV_DATA_small, aes(x = ABS_INDEX_NAT_C, y = PRAGMATIC_SHARE, color = factor(PROG_LISTA)), alpha = 0.2) +
  geom_ribbon(data = pred_df_interact, aes(x = ABS_INDEX_NAT_C, ymin = conf.low, ymax = conf.high, fill = GROUP), alpha = 0.2) +
  geom_line(data = pred_df_interact, aes(x = ABS_INDEX_NAT_C, y = predicted, color = GROUP), linewidth = 1.2) +
  labs(
    title = "Causal Effect of National Ideology on Pragmatism by Party Supply",
    subtitle = "IV-predicted pragmatic share (quadratic interaction model)",
    x = "Centered ABS_INDEX_NAT (Progressive → Conservative)",
    y = "Pragmatic Share",
    color = "Progressive List Present",
    fill = "Progressive List Present"
  ) +
  ylim(0, 1) +
  theme_minimal()


############################ PREDICTION PLOT 2 (BOTH LISTS) #######################

IV_DATA$IDEO_LIST <- ifelse(
  IV_DATA$PROG_LISTA == 1 & IV_DATA$CONS_LISTA == 1 & 
    !is.na(IV_DATA$PROG_LISTA) & !is.na(IV_DATA$CONS_LISTA), 1, 0)

IV_DATA_small$IDEO_LIST <- ifelse(
  IV_DATA_small$PROG_LISTA == 1 & IV_DATA_small$CONS_LISTA == 1 & 
    !is.na(IV_DATA_small$PROG_LISTA) & !is.na(IV_DATA_small$CONS_LISTA), 1, 0)

iv_model_interact <- ivreg(
  PRAGMATIC_SHARE ~ ABS_INDEX_NAT * IDEO_LIST + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
    ABS_INDEX * IDEO_LIST + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_small
)

# --- Step 2: Define ABS_INDEX_NAT sequence for prediction ---
abs_seq <- seq(min(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               max(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               length.out = 100)

# --- Step 3: Calculate mean control values ---
controls_means <- IV_DATA_small %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), ~ mean(.x, na.rm = TRUE)))

# --- Step 4: Create prediction data for IDEO_LIST = 0 and 1 ---
pred_list <- lapply(c(0, 1), function(p) {
  df <- data.frame(
    ABS_INDEX_NAT = abs_seq,
    POP_LOG = controls_means$POP_LOG,
    INCOME_LOG = controls_means$INCOME_LOG,
    FOREIGN_SHARE = controls_means$FOREIGN_SHARE,
    TERTIARY_EDU = controls_means$TERTIARY_EDU,
    INDEX_OLD = controls_means$INDEX_OLD,
    YEAR_DIFF = controls_means$YEAR_DIFF,
    MACROAREA = factor("NORTH", levels = levels(IV_DATA_small$MACROAREA)),
    IDEO_LIST = p
  )
  df$GROUP <- ifelse(p == 1, "Both Lists Present", "No Ideo List")
  return(df)
})

pred_df_interact <- bind_rows(pred_list)

# --- Step 5: Add instrument terms to avoid prediction error ---
pred_df_interact$ABS_INDEX <- pred_df_interact$ABS_INDEX_NAT
pred_df_interact$ABS_INDEX_PROG <- pred_df_interact$ABS_INDEX * pred_df_interact$IDEO_LIST

# --- Step 6: Build model matrix for robust SE calculation ---
X_mat <- model.matrix(~ ABS_INDEX_NAT * IDEO_LIST + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                        TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df_interact)

vcov_robust <- vcovCL(iv_model_interact, cluster = IV_DATA_small$COD_PROV)

# --- Step 7: Predict and compute confidence intervals ---
pred_df_interact$predicted <- predict(iv_model_interact, newdata = pred_df_interact)
se_pred <- sqrt(diag(X_mat %*% vcov_robust %*% t(X_mat)))
pred_df_interact$conf.low <- pred_df_interact$predicted - 1.96 * se_pred
pred_df_interact$conf.high <- pred_df_interact$predicted + 1.96 * se_pred

# --- Step 8: Plot the prediction with observed points ---
ggplot() +
  geom_point(data = IV_DATA_small, aes(x = ABS_INDEX_NAT, y = PRAGMATIC_SHARE, color = factor(IDEO_LIST)), alpha = 0.2) +
  geom_ribbon(data = pred_df_interact, aes(x = ABS_INDEX_NAT, ymin = conf.low, ymax = conf.high, fill = GROUP), alpha = 0.2) +
  geom_line(data = pred_df_interact, aes(x = ABS_INDEX_NAT, y = predicted, color = GROUP), linewidth = 1.2) +
  labs(
    title = "Causal Effect of National Ideology on Pragmatism by Party Supply",
    subtitle = "IV-predicted pragmatic share from interaction model",
    x = "ABS_INDEX_NAT (Progressive → Conservative)",
    y = "Pragmatic Share",
    color = "Both Lists Present",
    fill = "Both Lists Present"
  ) +
  ylim(0, 1) +
  theme_minimal()


# Compute mean line (optional)
mean_val <- mean(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE)


IV_DATA_small$PROG_LISTA <- factor(IV_DATA_small$PROG_LISTA,
                                   levels = c(0, 1),
                                   labels = c("No Progressive List", "Progressive List Present"))

pred_df_interact$GROUP <- factor(pred_df_interact$GROUP,
                                 levels = c("No Prog List", "Prog List Present"),
                                 labels = c("No Progressive List", "Progressive List Present"))

# Plot with custom colors
interaction_plot <- ggplot() +
  # Raw data points
  geom_point(data = IV_DATA_small,
             aes(x = ABS_INDEX_NAT, y = PRAGMATIC_SHARE, color = PROG_LISTA),
             alpha = 0.25, size = 1.2) +
  
  # Confidence intervals (shaded ribbon)
  geom_ribbon(data = pred_df_interact,
              aes(x = ABS_INDEX_NAT, ymin = conf.low, ymax = conf.high, fill = GROUP),
              alpha = 0.3) +
  
  # Prediction lines
  geom_line(data = pred_df_interact,
            aes(x = ABS_INDEX_NAT, y = predicted, color = GROUP),
            linewidth = 1.2) +
  
  # Vertical sample mean line
  geom_vline(xintercept = mean(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
             linetype = "dotted", color = "gray40") +
  annotate("text", x = mean(IV_DATA_small$ABS_INDEX_NAT), y = 0.08,
           label = "Sample mean", angle = 90, vjust = -0.5,
           size = 3.2, family = "serif", color = "gray40") +
  
  # Labels and legends
  labs(
    title = "Predicted Pragmatic Voting by National Ideology and Party Supply",
    subtitle = "IV estimates with interaction by presence of Progressive List",
    x = "National Ideology (Progressive to Conservative)",
    y = "Pragmatic Vote Share",
    color = "Party Supply",
    fill = "Party Supply"
  ) +
  
  # Color and fill mapping
  scale_color_manual(values = c("No Progressive List" = "#1f78b4",  # Blue
                                "Progressive List Present" = "#e31a1c")) +  # Red
  scale_fill_manual(values = c("No Progressive List" = "#1f78b4",
                               "Progressive List Present" = "#e31a1c")) +
  
  # Axis formatting
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = pretty(IV_DATA_small$ABS_INDEX_NAT, n = 6)) +
  
  # Theme
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.ticks = element_line(color = "gray40"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 8)),
    legend.position = "bottom"
  )

print(interaction_plot)


############################ PREDICTION PLOT 3 (POLARIZATION - INTERACTION) ##############################

iv_model_3 <- ivreg(PRAGMATIC_SHARE ~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
                      ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                    data = IV_DATA_small)

# Step 1: Generate sequence of ABS_POLAR_NAT values
abs_seq <- seq(min(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
               max(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
               length.out = 100)

# Step 2: Get average controls
controls_means <- IV_DATA %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), \(x) mean(x, na.rm = TRUE)))

# Step 3: Choose a macroarea (e.g., "NORTH")
macroarea <- "NORTH"

# Step 4: Create prediction data frame
pred_df <- data.frame(
  ABS_POLAR_NAT = abs_seq,
  ABS_POLAR = abs_seq,  # for compatibility if needed
  POP_LOG = controls_means$POP_LOG,
  INCOME_LOG = controls_means$INCOME_LOG,
  FOREIGN_SHARE = controls_means$FOREIGN_SHARE,
  TERTIARY_EDU = controls_means$TERTIARY_EDU,
  INDEX_OLD = controls_means$INDEX_OLD,
  YEAR_DIFF = controls_means$YEAR_DIFF,
  MACROAREA = factor(macroarea, levels = levels(IV_DATA_small$MACROAREA))
)

# Step 5: Predict using IV model
pred_df$predicted <- predict(iv_model_3, newdata = pred_df)

# Step 6: Cluster-robust standard errors
robust_vcov <- vcovCL(iv_model_3, cluster = IV_DATA_small$COD_PROV)

# Step 7: Design matrix for robust prediction intervals
X_mat <- model.matrix(~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                        TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df)

# Step 8: Compute standard errors and confidence bands
se_pred <- sqrt(diag(X_mat %*% robust_vcov %*% t(X_mat)))
pred_df$conf.low <- pred_df$predicted - 1.96 * se_pred
pred_df$conf.high <- pred_df$predicted + 1.96 * se_pred

# Step 9: Plot actual data + model predictions (FIXED)
ggplot() +
  geom_point(data = IV_DATA_small, aes(x = ABS_POLAR_NAT, y = PRAGMATIC_SHARE), alpha = 0.2) +
  geom_ribbon(data = pred_df, aes(x = ABS_POLAR_NAT, ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.2) +
  geom_line(data = pred_df, aes(x = ABS_POLAR_NAT, y = predicted), color = "darkblue", linewidth = 1.2) +
  geom_vline(xintercept = mean(IV_DATA$ABS_POLAR_NAT, na.rm = TRUE), linetype = "dotted", color = "gray") +
  labs(
    title = "IV-Predicted vs Observed Ideological Voting (ABS_INDEX_ADMIN)",
    subtitle = "Prediction based on average controls, assuming MACROAREA = North",
    x = "ABS_POLAR_NAT (Balanced to Dominated)",
    y = "PRAGMATIC_SHARE"
  ) +
  theme_minimal()


### INTERACTION ###

# --- Step 1: Estimate IV model with interaction ---
iv_model_admin_interact <- ivreg(
  PRAGMATIC_SHARE ~ ABS_POLAR_NAT * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
    ABS_POLAR * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_small
)

summary(iv_model_admin_interact)

# --- Step 2: Generate ABS_POLAR_NAT sequence ---
abs_seq <- seq(min(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
               max(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
               length.out = 100)


# --- Step 3: Get average control values ---
controls_means <- IV_DATA_small %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), \(x) mean(x, na.rm = TRUE)))

# --- Step 4: Create prediction data for PROG_LISTA = 0 and 1 ---
pred_list <- lapply(c(0, 1), function(p) {
  data.frame(
    ABS_POLAR_NAT = abs_seq,
    ABS_POLAR = abs_seq,  # for instrument compatibility
    PROG_LISTA = p,
    POP_LOG = controls_means$POP_LOG,
    INCOME_LOG = controls_means$INCOME_LOG,
    FOREIGN_SHARE = controls_means$FOREIGN_SHARE,
    TERTIARY_EDU = controls_means$TERTIARY_EDU,
    INDEX_OLD = controls_means$INDEX_OLD,
    YEAR_DIFF = controls_means$YEAR_DIFF,
    MACROAREA = factor("NORTH", levels = levels(IV_DATA_small$MACROAREA)),
    GROUP = ifelse(p == 1, "Prog List Present", "No Prog List")
  )
})

pred_df_interact <- bind_rows(pred_list)

# --- Step 5: Compute robust SE matrix and predictions ---
vcov_robust <- vcovCL(iv_model_admin_interact, cluster = IV_DATA_small$COD_PROV)

X_mat <- model.matrix(~ ABS_POLAR_NAT * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                        TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df_interact)

pred_df_interact$predicted <- predict(iv_model_admin_interact, newdata = pred_df_interact)

se_pred <- sqrt(diag(X_mat %*% vcov_robust %*% t(X_mat)))

pred_df_interact$conf.low <- pred_df_interact$predicted - 1.96 * se_pred
pred_df_interact$conf.high <- pred_df_interact$predicted + 1.96 * se_pred

# --- Step 6: Plot ---
ggplot() +
  geom_point(data = IV_DATA_small, aes(x = ABS_POLAR_NAT, y = PRAGMATIC_SHARE, color = factor(PROG_LISTA)), alpha = 0.2) +
  geom_ribbon(data = pred_df_interact, aes(x = ABS_POLAR_NAT, ymin = conf.low, ymax = conf.high, fill = GROUP), alpha = 0.2) +
  geom_line(data = pred_df_interact, aes(x = ABS_POLAR_NAT, y = predicted, color = GROUP), linewidth = 1.2) +
  geom_vline(xintercept = mean(IV_DATA$ABS_POLAR_NAT, na.rm = TRUE), linetype = "dotted", color = "gray") +
  labs(
    title = "IV-Predicted Local Ideology by National Ideology and Party Supply",
    subtitle = "Interaction between ABS_POLAR_NAT and Progressive List Presence",
    x = "ABS_POLAR_NAT (Balanced → Dominaned)",
    y = "PRAGMATIC_SHARE",
    color = "Progressive List Present",
    fill = "Progressive List Present"
  ) +
  theme_minimal()



# Compute mean line (optional)
mean_val <- mean(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE)


IV_DATA_small$PROG_LISTA <- factor(IV_DATA_small$PROG_LISTA,
                                   levels = c(0, 1),
                                   labels = c("No Progressive List", "Progressive List Present"))

pred_df_interact$GROUP <- factor(pred_df_interact$GROUP,
                                 levels = c("No Prog List", "Prog List Present"),
                                 labels = c("No Progressive List", "Progressive List Present"))

# Plot with custom colors
interaction_plot2 <- ggplot() +
  # Raw data points
  geom_point(data = IV_DATA_small,
             aes(x = ABS_POLAR_NAT, y = PRAGMATIC_SHARE, color = PROG_LISTA),
             alpha = 0.25, size = 1.2) +
  
  # Confidence intervals (shaded ribbon)
  geom_ribbon(data = pred_df_interact,
              aes(x = ABS_POLAR_NAT, ymin = conf.low, ymax = conf.high, fill = GROUP),
              alpha = 0.3) +
  
  # Prediction lines
  geom_line(data = pred_df_interact,
            aes(x = ABS_POLAR_NAT, y = predicted, color = GROUP),
            linewidth = 1.2) +
  
  # Vertical sample mean line
  geom_vline(xintercept = mean(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
             linetype = "dotted", color = "gray40") +
  annotate("text", x = mean(IV_DATA_small$ABS_POLAR_NAT), y = 0.08,
           label = "Sample mean", angle = 90, vjust = -0.5,
           size = 3.2, family = "serif", color = "gray40") +
  
  # Labels and legends
  labs(
    # title = "Predicted Pragmatic Voting by National Polarization and Party Supply",
    # subtitle = "IV estimates with interaction by presence of Progressive List",
    title = "(a) National Polarization",
    x = "National Polarization (Balanced to Dominated)",
    y = "Pragmatic Vote Share",
    color = "Party Supply",
    fill = "Party Supply"
  ) +
  
  # Color and fill mapping
  scale_color_manual(values = c("No Progressive List" = "#1f78b4",  # Blue
                                "Progressive List Present" = "#e31a1c")) +  # Red
  scale_fill_manual(values = c("No Progressive List" = "#1f78b4",
                               "Progressive List Present" = "#e31a1c")) +
  
  # Axis formatting
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = pretty(IV_DATA_small$ABS_POLAR_NAT, n = 6)) +
  
  # Theme
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.ticks = element_line(color = "gray40"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 8)),
    legend.position = "bottom"
  )

print(interaction_plot2)

ggsave("interaction_plot2.png", plot = interaction_plot2, width = 6.5, height = 4.5, dpi = 300)

combined_plot <- ggarrange(interaction_plot2,
                           INTER_PLOT,
                           nrow = 1,
                           ncol = 2)

print(combined_plot)
ggsave("combined_plot_inter.png", plot = combined_plot, width = 12, height = 4.5, dpi = 300)


################## PLACEBO TESTS ##########################

## RESTRICTED SAMPLE

# Model 0 – Instrument only
iv_model_0 <- ivreg(CARS_POLLUTING ~ ABS_INDEX_NAT | ABS_INDEX, data = IV_DATA_small)

# Model 1 – Basic controls
iv_model_1 <- ivreg(CARS_POLLUTING ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG |
                      ABS_INDEX + POP_LOG + INCOME_LOG,
                    data = IV_DATA_small)

# Model 2 – Add demographics and macroarea
iv_model_2 <- ivreg(CARS_POLLUTING ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD |
                      ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD,
                    data = IV_DATA_small)

# Model 3 – Full (adds year_diff)
iv_model_3 <- ivreg(CARS_POLLUTING ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
                      ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                    data = IV_DATA_small)

# Model 4 – Robust Full (adds economic + political controls)
iv_model_4 <- ivreg(CARS_POLLUTING ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA +
                      HIGH_TECH + LOCAL_TURNOUT |
                      ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA +
                      HIGH_TECH + LOCAL_TURNOUT,
                    data = IV_DATA_small)


# Model 0 – Instrument only
iv_model_0 <- ivreg(LIBRARIES ~ ABS_POLAR_NAT | ABS_POLAR, data = IV_DATA_small)

# Model 1 – Basic controls
iv_model_1 <- ivreg(LIBRARIES ~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG |
                      ABS_POLAR + POP_LOG + INCOME_LOG,
                    data = IV_DATA)

# Model 2 – Add demographics and macroarea
iv_model_2 <- ivreg(LIBRARIES ~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD |
                      ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD,
                    data = IV_DATA)

# Model 3 – Full (adds year_diff)
iv_model_3 <- ivreg(LIBRARIES ~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
                      ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                    data = IV_DATA)

# Model 4 – Robust Full (adds economic + political controls)
iv_model_4 <- ivreg(LIBRARIES ~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA +
                      HIGH_TECH + LOCAL_TURNOUT |
                      ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA +
                      HIGH_TECH + LOCAL_TURNOUT,
                    data = IV_DATA)

# Export all models to a stargazer table
stargazer(iv_model_0, iv_model_1, iv_model_2, iv_model_3, iv_model_4,
          type = "text",
          title = "Nested IV Models with ABS_INDEX_NAT",
          column.labels = c("No Controls", "Basic", "Extended", "Full", "Robust Full"),
          dep.var.labels = "Placebo: Cars Pollution",
          keep.stat = c("n", "rsq", "adj.rsq", "f"),
          digits = 3)

##################### PERMUTATION PLACEBO #############################

run_placebo_test <- function(data, outcome, endog, exog_instr, exog_ctrls, 
                             n_sim = 1000, seed = 123, panel_label = NULL) {
  
  # Friendly variable name mapping
  friendly_names <- c(
    "ABS_INDEX_NAT" = "National Ideology",
    "ABS_POLAR_NAT" = "National Polarization"
  )
  
  # Lookup friendly name
  endog_label <- ifelse(endog %in% names(friendly_names), friendly_names[endog], endog)
  # Optional panel label for combined plot titles (e.g., "(a)", "(b)")
  title_prefix <- if (!is.null(panel_label)) paste0("(", panel_label, ") ") else ""
  
  # Build formulas
  iv_formula <- as.formula(paste0(outcome, " ~ ", endog, " + ", exog_ctrls, " | ",
                                  exog_instr, " + ", exog_ctrls))
  
  # Step 1: True model
  true_model <- ivreg(iv_formula, data = data)
  true_coef <- coef(true_model)[endog]
  
  # Step 2: Simulations
  placebo_coefs <- numeric(n_sim)
  for (i in 1:n_sim) {
    shuffled_data <- data
    shuffled_data[[outcome]] <- sample(shuffled_data[[outcome]])
    
    placebo_model <- ivreg(iv_formula, data = shuffled_data)
    placebo_coefs[i] <- coef(placebo_model)[endog]
  }
  
  # Step 3: Empirical p-value
  empirical_p <- mean(abs(placebo_coefs) >= abs(true_coef))
  
  # Step 4: Plot
  placebo_df <- data.frame(placebo_coefs = placebo_coefs)
  p <- ggplot(placebo_df, aes(x = placebo_coefs)) +
    geom_histogram(bins = 40, fill = "gray85", color = "black") +
    geom_vline(xintercept = true_coef, color = "gray30", size = 1, linetype = "dashed") +
    annotate("text", x = true_coef, y = Inf, label = "True estimate",
             vjust = 1.5, hjust = 0, size = 3.5, family = "serif", color = "gray30", angle = 90) +
    labs(
      title = paste0(title_prefix, endog_label),,
      subtitle = paste0("True coefficient: ", round(true_coef, 3)),
      x = paste0("Placebo Estimates"),
      y = "Frequency"
    ) +
    theme_minimal(base_family = "serif", base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "gray50"),
      axis.ticks = element_line(color = "gray50"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 8))
    )
  
  list(
    plot = p,
    true_coef = true_coef,
    empirical_p = empirical_p
  )
}

# Controls and instruments
exog_ctrls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA"
exog_ctrls_full <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA + MUNICIPAL_SIZE"
exog_instr_index <- "ABS_INDEX"
exog_instr_polar <- "ABS_POLAR"

# Run placebo test for ABS_INDEX_NAT
placebo_index <- run_placebo_test(
  data = IV_DATA_small,
  outcome = "PRAGMATIC_SHARE",
  endog = "ABS_INDEX_NAT",
  exog_instr = exog_instr_index,
  exog_ctrls = exog_ctrls,
  panel_label = "b"
)

# Run placebo test for ABS_POLAR_NAT
placebo_polar <- run_placebo_test(
  data = IV_DATA_small,
  outcome = "PRAGMATIC_SHARE",
  endog = "ABS_POLAR_NAT",
  exog_instr = exog_instr_polar,
  exog_ctrls = exog_ctrls,
  panel_label = "a"
)

# Run placebo test for ABS_INDEX_NAT
placebo_index2 <- run_placebo_test(
  data = IV_DATA,
  outcome = "PRAGMATIC_SHARE",
  endog = "ABS_INDEX_NAT",
  exog_instr = exog_instr_index,
  exog_ctrls = exog_ctrls_full,
  panel_label = "b"
)

# Run placebo test for ABS_POLAR_NAT
placebo_polar2 <- run_placebo_test(
  data = IV_DATA,
  outcome = "PRAGMATIC_SHARE",
  endog = "ABS_POLAR_NAT",
  exog_instr = exog_instr_polar,
  exog_ctrls = exog_ctrls_full,
  panel_label = "a"
)

combined_placebo <- placebo_polar$plot + placebo_index$plot +
  plot_layout(ncol = 2)

print(combined_placebo)

combined_placebo2 <- placebo_polar$plot + placebo_index$plot +
  plot_layout(ncol = 1)

print(combined_placebo2)

ggsave("combined_placebo.png", plot = combined_placebo, 
       width = 12, height = 4.5, dpi = 300, units = "in")
ggsave("combined_placebo2.png", plot = combined_placebo2, 
       width = 7, height = 9, dpi = 300, units = "in")

# Display plots
print(placebo_index$plot)
cat("Empirical p-value for ABS_INDEX_NAT:", round(placebo_index$empirical_p, 4), "\n")

print(placebo_polar$plot)
cat("Empirical p-value for ABS_POLAR_NAT:", round(placebo_polar$empirical_p, 4), "\n")


################### FALSIFICATION TESTS FINAL ###########################

# Define controls (2018-2021 varying)
controls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF"
controls_full <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE"

# Model A.1 – PRAGMATIC_SHARE ~ ABS_INDEX_NAT
iv_model_1 <- ivreg(
  formula = as.formula(paste("LIBRARIES ~ ABS_INDEX_NAT +", controls, "| ABS_INDEX +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_1, vcov = vcovCL(iv_model_1, cluster = IV_DATA_small$COD_PROV)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A.2 – PRAGMATIC_SHARE ~ ABS_POLAR_NAT
iv_model_2 <- ivreg(
  formula = as.formula(paste("LIBRARIES ~ ABS_POLAR_NAT +", controls, "| ABS_POLAR +", controls)),
  data = IV_DATA_small
)
summary(coeftest(iv_model_2, vcov = vcovCL(iv_model_2, cluster = IV_DATA_small$COD_PROV)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF, data = IV_DATA_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

# Model A.3 – ABS_INDEX_ADMIN ~ ABS_INDEX_NAT
iv_model_3 <- ivreg(
  formula = as.formula(paste("LIBRARIES ~ ABS_INDEX_NAT +", controls_full, "| ABS_INDEX +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_3, vcov = vcovCL(iv_model_3, cluster = IV_DATA$COD_PROV)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$COD_PROV)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A.4 – ABS_POLAR_ADMIN ~ ABS_POLAR_NAT
iv_model_4 <- ivreg(
  formula = as.formula(paste("LIBRARIES ~ ABS_POLAR_NAT +", controls_full, "| ABS_POLAR +", controls_full)),
  data = IV_DATA
)
summary(coeftest(iv_model_4, vcov = vcovCL(iv_model_4, cluster = IV_DATA$COD_PROV)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA$COD_PROV)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

models <- list(
  "Libraries ~ Index"       = iv_model_1,
  "Libraries ~ Polar"       = iv_model_2,
  "Libraries ~ Index"     = iv_model_3,
  "Libraries ~ Polar"     = iv_model_4
)

modelsummary(models,
             vcov = ~ COD_PROV,
             title = "Falsification Check",
             output = "markdown",
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik")





################# REDUCED FORM TESTS #######################

# Reduced form template function
run_reduced_form <- function(data, outcome, instrument, controls_formula) {
  formula_rf <- as.formula(paste0(outcome, " ~ ", instrument, " + ", controls_formula))
  model_rf <- lm(formula_rf, data = data)
  return(model_rf)
}

controls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA"
controls_full <- paste(controls, "+ MUNICIPAL_SIZE")

# Model 1: PRAGMATIC_SHARE ~ ABS_INDEX (small sample)
rf1 <- run_reduced_form(
  data = IV_DATA_small,
  outcome = "PRAGMATIC_SHARE",
  instrument = "ABS_INDEX",
  controls_formula = controls
)

# Model 2: PRAGMATIC_SHARE ~ ABS_POLAR (small sample)
rf2 <- run_reduced_form(
  data = IV_DATA_small,
  outcome = "PRAGMATIC_SHARE",
  instrument = "ABS_POLAR",
  controls_formula = controls
)

# Model 3: ABS_INDEX_ADMIN ~ ABS_INDEX (full sample)
rf3 <- run_reduced_form(
  data = IV_DATA,
  outcome = "ABS_INDEX_ADMIN",
  instrument = "ABS_INDEX",
  controls_formula = controls_full
)

# Model 4: ABS_POLAR_ADMIN ~ ABS_INDEX (full sample)
rf4 <- run_reduced_form(
  data = IV_DATA,
  outcome = "ABS_POLAR_ADMIN",
  instrument = "ABS_INDEX",
  controls_formula = controls_full
)

cat("Reduced form results:\n")
cat("Model 1 (PRAGMATIC_SHARE ~ ABS_INDEX): coef =", coef(rf1)["ABS_INDEX"], 
    ", p-value =", summary(rf1)$coefficients["ABS_INDEX", 4], "\n")

cat("Model 2 (PRAGMATIC_SHARE ~ ABS_POLAR): coef =", coef(rf2)["ABS_POLAR"], 
    ", p-value =", summary(rf2)$coefficients["ABS_POLAR", 4], "\n")

cat("Model 3 (ABS_INDEX_ADMIN ~ ABS_INDEX): coef =", coef(rf3)["ABS_INDEX"], 
    ", p-value =", summary(rf3)$coefficients["ABS_INDEX", 4], "\n")

cat("Model 4 (ABS_POLAR_ADMIN ~ ABS_INDEX): coef =", coef(rf4)["ABS_INDEX"], 
    ", p-value =", summary(rf4)$coefficients["ABS_INDEX", 4], "\n")

# Define controls (2018-2021 varying)
controls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF"
controls_full <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE"

# Model A.1 – PRAGMATIC_SHARE ~ ABS_INDEX
model_1 <- lm(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_INDEX +", controls)),
  data = IV_DATA_small
)
summary(coeftest(model_1, vcov = vcovCL(model_1, cluster = IV_DATA_small$COD_PROV)))

# Model A.2 – PRAGMATIC_SHARE ~ ABS_POLAR
model_2 <- lm(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_POLAR +", controls)),
  data = IV_DATA_small
)
summary(coeftest(model_2, vcov = vcovCL(model_2, cluster = IV_DATA_small$COD_PROV)))

# Model A.3 – PRAGMATIC_SHARE ~ ABS_INDEX
model_3 <- lm(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_INDEX +", controls_full)),
  data = IV_DATA
)
summary(coeftest(model_3, vcov = vcovCL(model_3, cluster = IV_DATA$COD_PROV)))
# Model A.4 – PRAGMATIC_SHARE ~ ABS_POLAR
model_4 <- lm(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_POLAR +", controls_full)),
  data = IV_DATA
)
summary(coeftest(model_4, vcov = vcovCL(model_4, cluster = IV_DATA$COD_PROV)))

models <- list(
  "Pragmatic ~ Index"       = model_1,
  "Pragmatic ~ Polar"       = model_2,
  "Pragmatic ~ Index"     = model_3,
  "Pragmatic ~ Polar"     = model_4
)

modelsummary(models,
             vcov = ~ COD_PROV,
             title = "Reduced form results",
             output = "markdown",
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik")


########################## JACKKNIFE ROBUSTNESS #####################################

# Purpose: Test whether the IV estimates are overly influenced
# by any single municipality (outlier sensitivity).
# Method: Iteratively re-estimate the IV model, removing one
# observation at a time, and record the coefficient on the
# endogenous regressor (ABS_INDEX_NAT or ABS_POLAR_NAT).
# 
# Result Summary:
# - Model 1 (ABS_INDEX_NAT → PRAGMATIC_SHARE): range ~ [0.294, 0.318]
# - Model 2 (ABS_POLAR_NAT → PRAGMATIC_SHARE): range ~ [–0.388, –0.333]
# - Model 3 (ABS_INDEX_NAT → ABS_INDEX_ADMIN): range ~ [0.801, 0.836]
# - Model 4 (ABS_INDEX_NAT → ABS_POLAR_ADMIN): range ~ [0.182, 0.206]
#
# In all four models, the jackknife estimates are tightly clustered
# around the full-sample coefficient. This confirms that the results
# are not driven by influential observations and are highly robust.

### Jackknife function ###

run_jackknife_iv <- function(data, outcome, endog, instrument, controls_formula) {
  n <- nrow(data)
  jackknife_coefs <- numeric(n)
  
  friendly_names <- c(
    "ABS_INDEX_NAT" = "(b) National Ideology",
    "ABS_POLAR_NAT" = "(a) National Polarization"
  )
  
  endog_label <- ifelse(endog %in% names(friendly_names), friendly_names[endog], endog)
  
  # Build full IV formula
  iv_formula <- as.formula(paste0(
    outcome, " ~ ", endog, " + ", controls_formula, " | ",
    instrument, " + ", controls_formula
  ))
  
  for (i in 1:n) {
    data_jk <- data[-i, ]
    
    model_jk <- tryCatch({
      ivreg(iv_formula, data = data_jk)
    }, error = function(e) return(NA))
    
    jackknife_coefs[i] <- if (!is.na(model_jk)[1]) coef(model_jk)[endog] else NA
  }
  
  # Remove failed runs
  jackknife_coefs <- jackknife_coefs[!is.na(jackknife_coefs)]
  
  # True full-sample coefficient
  true_model <- ivreg(iv_formula, data = data)
  true_coef <- coef(true_model)[endog]
  
  # Prepare data
  jack_df <- data.frame(coef = jackknife_coefs)
  
  # Plot
  jack_plot <- ggplot(jack_df, aes(x = coef)) +
    geom_histogram(bins = 40, fill = "gray85", color = "black") +
    geom_vline(xintercept = true_coef, color = "gray30", size = 1, linetype = "dashed") +
    annotate("text", x = true_coef, y = Inf, label = "True estimate",
             vjust = 1.5, hjust = 0, size = 3.5, family = "serif", color = "gray30", angle = 90) +
    labs(
      title = paste(endog_label),
      subtitle = paste0("True coefficient: ", round(true_coef, 3)),
      x = "Leave-One-Out IV Estimates",
      y = "Frequency"
    ) +
    theme_minimal(base_family = "serif", base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "gray50"),
      axis.ticks = element_line(color = "gray50"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 8))
    )
  
  return(list(coefs = jackknife_coefs, true_coef = true_coef, plot = jack_plot))
}


### Run models ###

# Controls
controls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA"
controls_full <- paste(controls, "+ MUNICIPAL_SIZE")

# Model 1
jk1 <- run_jackknife_iv(
  data = IV_DATA_small,
  outcome = "PRAGMATIC_SHARE",
  endog = "ABS_INDEX_NAT",
  instrument = "ABS_INDEX",
  controls_formula = controls
)

# Model 2
jk2 <- run_jackknife_iv(
  data = IV_DATA_small,
  outcome = "PRAGMATIC_SHARE",
  endog = "ABS_POLAR_NAT",
  instrument = "ABS_POLAR",
  controls_formula = controls
)

# Model 3
jk3 <- run_jackknife_iv(
  data = IV_DATA,
  outcome = "PRAGMATIC_SHARE",
  endog = "ABS_INDEX_NAT",
  instrument = "ABS_INDEX",
  controls_formula = controls_full
)

# Model 4
jk4 <- run_jackknife_iv(
  data = IV_DATA,
  outcome = "PRAGMATIC_SHARE",
  endog = "ABS_POLAR_NAT",
  instrument = "ABS_POLAR",
  controls_formula = controls_full
)
### Results ###

# Show plots
print(jk1$plot)
print(jk2$plot)
print(jk3$plot)
print(jk4$plot)

# Show ranges
cat("Model 1 range:", range(jk1$coefs), "\n")
cat("Model 2 range:", range(jk2$coefs), "\n")
cat("Model 3 range:", range(jk3$coefs), "\n")
cat("Model 4 range:", range(jk4$coefs), "\n")

combined_jack <- jk2$plot + jk1$plot +
  plot_layout(ncol = 2)

print(combined_jack)

combined_jack2 <- jk4$plot + jk3$plot +
  plot_layout(ncol = 2)

print(combined_jack2)

ggsave("combined_jack.png", plot = combined_jack, 
       width = 12, height = 4.5, dpi = 300, units = "in")
ggsave("combined_jack2.png", plot = combined_jack2, 
       width = 12, height = 4.5, dpi = 300, units = "in")


############################# FIRST-STAGE HETEROGENEITY ###########################

# Purpose: Test whether the instrument's predictive power (first stage)
# varies across macro-regions (North, Center, South).
#
# Result Summary:
# - Model 1 (ABS_INDEX → ABS_INDEX_NAT): strong first-stage overall; significantly weaker in South.
# - Model 2 (ABS_POLAR → ABS_POLAR_NAT): same pattern; predictive power collapses in the South.
# - Model 3 (ABS_INDEX_NAT → ABS_INDEX_ADMIN): strong in North; weaker in South.
# - Model 4 (ABS_INDEX_NAT → ABS_POLAR_ADMIN): predictive strength concentrated in the North;
#   both Center and South interactions negative and significant.
#
# Interpretation:
# The instrument remains relevant, but its power is regionally heterogeneous.
# The Local Average Treatment Effect (LATE) is therefore mostly identified
# through municipalities in the North — where historical ideology better predicts
# national and local ideological behavior today.

### Interaction function ###

run_first_stage_interaction <- function(data, outcome, instrument, controls_formula, subgroup_var) {
  interaction_formula <- as.formula(paste0(
    outcome, " ~ ", instrument, " * ", subgroup_var, " + ", controls_formula
  ))
  
  model_fs <- lm(interaction_formula, data = data)
  return(model_fs)
}

### Run ###

controls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF"
controls_full <- paste(controls, "+ MUNICIPAL_SIZE")

# Model 1: ABS_INDEX_NAT ~ ABS_INDEX * MACROAREA
fs1_macro <- run_first_stage_interaction(
  data = IV_DATA_small,
  outcome = "ABS_INDEX_NAT",
  instrument = "ABS_INDEX",
  controls_formula = controls,
  subgroup_var = "MACROAREA"
)

# Model 2: ABS_POLAR_NAT ~ ABS_POLAR * MACROAREA
fs2_macro <- run_first_stage_interaction(
  data = IV_DATA_small,
  outcome = "ABS_POLAR_NAT",
  instrument = "ABS_POLAR",
  controls_formula = controls,
  subgroup_var = "MACROAREA"
)

# Model 3: ABS_INDEX_ADMIN ~ ABS_INDEX_NAT * MACROAREA
fs3_macro <- run_first_stage_interaction(
  data = IV_DATA,
  outcome = "ABS_INDEX_ADMIN",
  instrument = "ABS_INDEX_NAT",
  controls_formula = controls_full,
  subgroup_var = "MACROAREA"
)

# Model 4: ABS_POLAR_ADMIN ~ ABS_INDEX_NAT * MACROAREA
fs4_macro <- run_first_stage_interaction(
  data = IV_DATA,
  outcome = "ABS_POLAR_ADMIN",
  instrument = "ABS_INDEX_NAT",
  controls_formula = controls_full,
  subgroup_var = "MACROAREA"
)

### View ###

summary(fs1_macro)
summary(fs2_macro)
summary(fs3_macro)
summary(fs4_macro)



# Define controls
controls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF"
controls_full <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE"

# Model A.1 – ABS_INDEX_NAT ~ ABS_INDEX * MACROAREA
model_1 <- lm(
  formula = as.formula(paste("ABS_INDEX_NAT ~ ABS_INDEX * MACROAREA +", controls)),
  data = IV_DATA_small
)
vcov_1 <- vcovCL(model_1, cluster = IV_DATA_small$COD_PROV)
summary(coeftest(model_1, vcov = vcov_1))

# Joint F-test for interaction terms
linearHypothesis(model_1,
                 c("ABS_INDEX:MACROAREACENTER = 0", "ABS_INDEX:MACROAREASOUTH = 0"),
                 vcov = vcov_1
)

# Model A.2 – ABS_POLAR_NAT ~ ABS_POLAR * MACROAREA
model_2 <- lm(
  formula = as.formula(paste("ABS_POLAR_NAT ~ ABS_POLAR * MACROAREA +", controls)),
  data = IV_DATA_small
)
vcov_2 <- vcovCL(model_2, cluster = IV_DATA_small$COD_PROV)
summary(coeftest(model_2, vcov = vcov_2))

linearHypothesis(model_2,
                 c("ABS_POLAR:MACROAREACENTER = 0", "ABS_POLAR:MACROAREASOUTH = 0"),
                 vcov = vcov_2
)

# Model A.3 – ABS_INDEX_NAT ~ ABS_INDEX * MACROAREA
model_3 <- lm(
  formula = as.formula(paste("ABS_INDEX_NAT ~ ABS_INDEX * MACROAREA +", controls_full)),
  data = IV_DATA
)
vcov_3 <- vcovCL(model_3, cluster = IV_DATA$COD_PROV)
summary(coeftest(model_3, vcov = vcov_3))

linearHypothesis(model_3,
                 c("ABS_INDEX:MACROAREACENTER = 0", "ABS_INDEX:MACROAREASOUTH = 0"),
                 vcov = vcov_3
)

# Model A.4 – ABS_POLAR_NAT ~ ABS_POLAR * MACROAREA
model_4 <- lm(
  formula = as.formula(paste("ABS_POLAR_NAT ~ ABS_POLAR * MACROAREA +", controls_full)),
  data = IV_DATA
)
vcov_4 <- vcovCL(model_4, cluster = IV_DATA$COD_PROV)
summary(coeftest(model_4, vcov = vcov_4))

linearHypothesis(model_4,
                 c("ABS_POLAR:MACROAREACENTER = 0", "ABS_POLAR:MACROAREASOUTH = 0"),
                 vcov = vcov_4
)

models <- list(
  "Index Nat ~ Index"       = model_1,
  "Polar Nat ~ Polar"       = model_3,
  "Index Nat ~ Index"     = model_2,
  "Polar Nat ~ Polar"     = model_4
)

modelsummary(models,
             vcov = ~ COD_PROV,
             title = "First-stage heterogeneity",
             output = "latex",
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik")


############################## ROBUSTNESS CHECK: FILTER FOR BOTH LISTS #############################

IV_DATA_B <- IV_DATA %>%
  filter(PROG_LISTA == 1 & CONS_LISTA == 1)
IV_DATA_B_small <- IV_DATA_small %>%
  filter(PROG_LISTA == 1 & CONS_LISTA == 1)

summary(IV_DATA_small$ABS_INDEX_NAT)

quantile(IV_DATA_B$ABS_POLAR_NAT)
sd(IV_DATA_B$ABS_POLAR_NAT)

quantile(IV_DATA_B$PRAGMATIC_SHARE)
sd(IV_DATA_B$PRAGMATIC_SHARE)



# Define controls
controls <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF"
controls_full <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE"

# Model B1 – PRAGMATIC_SHARE ~ ABS_INDEX_NAT
iv_model_1_B <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_INDEX_NAT +", controls_full, "| ABS_INDEX +", controls_full)),
  data = IV_DATA_B
)
summary(coeftest(iv_model_1_B, vcov = vcovCL(iv_model_1_B, cluster = IV_DATA_B$COD_PROV)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA_B)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_B$COD_PROV)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)


# Model B2 – PRAGMATIC_SHARE ~ ABS_POLAR_NAT
iv_model_2_B <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_POLAR_NAT +", controls_full, "| ABS_POLAR +", controls_full)),
  data = IV_DATA_B
)
summary(coeftest(iv_model_2_B, vcov = vcovCL(iv_model_2_B, cluster = IV_DATA_B$COD_PROV)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA + YEAR_DIFF + MUNICIPAL_SIZE, data = IV_DATA_B)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_B$COD_PROV)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

# Model B3 – ABS_INDEX_ADMIN ~ ABS_INDEX_NAT
iv_model_3_B <- ivreg(
  formula = as.formula(paste("ABS_INDEX_ADMIN ~ ABS_INDEX_NAT +", controls_full, "| ABS_INDEX +", controls_full)),
  data = IV_DATA_B
)
summary(coeftest(iv_model_3_B, vcov = vcovCL(iv_model_3_B, cluster = IV_DATA_B$COD_PROV)))
summary(iv_model_1_B, diagnostic = T)

# Model B4 – ABS_POLAR_ADMIN ~ ABS_POLAR_NAT
iv_model_4_B <- ivreg(
  formula = as.formula(paste("ABS_POLAR_ADMIN ~ ABS_POLAR_NAT +", controls_full, "| ABS_POLAR +", controls_full)),
  data = IV_DATA_B
)
summary(coeftest(iv_model_4_B, vcov = vcovCL(iv_model_4_B, cluster = IV_DATA_B$COD_PROV)))

models <- list(
  "Pragmatic ~ Index"       = iv_model_1_B,
  "Pragmatic ~ Polar"       = iv_model_2_B,
  "Index_Admin ~ Index"     = iv_model_3_B,
  "Polar_Admin ~ Polar"     = iv_model_4_B
)

modelsummary(models,
             vcov = ~ COD_PROV,
             title = "IV Estimates filtering for both lists",
             output = "markdown",
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik")



##################### QUADRATIC MODEL ##########################################

IV_DATA_B <- IV_DATA %>%
  filter(PROG_LISTA == 1 & CONS_LISTA == 1)
IV_DATA_B_small <- IV_DATA_small %>%
  filter(PROG_LISTA == 1 & CONS_LISTA == 1)

IV_DATA_B$ABS_INDEX_NAT_sq <- IV_DATA_B$ABS_INDEX_NAT^2
IV_DATA_B$ABS_INDEX_sq <- IV_DATA_B$ABS_INDEX^2

iv_model_quad <- ivreg(
  PRAGMATIC_SHARE ~ ABS_INDEX_NAT + ABS_INDEX_NAT_sq + 
    POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + 
    INDEX_OLD + YEAR_DIFF + MACROAREA |
    ABS_INDEX + ABS_INDEX_sq + 
    POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + 
    INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_B
)

summary(iv_model_quad, diagnostic = T)


# Get the model frame (only rows used in estimation)
model_data <- model.frame(iv_model_quad)

# Add fitted values to that cleaned dataset
model_data$fitted <- predict(iv_model_quad)

# Now plot the relationship
ggplot(model_data, aes(x = ABS_INDEX_NAT, y = fitted)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(
    title = "Fitted Relationship: ABS_INDEX_NAT vs PRAGMATIC_SHARE",
    x = "ABS_INDEX_NAT",
    y = "Fitted PRAGMATIC_SHARE"
  )

# Step 1: Generate sequence for ABS_INDEX_NAT
abs_seq <- seq(
  quantile(IV_DATA_B$ABS_INDEX_NAT, 0.02, na.rm = TRUE),
  quantile(IV_DATA_B$ABS_INDEX_NAT, 0.99, na.rm = TRUE),
  length.out = 100
)

# Step 2: Compute mean of control variables
controls_means <- IV_DATA_B %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), ~ mean(.x, na.rm = TRUE)))

# Step 3: Build prediction data frame (with average controls and one macro-area)
pred_df_quad <- data.frame(
  ABS_INDEX_NAT = abs_seq,
  ABS_INDEX_NAT_sq = abs_seq^2,
  POP_LOG = controls_means$POP_LOG,
  INCOME_LOG = controls_means$INCOME_LOG,
  FOREIGN_SHARE = controls_means$FOREIGN_SHARE,
  TERTIARY_EDU = controls_means$TERTIARY_EDU,
  INDEX_OLD = controls_means$INDEX_OLD,
  YEAR_DIFF = controls_means$YEAR_DIFF,
  MACROAREA = factor("NORTH", levels = levels(IV_DATA_B$MACROAREA))
)

# Step 4: Add instrument terms (needed for predict.ivreg to work)
pred_df_quad$ABS_INDEX <- pred_df_quad$ABS_INDEX_NAT
pred_df_quad$ABS_INDEX_sq <- pred_df_quad$ABS_INDEX_NAT_sq

# Step 5: Compute model matrix and robust standard errors
X_mat <- model.matrix(~ ABS_INDEX_NAT + ABS_INDEX_NAT_sq + POP_LOG + INCOME_LOG +
                        FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df_quad)

vcov_robust <- vcovCL(iv_model_quad, cluster = IV_DATA_B$COD_PROV)

# Step 6: Get predictions and confidence intervals
pred_df_quad$predicted <- predict(iv_model_quad, newdata = pred_df_quad)
se_pred <- sqrt(diag(X_mat %*% vcov_robust %*% t(X_mat)))
pred_df_quad$conf.low <- pred_df_quad$predicted - 1.96 * se_pred
pred_df_quad$conf.high <- pred_df_quad$predicted + 1.96 * se_pred

# Step 6.5: Compute peak of the quadratic curve
b1 <- coef(iv_model_quad)["ABS_INDEX_NAT"]
b2 <- coef(iv_model_quad)["ABS_INDEX_NAT_sq"]
peak_x <- -b1 / (2 * b2)
peak_x

# Step 7: Plot with peak indicator
pred_iv_quad <- ggplot(pred_df_quad, aes(x = ABS_INDEX_NAT, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray75", alpha = 0.4) +
  geom_line(color = "black", size = 1.2) +
  
  # Peak line
  geom_vline(xintercept = peak_x, linetype = "dashed", color = "gray10") +
  annotate("text", x = peak_x -0.01, y = 0.1,
           label = "Predicted Maximum", angle = 90, vjust = -0.5, size = 3.2,
           family = "serif", color = "gray30") +
  
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = pretty(IV_DATA_B$ABS_INDEX_NAT, n = 6)) +
  labs(
    title = "Predicted Pragmatic Voting Across National Ideology",
    subtitle = "Quadratic IV estimates with average controls (Macro-area = North)",
    x = "National Ideology (Progressive to Conservative)",
    y = "Pragmatic Vote Share"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.ticks = element_line(color = "gray40"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 8))
  )

print(pred_iv_quad)

ggsave("pred_iv_quad.png", plot = pred_iv_quad, width = 5.5, height = 5.0, dpi = 300)
ggsave("pred_iv_quad.png", plot = pred_iv_quad, width = 8, height = 5.0, dpi = 300)


########################## OPTION A1: IV REGRESSION WITH BASELINE (2018) CONTROLS ###############################

# This specification estimates the causal effect of national-level ideological alignment 
# (ABS_INDEX_NAT from the 2018 general election) on pragmatic voting behavior in local elections.

# The endogenous variable is ABS_INDEX_NAT (or ABS_POLAR_NAT), instrumented using historical ideology 
# (ABS_INDEX or ABS_POLAR, based on early postwar national elections and referenda).

# All control variables are fixed at their 2018 values for all municipalities, 
# capturing pre-treatment socioeconomic and demographic characteristics.

# YEAR_DIFF is excluded in this setup, as controls are no longer time-varying.

# The model is estimated using ivreg(), and standard errors are clustered at the provincial level 
# (COD_PROV) to account for spatial correlation in the error structure.

IV_DATA_A1_small <- IV_DATA_A1 %>%
  filter(!MUNICIPAL_SIZE %in% c("SMALL"))

# Define controls (2018 fixed)
controls_A1 <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA"
controls_A1_full <- "POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + MACROAREA + MUNICIPAL_SIZE"


# Model A1.1 – PRAGMATIC_SHARE ~ ABS_INDEX_NAT
iv_model_A1_1 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_INDEX_NAT +", controls_A1, "| ABS_INDEX +", controls_A1)),
  data = IV_DATA_A1_small
)
summary(coeftest(iv_model_A1_1, vcov = vcovCL(iv_model_A1_1, cluster = IV_DATA_A1_small$COD_PROV)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA, data = IV_DATA_A1_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_A1_small$COD_PROV)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A1.2 – PRAGMATIC_SHARE ~ ABS_POLAR_NAT
iv_model_A1_2 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_POLAR_NAT +", controls_A1, "| ABS_POLAR +", controls_A1)),
  data = IV_DATA_A1_small
)
summary(coeftest(iv_model_A1_2, vcov = vcovCL(iv_model_A1_2, cluster = IV_DATA_A1_small$COD_PROV)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA, data = IV_DATA_A1_small)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_A1_small$COD_PROV)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

# Model A1.3 – PRAGMATIC_SHARE ~ ABS_INDEX_NAT
iv_model_A1_3 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_INDEX_NAT +", controls_A1_full, "| ABS_INDEX +", controls_A1_full)),
  data = IV_DATA_A1
)
summary(coeftest(iv_model_A1_3, vcov = vcovCL(iv_model_A1_3, cluster = IV_DATA_A1$COD_PROV)))
fs_model <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA, data = IV_DATA_A1)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_A1$COD_PROV)
waldtest(fs_model, . ~ . - ABS_INDEX, vcov = fs_vcov)

# Model A1.4 – PRAGMATIC_SHARE ~ ABS_POLAR_NAT
iv_model_A1_4 <- ivreg(
  formula = as.formula(paste("PRAGMATIC_SHARE ~ ABS_POLAR_NAT +", controls_A1_full, "| ABS_POLAR +", controls_A1_full)),
  data = IV_DATA_A1
)
summary(coeftest(iv_model_A1_4, vcov = vcovCL(iv_model_A1_4, cluster = IV_DATA_A1$COD_PROV)))
fs_model <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                 INDEX_OLD + MACROAREA, data = IV_DATA_A1)
fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_A1$COD_PROV)
waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)

models <- list(
  "Pragmatic ~ Index"       = iv_model_A1_1,
  "Pragmatic ~ Polar"       = iv_model_A1_2,
  "Pragmatic ~ Index"     = iv_model_A1_3,
  "Pragmatic ~ Polar"     = iv_model_A1_4
)

modelsummary(models,
             vcov = ~ COD_PROV,
             title = "IV Estimates Using 2018-Only Controls (Option A1)",
             output = "latex",  # or "latex", "html", "docx"
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik")

################################################ CONLEY BOUNDS #######################################

reduced <- lm(PRAGMATIC_SHARE ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA, data = IV_DATA_small)

first_stage <- lm(ABS_INDEX_NAT ~ ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
              TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA, data = IV_DATA_small)

iv_model <- ivreg(PRAGMATIC_SHARE ~ ABS_INDEX_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
                    ABS_INDEX + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                    data = IV_DATA_small)

pi1 <- coef(reduced)["ABS_INDEX"]
pi2 <- coef(first_stage)["ABS_INDEX"]
beta_iv <- coef(iv_model)["ABS_INDEX_NAT"]

# Implied gamma if exclusion restriction is violated
gamma_hat <- pi1 - beta_iv * pi2
gamma_hat

gamma_range <- c(-0.1, 0.1)

z_coef <- coef(first_stage)["ABS_INDEX"]  
beta_iv <- coef(iv_model)["ABS_INDEX_NAT"]

gamma_seq <- seq(-0.2, 0.2, by = 0.01)

adjusted_betas <- beta_iv + gamma_seq / z_coef

plot(gamma_seq, adjusted_betas, type = "l", lwd = 2,
     xlab = "Gamma (Direct Effect of Historical Ideology on Civic Vote)",
     ylab = "Adjusted IV Estimate",
     main = "Conley Bounds Sensitivity Analysis")
abline(h = beta_iv, col = "red", lty = 2)

z_coef


### PLOT ###

# Find where the adjusted estimate crosses zero
# Using linear interpolation for better precision

sign_change <- which(diff(sign(adjusted_betas)) != 0)

# Narrow in on gamma values around the threshold
gamma1 <- gamma_seq[sign_change]
gamma2 <- gamma_seq[sign_change + 1]
beta1 <- adjusted_betas[sign_change]
beta2 <- adjusted_betas[sign_change + 1]

# Linear interpolation for gamma such that beta = 0
gamma_threshold <- gamma1 - beta1 * (gamma2 - gamma1) / (beta2 - beta1)
gamma_threshold

# Create data for the Conley bounds
gamma_seq <- seq(-0.2, 0.2, by = 0.01)
z_coef <- coef(first_stage)["ABS_INDEX"]
beta_iv <- coef(iv_model)["ABS_INDEX_NAT"]
adjusted_betas <- beta_iv + gamma_seq / z_coef
conley_df <- data.frame(gamma = gamma_seq, beta = adjusted_betas)

# Prepare shaded data where estimate is positive
conley_df$robust <- ifelse(conley_df$beta > 0, TRUE, FALSE)

# Enhanced plot
conley_plot <- ggplot(conley_df, aes(x = gamma, y = beta)) +
  # Robust region shading
  geom_ribbon(data = subset(conley_df, robust == TRUE),
              aes(ymin = 0, ymax = beta),
              fill = "gray80", alpha = 0.5) +
  
  # Conley estimate line
  geom_line(color = "gray10", linewidth = 1.2) +
  
  # Baseline IV estimate at gamma = 0
  annotate("point", x = 0, y = beta_iv, color = "gray10", size = 2) +
  geom_hline(yintercept = beta_iv, linetype = "dotted", color = "gray20") +
  annotate("text", x = 0.18, y = beta_iv + 0.05,
           label = "IV estimate", size = 4, family = "serif", color = "gray40") +
  
  # Zero effect line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  annotate("text", x = 0.18, y = 0.05,
           label = "Zero effect", size = 4, family = "serif", color = "gray50") +
  
  # Vertical threshold line
  geom_vline(xintercept = gamma_threshold, linetype = "dotted", color = "gray20") +
  annotate("text", x = gamma_threshold -0.03, y = min(adjusted_betas) + 0.31,
           label = sprintf("γ = %.2f", gamma_threshold),
           angle = 90, vjust = 2, size = 4, family = "serif", color = "gray40") +
  # Labels
  labs(
    title = "(b) Absolute Ideology Index",
    # title = "Sensitivity of IV Estimate to Exclusion Restriction Violations",
    # subtitle = "Conley bounds: Adjusted IV estimates across γ (direct effect of instrument)",
    x = "γ (Direct Effect of Historical Ideology on Pragmatic Vote Share)",
    y = "Adjusted IV Estimate"
  ) +
  
  # Scales
  scale_y_continuous(breaks = pretty(adjusted_betas, n = 5)) +
  scale_x_continuous(breaks = seq(-0.2, 0.2, by = 0.05)) +
  
  # Matched theme
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.ticks = element_line(color = "gray40"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 8))
  )

print(conley_plot)


### POLARIZATION ###

reduced <- lm(PRAGMATIC_SHARE ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA, data = IV_DATA_small)

first_stage <- lm(ABS_POLAR_NAT ~ ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA, data = IV_DATA_small)

iv_model <- ivreg(PRAGMATIC_SHARE ~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
                    ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                  data = IV_DATA_small)

pi1 <- coef(reduced)["ABS_POLAR"]
pi2 <- coef(first_stage)["ABS_POLAR"]
beta_iv <- coef(iv_model)["ABS_POLAR_NAT"]

# Implied gamma if exclusion restriction is violated
gamma_hat <- pi1 - beta_iv * pi2
gamma_hat

gamma_range <- c(-0.1, 0.1)
z_coef <- coef(first_stage)["ABS_POLAR"]  
beta_iv <- coef(iv_model)["ABS_POLAR_NAT"]

gamma_seq <- seq(-0.2, 0.2, by = 0.01)

adjusted_betas <- beta_iv + gamma_seq / z_coef

plot(gamma_seq, adjusted_betas, type = "l", lwd = 2,
     xlab = "Gamma (Direct Effect of Historical Polarization on Civic Vote)",
     ylab = "Adjusted IV Estimate",
     main = "Conley Bounds Sensitivity Analysis")
abline(h = beta_iv, col = "red", lty = 2)

z_coef

### PLOTS ###

# 1. New first-stage and IV coefficients
z_coef <- coef(first_stage)["ABS_POLAR"]  # instrument to endogenous
beta_iv <- coef(iv_model)["ABS_POLAR_NAT"]  # new IV estimate (~ -0.4)

# 2. Define gamma sequence
gamma_seq <- seq(-0.2, 0.2, by = 0.01)

# 3. Calculate adjusted IV estimates
adjusted_betas <- beta_iv + gamma_seq / z_coef
conley_df <- data.frame(gamma = gamma_seq, beta = adjusted_betas)

# 4. Define 'robust' zone (where estimate remains negative)
conley_df$robust <- conley_df$beta < 0

# 5. Calculate gamma threshold where estimate crosses zero
sign_change <- which(diff(sign(adjusted_betas)) != 0)
gamma1 <- gamma_seq[sign_change]
gamma2 <- gamma_seq[sign_change + 1]
beta1 <- adjusted_betas[sign_change]
beta2 <- adjusted_betas[sign_change + 1]
gamma_threshold <- gamma1 - beta1 * (gamma2 - gamma1) / (beta2 - beta1)

# 6. Plot
conley_plot_pol <- ggplot(conley_df, aes(x = gamma, y = beta)) +
  # Shading: where effect remains negative (supports causal interpretation)
  geom_ribbon(data = subset(conley_df, robust == TRUE),
              aes(ymin = beta, ymax = 0),
              fill = "gray80", alpha = 0.5) +
  
  # Line of adjusted IV estimates
  geom_line(color = "gray10", linewidth = 1.2) +
  
  # Baseline IV estimate at gamma = 0
  annotate("point", x = 0, y = beta_iv, color = "gray10", size = 2) +
  geom_hline(yintercept = beta_iv, linetype = "dotted", color = "gray20") +
  annotate("text", x = 0.18, y = beta_iv + 0.05,
           label = "IV estimate", size = 4, family = "serif", color = "gray40") +
  
  # Zero effect reference line — nudged left
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  annotate("text", x = 0.09, y = 0.05,  # moved from x = 0.18 to x = 0.13
           label = "Zero effect", size = 4, family = "serif", color = "gray50") +
  
  # Threshold gamma where β crosses 0
  geom_vline(xintercept = gamma_threshold, linetype = "dotted", color = "gray20") +
  annotate("text", x = gamma_threshold, y = min(adjusted_betas) + 0.15,
           label = sprintf("γ = %.2f", gamma_threshold),
           angle = 90, vjust = -0.7, size = 4, family = "serif", color = "gray40") +
  # Labels and styling
  labs(
    title = "(a) Polarization Index",
    # title = "Sensitivity of IV Estimate to Exclusion Restriction Violations",
    # subtitle = "Conley bounds: Adjusted IV estimates across γ (direct effect of instrument)",
    x = "γ (Direct Effect of Historical Polarization on Pragmatic Vote Share)",
    y = "Adjusted IV Estimate"
  ) +
  scale_y_continuous(breaks = pretty(adjusted_betas, n = 5)) +
  scale_x_continuous(breaks = seq(-0.2, 0.2, by = 0.05)) +
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.ticks = element_line(color = "gray40"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 8))
  )

print(conley_plot_pol)

# ggsave("conley_plot_pol.png", plot = conley_plot_pol, width = 6.5, height = 4.5, dpi = 300)


combined_plot <- conley_plot_pol + conley_plot +
  plot_layout(ncol = 2)

print(combined_plot)

# ggsave("conley_combined.png", plot = combined_plot,
#        width = 12, height = 4.5, dpi = 300, units = "in")

combined_plot2 <- conley_plot_pol + conley_plot +
  plot_layout(ncol = 1)
print(combined_plot2)

ggsave("conley_combined2.png", plot = combined_plot2, width = 6.5, height = 7.2, dpi = 300)


################# SMALL MUNICIPALITIES SENSITIVITY ###########################

### Density plot ###

IV_DATA_B %>%
  count(MUNICIPAL_SIZE)
IV_DATA %>%
  count(MUNICIPAL_SIZE)

# Create a size category variable (adjust threshold as needed)
IV_DATA$size_cat <- ifelse(IV_DATA$POPULATION < 10000, "Small", "Large")

# Density plot of pragmatic vote share
ggplot(IV_DATA_B, aes(x = PRAGMATIC_SHARE, fill = MUNICIPAL_SIZE)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribution of Pragmatic Vote Share by Municipality Size",
    x = "Pragmatic Vote Share",
    y = "Density",
    fill = "Municipality Size"
  ) +
  theme_minimal()

### Fitted values ###

ggplot(IV_DATA, aes(x = ABS_INDEX_NAT, y = PRAGMATIC_SHARE, color = MUNICIPAL_SIZE)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "National Ideology and Pragmatic Voting by Municipality Size",
    x = "National Ideology (Absolute Index)",
    y = "Pragmatic Vote Share",
    color = "Municipality Size"
  ) +
  theme_minimal()

################### BAYESIAN SENSITIVITY - POLARIZATION ######################

# library(brms)

### SINGLE PRIOR ###

# Set a weak prior on the direct effect of the instrument (ABS_INDEX)
priors <- c(
  prior(normal(0, 0.05), class = "b", coef = "ABS_INDEX"),  # Prior on gamma
  prior(normal(0, 1), class = "b")  # Generic prior for all other coefficients
)

# Fit Bayesian model allowing for direct effect of ABS_INDEX (gamma)
bayes_model <- brm(
  formula = PRAGMATIC_SHARE ~ ABS_INDEX_NAT + ABS_INDEX + POP_LOG + INCOME_LOG +
    FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_small,
  prior = priors,
  chains = 4,
  iter = 2000,
  seed = 123
)

# Summarize the result
summary(bayes_model)

# Plot posterior of beta (ABS_INDEX_NAT)
plot(bayes_model, variable = "b_ABS_INDEX_NAT")


# Prior: small direct effect of historical polarization on outcome
priors_polar <- c(
  prior(normal(0, 0.2), class = "b", coef = "ABS_POLAR"),  # Prior on gamma (direct effect)
  prior(normal(0, 1), class = "b")  # Generic weak prior for others
)

# Bayesian model allowing for violation of exclusion restriction
bayes_polar <- brm(
  formula = PRAGMATIC_SHARE ~ ABS_POLAR_NAT + ABS_POLAR + POP_LOG + INCOME_LOG +
    FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_small,
  prior = priors_polar,
  chains = 4,
  iter = 4000,
  seed = 123
)

# Summary of results
summary(bayes_polar)

# Plot posterior for beta (causal effect of national polarization)
plot(bayes_polar, variable = "b_ABS_POLAR_NAT")


### FULL PRIORS ###

prior_sds <- c(0.01, 0.03, 0.05, 0.1, 0.15, 0.2, 0.3, 0.5, 1.0)
models <- list()

for (sd in prior_sds) {
  cat("Running model with prior SD =", sd, "\n")
  
  # Dynamically create prior expression
  prior_expr <- eval(parse(text = paste0(
    'prior(normal(0, ', sd, '), class = "b", coef = "ABS_POLAR")'
  )))
  
  # Full prior object
  prior <- c(
    prior_expr,
    prior(normal(0, 1), class = "b")
  )
  
  model <- brm(
    formula = PRAGMATIC_SHARE ~ ABS_POLAR_NAT + ABS_POLAR + POP_LOG + INCOME_LOG +
      FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
    data = IV_DATA_small,
    prior = prior,
    chains = 4, iter = 4000, warmup = 1000,
    seed = 123, refresh = 0,
    control = list(adapt_delta = 0.95, max_treedepth = 15)
  )
  
  models[[paste0("SD_", sd)]] <- model
}

summary_df <- data.frame()

for (name in names(models)) {
  model <- models[[name]]
  
  if (is.null(model)) {
    cat("WARNING: Model", name, "is NULL. Skipping.\n")
    next
  }
  
  summ <- tryCatch(summary(model), error = function(e) NULL)
  
  if (is.null(summ) || !"ABS_POLAR_NAT" %in% rownames(summ$fixed)) {
    cat("WARNING: Model", name, "is missing expected parameters. Skipping.\n")
    next
  }
  
  beta_row <- summ$fixed["ABS_POLAR_NAT", ]
  gamma_row <- summ$fixed["ABS_POLAR", ]
  
  summary_df <- rbind(summary_df, data.frame(
    Prior_SD = as.numeric(gsub("SD_", "", name)),
    
    Beta_Estimate = as.numeric(beta_row["Estimate"]),
    Beta_Lower_95_CI = as.numeric(beta_row["l-95% CI"]),
    Beta_Upper_95_CI = as.numeric(beta_row["u-95% CI"]),
    
    Gamma_Estimate = as.numeric(gamma_row["Estimate"]),
    Gamma_Lower_95_CI = as.numeric(gamma_row["l-95% CI"]),
    Gamma_Upper_95_CI = as.numeric(gamma_row["u-95% CI"])
  ))
}

summary_df

### PLOT ###

# Prepare tidy data for plotting
plot_df <- summary_df %>%
  pivot_longer(
    cols = -Prior_SD,
    names_to = c("Parameter", "Statistic"),
    names_pattern = "^(Beta|Gamma)_(Estimate|Lower|Upper).*",
    values_to = "Value"
  ) %>%
  pivot_wider(names_from = Statistic, values_from = Value)

# Set y-axis limits based on your credible intervals
y_min <- min(plot_df$Lower)
y_max <- max(plot_df$Upper)

# Plot
bayesian_plot <- ggplot(plot_df, aes(x = Prior_SD, y = Estimate)) +
  # 95% credible interval
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray80", alpha = 0.5) +
  
  # Posterior mean line
  geom_line(color = "gray10", linewidth = 1.2) +
  
  # Zero effect reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  annotate("text", x = max(plot_df$Prior_SD)*0.9, y = 0.02,
           label = "Zero effect", size = 4, family = "serif", color = "gray50") +
  
  # Axis, labels, and styling
  facet_wrap(~Parameter, scales = "free_y", nrow = 2,
             labeller = as_labeller(c("Beta" = "Treatment Effect (β)",
                                      "Gamma" = "Exclusion Violation (γ)"))) +
  labs(
    # title = "Bayesian Sensitivity Analysis | Polarization Index",
    x = "Prior SD on γ (Direct Effect of Instrument)",
    y = "Posterior Mean and 95% Credible Interval"
  ) +
  scale_x_continuous(breaks = unique(plot_df$Prior_SD)) +
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.ticks = element_line(color = "gray40"),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(bayesian_plot)

ggsave("bayesian_sensitivity_plot.png", plot = bayesian_plot, width = 8, height = 5)
ggsave("bayesian_sensitivity_plot.png", plot = bayesian_plot, width = 8, height = 5)


### PLOT 2 ###

# library(bayesplot)

priors_to_plot <- c("SD_0.01", "SD_0.1", "SD_0.5", "SD_1")

theme_set(
  theme_minimal(base_family = "serif", base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "gray40"),
      axis.ticks = element_line(color = "gray40"),
      plot.title = element_text(face = "bold", color = "gray30"),
      axis.text = element_text(color = "gray30"),
      axis.title = element_text(color = "gray30")
    )
)

color_scheme_set("gray")


density_plots <- lapply(priors_to_plot, function(name) {
  model <- models[[name]]
  mcmc_dens(model, pars = "b_ABS_POLAR_NAT") +
    ggtitle(paste("Prior SD =", gsub("SD_", "", name))) +
    labs(x = expression(paste("Posterior of ", beta)), y = "Density")
})

trace_plots <- lapply(priors_to_plot, function(name) {
  model <- models[[name]]
  mcmc_trace(model, pars = "b_ABS_POLAR_NAT", size = 0.3) +
    ggtitle(paste("Prior SD =", gsub("SD_", "", name))) +
    labs(x = "Iteration", y = expression(beta))
})

posterior_grid <- (density_plots[[1]] | density_plots[[2]]) /
  (density_plots[[3]] | density_plots[[4]])

trace_grid <- (trace_plots[[1]] | trace_plots[[2]]) /
  (trace_plots[[3]] | trace_plots[[4]])

print(posterior_grid)
print(trace_grid)

# library(posterior)  # needed for as_draws_df()

density_plots <- lapply(priors_to_plot, function(name) {
  model <- models[[name]]
  draws <- as_draws_df(model)
  
  beta_draws <- draws$b_ABS_POLAR_NAT
  mean_beta <- mean(beta_draws)
  ci <- quantile(beta_draws, probs = c(0.025, 0.975))
  
  # Compute density manually
  dens <- density(beta_draws)
  dens_df <- data.frame(x = dens$x, y = dens$y)
  
  # Subset for CI region
  ci_df <- subset(dens_df, x >= ci[1] & x <= ci[2])
  
  ggplot() +
    # CI band: shaded part of density curve
    geom_area(data = ci_df, aes(x = x, y = y),
              fill = "gray60", alpha = 0.3) +
    
    # Full density curve
    geom_area(data = dens_df, aes(x = x, y = y),
              fill = "gray85") +
    geom_line(data = dens_df, aes(x = x, y = y),
              color = "gray20", linewidth = 1.1) +
    
    # Vertical line at posterior mean
    geom_segment(aes(x = mean_beta, xend = mean_beta,
                     y = 0, yend = max(dens_df$y)),
                 color = "gray10", linewidth = 0.8) +
    
    ggtitle(paste("Prior SD =", gsub("SD_", "", name))) +
    labs(x = expression(paste("Posterior of ", beta)), y = "Density") +
    
    theme_minimal(base_family = "serif", base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "gray40"),
      axis.text = element_text(color = "gray30"),
      axis.title = element_text(color = "gray30"),
      plot.title = element_text(face = "bold", color = "gray30")
    )
})

trace_plots <- lapply(priors_to_plot, function(name) {
  model <- models[[name]]
  mcmc_trace(model, pars = "b_ABS_POLAR_NAT", size = 0.8) +
    # scale_color_manual(values = c("black", "gray40", "gray60", "gray80")) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")) +
    ggtitle(paste("Prior SD =", gsub("SD_", "", name))) +
    labs(x = "Post-warmup Iteration", y = expression(beta)) +
    theme_minimal(base_family = "serif", base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "gray40"),
      axis.text = element_text(color = "gray30"),
      axis.title = element_text(color = "gray30"),
      plot.title = element_text(face = "bold", color = "gray30")
    )
})

posterior_grid <- (density_plots[[1]] | density_plots[[2]]) /
  (density_plots[[3]] | density_plots[[4]])

trace_grid <- (trace_plots[[1]] | trace_plots[[2]]) /
  (trace_plots[[3]] | trace_plots[[4]])

print(posterior_grid)
print(trace_grid)

ggsave("posterior_grid.png", plot = posterior_grid, width = 7.5, height = 6)
ggsave("trace_grid.png", plot = trace_grid, width = 7.5, height = 6)

################### BAYESIAN SENSITIVITY - IDEOLOGY ######################

### LINEAR ###

prior_sds <- c(0.01, 0.03, 0.05, 0.1, 0.15, 0.2, 0.3, 0.5, 1.0)
models1 <- list()

for (sd in prior_sds) {
  cat("Running model with prior SD =", sd, "\n")
  
  # Dynamically create prior expression (your original, working approach)
  prior_expr1 <- eval(parse(text = paste0(
    'prior(normal(0, ', sd, '), class = "b", coef = "ABS_INDEX")'
  )))
  
  # Full prior object
  prior1 <- c(
    prior_expr1,
    prior(normal(0, 1), class = "b")
  )
  
  model1 <- brm(
    formula = PRAGMATIC_SHARE ~ ABS_INDEX_NAT + ABS_INDEX + POP_LOG + INCOME_LOG +
      FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
    data = IV_DATA_small,
    prior = prior1,
    chains = 4, iter = 4000, warmup = 1000,
    seed = 123, refresh = 0,
    control = list(adapt_delta = 0.95, max_treedepth = 15)
  )
  
  models1[[paste0("SD_", sd)]] <- model1
}

summary_df1 <- data.frame()

for (name in names(models1)) {
  model1 <- models1[[name]]
  
  if (is.null(model1)) {
    cat("WARNING: Model", name, "is NULL. Skipping.\n")
    next
  }
  
  summ <- tryCatch(summary(model1), error = function(e) NULL)
  
  if (is.null(summ) || !"ABS_INDEX_NAT" %in% rownames(summ$fixed)) {
    cat("WARNING: Model", name, "is missing expected parameters. Skipping.\n")
    next
  }
  
  beta_row1 <- summ$fixed["ABS_INDEX_NAT", ]
  gamma_row1 <- summ$fixed["ABS_INDEX", ]
  
  summary_df1 <- rbind(summary_df1, data.frame(
    Prior_SD = as.numeric(gsub("SD_", "", name)),
    
    Beta_Estimate = as.numeric(beta_row1["Estimate"]),
    Beta_Lower_95_CI = as.numeric(beta_row1["l-95% CI"]),
    Beta_Upper_95_CI = as.numeric(beta_row1["u-95% CI"]),
    
    Gamma_Estimate = as.numeric(gamma_row1["Estimate"]),
    Gamma_Lower_95_CI = as.numeric(gamma_row1["l-95% CI"]),
    Gamma_Upper_95_CI = as.numeric(gamma_row1["u-95% CI"])
  ))
}

summary_df1

### QUADRATIC ###

# Ensure the quadratic term is properly defined in your dataset
IV_DATA_B <- IV_DATA %>%
  filter(PROG_LISTA == 1 & CONS_LISTA == 1)
IV_DATA_B$ABS_INDEX_NAT_sq <- IV_DATA_B$ABS_INDEX_NAT^2
IV_DATA_B$ABS_INDEX_sq <- IV_DATA_B$ABS_INDEX^2

# Bayesian model loop for quadratic specification
prior_sds <- c(0.01, 0.03, 0.05, 0.1, 0.15, 0.2, 0.3, 0.5, 1.0)
models2 <- list()

for (prior_sd in prior_sds) {
  cat("Running model with prior SD =", prior_sd, "\n")
  
  # Define priors for both exclusion terms
  prior_expr2 <- c(
    eval(parse(text = paste0('prior(normal(0, ', prior_sd, '), class = "b", coef = "ABS_INDEX")'))),
    eval(parse(text = paste0('prior(normal(0, ', prior_sd, '), class = "b", coef = "ABS_INDEX_sq")')))
  )
  
  prior2 <- c(
    prior_expr2,
    prior(normal(0, 1), class = "b")  # weakly informative for other terms
  )
  
  model2 <- brm(
    formula = PRAGMATIC_SHARE ~ ABS_INDEX_NAT + ABS_INDEX_NAT_sq + 
      ABS_INDEX + ABS_INDEX_sq + 
      POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
    data = IV_DATA_B,
    prior = prior2,
    chains = 4, iter = 4000, warmup = 1000,
    seed = 123, refresh = 0,
    control = list(adapt_delta = 0.95, max_treedepth = 15)
  )
  
  # Use consistent model naming
  model_name <- sprintf("SD_%.2f", prior_sd)
  models2[[model_name]] <- model2
}

names(models2)

summary_df2 <- data.frame()

for (name in names(models2)) {
  model2 <- models2[[name]]
  
  if (is.null(model2)) {
    cat("WARNING: Model", name, "is NULL. Skipping.\n")
    next
  }
  
  summ <- tryCatch(summary(model2), error = function(e) NULL)
  
  if (is.null(summ) || !"ABS_INDEX_NAT" %in% rownames(summ$fixed)) {
    cat("WARNING: Model", name, "is missing expected parameters. Skipping.\n")
    next
  }
  
  beta1_row <- summ$fixed["ABS_INDEX_NAT", ]
  beta2_row <- summ$fixed["ABS_INDEX_NAT_sq", ]
  gamma1_row <- summ$fixed["ABS_INDEX", ]
  gamma2_row <- summ$fixed["ABS_INDEX_sq", ]
  
  summary_df2 <- rbind(summary_df2, data.frame(
    Prior_SD = as.numeric(gsub("SD_", "", name)),
    
    Beta_Linear_Estimate = as.numeric(beta1_row["Estimate"]),
    Beta_Linear_Lower_95_CI = as.numeric(beta1_row["l-95% CI"]),
    Beta_Linear_Upper_95_CI = as.numeric(beta1_row["u-95% CI"]),
    
    Beta_Quadratic_Estimate = as.numeric(beta2_row["Estimate"]),
    Beta_Quadratic_Lower_95_CI = as.numeric(beta2_row["l-95% CI"]),
    Beta_Quadratic_Upper_95_CI = as.numeric(beta2_row["u-95% CI"]),
    
    Gamma_Linear_Estimate = as.numeric(gamma1_row["Estimate"]),
    Gamma_Linear_Lower_95_CI = as.numeric(gamma1_row["l-95% CI"]),
    Gamma_Linear_Upper_95_CI = as.numeric(gamma1_row["u-95% CI"]),
    
    Gamma_Squared_Estimate = as.numeric(gamma2_row["Estimate"]),
    Gamma_Squared_Lower_95_CI = as.numeric(gamma2_row["l-95% CI"]),
    Gamma_Squared_Upper_95_CI = as.numeric(gamma2_row["u-95% CI"])
  ))
}

summary_df2
