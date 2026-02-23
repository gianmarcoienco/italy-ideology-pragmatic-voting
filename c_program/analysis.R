# ==========================================================
# analysis.R
# Performs IV estimation of the effect of ideological alignment
# on non-partisan (pragmatic) voting. Includes robustness checks
# and sensitivity analysis.
#
# Note: Requires IV_DATA dataset (not included in repo).
# ==========================================================

stopifnot(exists("A"))

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

IV_DATA_small <- IV_DATA %>%
  filter(!MUNICIPAL_SIZE %in% c("SMALL"))

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

# write.csv(IV_DATA_small, file.path(A, "IV_DATA_small.csv"), row.names = F)


#####----------------------------------------------------######
#------------------------BASELINE IV -------------------------#
#####-----------------------------------------------------#####

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

stargazer(iv_model_0, iv_model_1, iv_model_2, iv_model_3, iv_model_4,
          type = "text",
          title = "Nested IV Models with ABS_POLAR_NAT",
          column.labels = c("No Controls", "Basic", "Extended", "Full", "Robust Full"),
          dep.var.labels = "Pragmatic Share",
          keep.stat = c("n", "rsq", "adj.rsq", "f"),
          digits = 3)

summary(iv_model_4, diagnostics = TRUE)
summary(iv_model_3, diagnostics = TRUE)


#####----------------------------------------------------######
#------------------------CLUSTERING --------------------------#
#####-----------------------------------------------------#####

### PROVINCE LEVEL ###

iv_models <- list(iv_model_0, iv_model_1, iv_model_2, iv_model_3, iv_model_4)
model_names <- c("No Controls", "Basic", "Extended", "Full", "Robust Full")

cl_prov <- IV_DATA_small$COD_PROV

clustered_ses <- lapply(iv_models, vcovCL, cluster = cl_prov)
clustered_summaries <- Map(coeftest, iv_models, clustered_ses)

names(clustered_summaries) <- model_names
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

modelsummary(
  models = iv_models,
  vcov = clustered_ses,
  coef_map = coef_map,
  stars = TRUE,
  output = "latex",
  title = "IV Estimates with Clustered Standard Errors",
  gof_omit = "AIC|BIC|Log.Lik"
)

### Correct F-test ###

get_fstat_info <- function(controls_formula) {
  fml <- as.formula(paste("ABS_POLAR_NAT ~ ABS_POLAR", controls_formula))
  fs_model <- lm(fml, data = IV_DATA_small)
  fs_vcov <- vcovCL(fs_model, cluster = IV_DATA_small$COD_PROV)
  
  wtest <- waldtest(fs_model, . ~ . - ABS_POLAR, vcov = fs_vcov)
  
  f_val <- round(wtest[2, "F"], 2)
  p_val <- round(wtest[2, "Pr(>F)"], 3)
  
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

controls_list <- list(
  "",  # Model 0: No controls
  "+ POP_LOG + INCOME_LOG",
  "+ POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD",
  "+ POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA",
  "+ POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA + HIGH_TECH + LOCAL_TURNOUT"
)

fstat_table <- do.call(rbind, lapply(controls_list, get_fstat_info))
row.names(fstat_table) <- model_names
fstat_table

#### PROV-YEAR CLUSTERING ####################

IV_DATA$cluster_prov_year <- interaction(IV_DATA$COD_PROV, IV_DATA$YEAR)
IV_DATA_small$cluster_prov_year <- interaction(IV_DATA_small$COD_PROV, IV_DATA_small$YEAR)

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


#####----------------------------------------------------######
#----------------PREDICTION PLOT (POLARIZATION) --------------#
#####-----------------------------------------------------#####

abs_seq <- seq(min(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
               max(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
               length.out = 100)

controls_means <- IV_DATA_small %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), \(x) mean(x, na.rm = TRUE)))
macroarea <- "NORTH"

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

pred_df$predicted <- predict(iv_model_3, newdata = pred_df)
robust_vcov <- vcovCL(iv_model_3, cluster = IV_DATA_small$COD_PROV)

X_mat <- model.matrix(~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                        TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df)

se_pred <- sqrt(diag(X_mat %*% robust_vcov %*% t(X_mat)))
pred_df$conf.low <- pred_df$predicted - 1.96 * se_pred
pred_df$conf.high <- pred_df$predicted + 1.96 * se_pred

pred_plot_polar <- ggplot() +
  geom_point(data = IV_DATA_small,
             aes(x = ABS_POLAR_NAT, y = PRAGMATIC_SHARE),
             alpha = 0.15, size = 1, color = "gray50") +
  
  geom_ribbon(data = pred_df,
              aes(x = ABS_POLAR_NAT, ymin = conf.low, ymax = conf.high),
              fill = "gray65", alpha = 0.4) +
  geom_line(data = pred_df,
            aes(x = ABS_POLAR_NAT, y = predicted),
            color = "gray10", linewidth = 1.2) +
  
  geom_vline(xintercept = mean(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
             linetype = "dotted", color = "gray20") +
  annotate("text", x = mean(IV_DATA_small$ABS_POLAR_NAT), y = 0.10,
           label = "Sample mean", angle = 90, vjust = -0.5, size = 3.2, family = "serif", color = "gray40") +
  
  labs(
    title = "Predicted Pragmatic Voting by National Polarization",
    subtitle = "IV estimates with average controls (Macro-area = North)",
    x = "National Polarization (Balanced to Dominated)",
    y = "Pragmatic Vote Share"
  ) +
  
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = pretty(IV_DATA_small$ABS_POLAR_NAT, n = 6)) +
  
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



#####----------------------------------------------------######
#---------------------MODEL COMPARISON -----------------------#
#####-----------------------------------------------------#####

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



#####----------------------------------------------------######
#-------------ROBUSTNESS CHECK: RELATIVE IDEOLOGY ------------#
#####-----------------------------------------------------#####

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

#####----------------------------------------------------######
#----ROBUSTNESS CHECK: PARTY SUPPLY AND PRAGMATIC CHANNEL ----#
#####-----------------------------------------------------#####

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



#####----------------------------------------------------######
#-------------PREDICTION PLOT: INTERACTION -------------------#
#####-----------------------------------------------------#####

iv_model_interact <- ivreg(
  PRAGMATIC_SHARE ~ ABS_INDEX_NAT * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
    ABS_INDEX * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_small
)

abs_seq <- seq(min(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               max(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               length.out = 100)

controls_means <- IV_DATA_small %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), ~ mean(.x, na.rm = TRUE)))

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

pred_df_interact$ABS_INDEX <- pred_df_interact$ABS_INDEX_NAT
pred_df_interact$ABS_INDEX_PROG <- pred_df_interact$ABS_INDEX * pred_df_interact$PROG_LISTA

X_mat <- model.matrix(~ ABS_INDEX_NAT * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                        TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df_interact)

vcov_robust <- vcovCL(iv_model_interact, cluster = IV_DATA_small$COD_PROV)

pred_df_interact$predicted <- predict(iv_model_interact, newdata = pred_df_interact)
se_pred <- sqrt(diag(X_mat %*% vcov_robust %*% t(X_mat)))
pred_df_interact$conf.low <- pred_df_interact$predicted - 1.96 * se_pred
pred_df_interact$conf.high <- pred_df_interact$predicted + 1.96 * se_pred

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


mean_val <- mean(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE)

IV_DATA_small$PROG_LISTA <- factor(IV_DATA_small$PROG_LISTA,
                                   levels = c(0, 1),
                                   labels = c("No Progressive List", "Progressive List Present"))

pred_df_interact$GROUP <- factor(pred_df_interact$GROUP,
                                 levels = c("No Prog List", "Prog List Present"),
                                 labels = c("No Progressive List", "Progressive List Present"))

INTER_PLOT <- ggplot() +
  geom_point(data = IV_DATA_small,
             aes(x = ABS_INDEX_NAT, y = PRAGMATIC_SHARE, color = PROG_LISTA),
             alpha = 0.25, size = 1.2) +
    geom_ribbon(data = pred_df_interact,
              aes(x = ABS_INDEX_NAT, ymin = conf.low, ymax = conf.high, fill = GROUP),
              alpha = 0.3) +
    geom_line(data = pred_df_interact,
            aes(x = ABS_INDEX_NAT, y = predicted, color = GROUP),
            linewidth = 1.2) +
    geom_vline(xintercept = mean(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
             linetype = "dotted", color = "gray40") +
  annotate("text", x = mean(IV_DATA_small$ABS_INDEX_NAT), y = 0.08,
           label = "Sample mean", angle = 90, vjust = -0.5,
           size = 3.2, family = "serif", color = "gray40") +
    labs(
    # title = "Predicted Pragmatic Voting by National Ideology and Party Supply",
    # subtitle = "IV estimates with interaction by presence of Progressive List",
    title = "(b) National Ideology",
    x = "National Ideology (Progressive to Conservative)",
    y = "Pragmatic Vote Share",
    color = "Party Supply",
    fill = "Party Supply"
  ) +
    scale_color_manual(values = c("No Progressive List" = "#1f78b4",  
                                "Progressive List Present" = "#e31a1c")) +  
  scale_fill_manual(values = c("No Progressive List" = "#1f78b4",
                               "Progressive List Present" = "#e31a1c")) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = pretty(IV_DATA_small$ABS_INDEX_NAT, n = 6)) +
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

# ggsave("interaction_plot.png", plot = INTER_PLOT, width = 6.5, height = 4.5, dpi = 300)

### CONSERVATIVE ###

iv_model_interact_cons <- ivreg(
  PRAGMATIC_SHARE ~ ABS_INDEX_NAT * CONS_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
    ABS_INDEX * CONS_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_small
)

abs_seq <- seq(min(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               max(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               length.out = 100)

controls_means <- IV_DATA_small %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), ~ mean(.x, na.rm = TRUE)))

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

pred_df_interact$ABS_INDEX <- pred_df_interact$ABS_INDEX_NAT
pred_df_interact$ABS_INDEX_PROG <- pred_df_interact$ABS_INDEX * pred_df_interact$CONS_LISTA

X_mat <- model.matrix(~ ABS_INDEX_NAT * CONS_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                        TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df_interact)

vcov_robust <- vcovCL(iv_model_interact_cons, cluster = IV_DATA_small$COD_PROV)

pred_df_interact$predicted <- predict(iv_model_interact_cons, newdata = pred_df_interact)
se_pred <- sqrt(diag(X_mat %*% vcov_robust %*% t(X_mat)))
pred_df_interact$conf.low <- pred_df_interact$predicted - 1.96 * se_pred
pred_df_interact$conf.high <- pred_df_interact$predicted + 1.96 * se_pred

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

IV_DATA_small <- IV_DATA_small %>%
  mutate(
    ABS_INDEX_NAT_C = scale(ABS_INDEX_NAT, scale = FALSE),
    ABS_INDEX_C = scale(ABS_INDEX, scale = FALSE),
    ABS_INDEX_NAT_C_SQ = ABS_INDEX_NAT_C^2,
    ABS_INDEX_C_SQ = ABS_INDEX_C^2
  )

iv_model_interact_quad <- ivreg(
  PRAGMATIC_SHARE ~ ABS_INDEX_NAT_C * PROG_LISTA + ABS_INDEX_NAT_C_SQ * PROG_LISTA +
    POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
    ABS_INDEX_C * PROG_LISTA + ABS_INDEX_C_SQ * PROG_LISTA +
    POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_small
)

summary(iv_model_interact_quad)

abs_seq <- seq(min(IV_DATA_small$ABS_INDEX_NAT_C, na.rm = TRUE),
               max(IV_DATA_small$ABS_INDEX_NAT_C, na.rm = TRUE),
               length.out = 100)

controls_means <- IV_DATA_small %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), ~ mean(.x, na.rm = TRUE)))

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

pred_df_interact$ABS_INDEX_C <- pred_df_interact$ABS_INDEX_NAT_C
pred_df_interact$ABS_INDEX_C_SQ <- pred_df_interact$ABS_INDEX_C^2

X_mat_quad <- model.matrix(~ ABS_INDEX_NAT_C * PROG_LISTA + ABS_INDEX_NAT_C_SQ * PROG_LISTA +
                             POP_LOG + INCOME_LOG + FOREIGN_SHARE + TERTIARY_EDU +
                             INDEX_OLD + YEAR_DIFF + MACROAREA,
                           data = pred_df_interact)

vcov_robust <- vcovCL(iv_model_interact_quad, cluster = IV_DATA_small$COD_PROV)

pred_df_interact$predicted <- predict(iv_model_interact_quad, newdata = pred_df_interact)
se_pred_quad <- sqrt(diag(X_mat_quad %*% vcov_robust %*% t(X_mat_quad)))

pred_df_interact$conf.low <- pred_df_interact$predicted - 1.96 * se_pred_quad
pred_df_interact$conf.high <- pred_df_interact$predicted + 1.96 * se_pred_quad

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


#####----------------------------------------------------######
#--------------PREDICTION PLOT: BOTH LISTS -------------------#
#####-----------------------------------------------------#####

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

abs_seq <- seq(min(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               max(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
               length.out = 100)

controls_means <- IV_DATA_small %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), ~ mean(.x, na.rm = TRUE)))

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

pred_df_interact$ABS_INDEX <- pred_df_interact$ABS_INDEX_NAT
pred_df_interact$ABS_INDEX_PROG <- pred_df_interact$ABS_INDEX * pred_df_interact$IDEO_LIST

X_mat <- model.matrix(~ ABS_INDEX_NAT * IDEO_LIST + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                        TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df_interact)

vcov_robust <- vcovCL(iv_model_interact, cluster = IV_DATA_small$COD_PROV)

pred_df_interact$predicted <- predict(iv_model_interact, newdata = pred_df_interact)
se_pred <- sqrt(diag(X_mat %*% vcov_robust %*% t(X_mat)))
pred_df_interact$conf.low <- pred_df_interact$predicted - 1.96 * se_pred
pred_df_interact$conf.high <- pred_df_interact$predicted + 1.96 * se_pred

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

mean_val <- mean(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE)


IV_DATA_small$PROG_LISTA <- factor(IV_DATA_small$PROG_LISTA,
                                   levels = c(0, 1),
                                   labels = c("No Progressive List", "Progressive List Present"))

pred_df_interact$GROUP <- factor(pred_df_interact$GROUP,
                                 levels = c("No Prog List", "Prog List Present"),
                                 labels = c("No Progressive List", "Progressive List Present"))

interaction_plot <- ggplot() +
  geom_point(data = IV_DATA_small,
             aes(x = ABS_INDEX_NAT, y = PRAGMATIC_SHARE, color = PROG_LISTA),
             alpha = 0.25, size = 1.2) +
    geom_ribbon(data = pred_df_interact,
              aes(x = ABS_INDEX_NAT, ymin = conf.low, ymax = conf.high, fill = GROUP),
              alpha = 0.3) +
    geom_line(data = pred_df_interact,
            aes(x = ABS_INDEX_NAT, y = predicted, color = GROUP),
            linewidth = 1.2) +
    geom_vline(xintercept = mean(IV_DATA_small$ABS_INDEX_NAT, na.rm = TRUE),
             linetype = "dotted", color = "gray40") +
  annotate("text", x = mean(IV_DATA_small$ABS_INDEX_NAT), y = 0.08,
           label = "Sample mean", angle = 90, vjust = -0.5,
           size = 3.2, family = "serif", color = "gray40") +
    labs(
    title = "Predicted Pragmatic Voting by National Ideology and Party Supply",
    subtitle = "IV estimates with interaction by presence of Progressive List",
    x = "National Ideology (Progressive to Conservative)",
    y = "Pragmatic Vote Share",
    color = "Party Supply",
    fill = "Party Supply"
  ) +
    scale_color_manual(values = c("No Progressive List" = "#1f78b4",  
                                "Progressive List Present" = "#e31a1c")) + 
  scale_fill_manual(values = c("No Progressive List" = "#1f78b4",
                               "Progressive List Present" = "#e31a1c")) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = pretty(IV_DATA_small$ABS_INDEX_NAT, n = 6)) +
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


#####----------------------------------------------------######
#---------PREDICTION PLOT: POLARIZATION INTERACTION ----------#
#####-----------------------------------------------------#####

iv_model_3 <- ivreg(PRAGMATIC_SHARE ~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
                      ABS_POLAR + POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                      TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                    data = IV_DATA_small)

abs_seq <- seq(min(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
               max(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
               length.out = 100)

controls_means <- IV_DATA %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), \(x) mean(x, na.rm = TRUE)))
macroarea <- "NORTH"
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

pred_df$predicted <- predict(iv_model_3, newdata = pred_df)

robust_vcov <- vcovCL(iv_model_3, cluster = IV_DATA_small$COD_PROV)

X_mat <- model.matrix(~ ABS_POLAR_NAT + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                        TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df)

se_pred <- sqrt(diag(X_mat %*% robust_vcov %*% t(X_mat)))
pred_df$conf.low <- pred_df$predicted - 1.96 * se_pred
pred_df$conf.high <- pred_df$predicted + 1.96 * se_pred

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

iv_model_admin_interact <- ivreg(
  PRAGMATIC_SHARE ~ ABS_POLAR_NAT * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA |
    ABS_POLAR * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
    TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
  data = IV_DATA_small
)

summary(iv_model_admin_interact)

abs_seq <- seq(min(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
               max(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
               length.out = 100)

controls_means <- IV_DATA_small %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), \(x) mean(x, na.rm = TRUE)))

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

vcov_robust <- vcovCL(iv_model_admin_interact, cluster = IV_DATA_small$COD_PROV)

X_mat <- model.matrix(~ ABS_POLAR_NAT * PROG_LISTA + POP_LOG + INCOME_LOG + FOREIGN_SHARE +
                        TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df_interact)

pred_df_interact$predicted <- predict(iv_model_admin_interact, newdata = pred_df_interact)

se_pred <- sqrt(diag(X_mat %*% vcov_robust %*% t(X_mat)))

pred_df_interact$conf.low <- pred_df_interact$predicted - 1.96 * se_pred
pred_df_interact$conf.high <- pred_df_interact$predicted + 1.96 * se_pred

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

mean_val <- mean(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE)


IV_DATA_small$PROG_LISTA <- factor(IV_DATA_small$PROG_LISTA,
                                   levels = c(0, 1),
                                   labels = c("No Progressive List", "Progressive List Present"))

pred_df_interact$GROUP <- factor(pred_df_interact$GROUP,
                                 levels = c("No Prog List", "Prog List Present"),
                                 labels = c("No Progressive List", "Progressive List Present"))

interaction_plot2 <- ggplot() +
  geom_point(data = IV_DATA_small,
             aes(x = ABS_POLAR_NAT, y = PRAGMATIC_SHARE, color = PROG_LISTA),
             alpha = 0.25, size = 1.2) +
    geom_ribbon(data = pred_df_interact,
              aes(x = ABS_POLAR_NAT, ymin = conf.low, ymax = conf.high, fill = GROUP),
              alpha = 0.3) +
    geom_line(data = pred_df_interact,
            aes(x = ABS_POLAR_NAT, y = predicted, color = GROUP),
            linewidth = 1.2) +
    geom_vline(xintercept = mean(IV_DATA_small$ABS_POLAR_NAT, na.rm = TRUE),
             linetype = "dotted", color = "gray40") +
  annotate("text", x = mean(IV_DATA_small$ABS_POLAR_NAT), y = 0.08,
           label = "Sample mean", angle = 90, vjust = -0.5,
           size = 3.2, family = "serif", color = "gray40") +
    labs(
    # title = "Predicted Pragmatic Voting by National Polarization and Party Supply",
    # subtitle = "IV estimates with interaction by presence of Progressive List",
    title = "(a) National Polarization",
    x = "National Polarization (Balanced to Dominated)",
    y = "Pragmatic Vote Share",
    color = "Party Supply",
    fill = "Party Supply"
  ) +
    scale_color_manual(values = c("No Progressive List" = "#1f78b4",
                                "Progressive List Present" = "#e31a1c")) + 
  scale_fill_manual(values = c("No Progressive List" = "#1f78b4",
                               "Progressive List Present" = "#e31a1c")) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = pretty(IV_DATA_small$ABS_POLAR_NAT, n = 6)) +
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

# ggsave("interaction_plot2.png", plot = interaction_plot2, width = 6.5, height = 4.5, dpi = 300)

combined_plot <- ggarrange(interaction_plot2,
                           INTER_PLOT,
                           nrow = 1,
                           ncol = 2)

print(combined_plot)
# ggsave("combined_plot_inter.png", plot = combined_plot, width = 12, height = 4.5, dpi = 300)


#####----------------------------------------------------######
#----------------FALSIFICATION TEST: LIBRARIES ---------------#
#####-----------------------------------------------------#####

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

#####----------------------------------------------------######
#-------------------REDUCED FORM TESTS -----------------------#
#####-----------------------------------------------------#####

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


#####----------------------------------------------------######
#-------------------JACKKNIFE ROBUSTNESS ---------------------#
#####-----------------------------------------------------#####

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
  
  jackknife_coefs <- jackknife_coefs[!is.na(jackknife_coefs)]
  
  true_model <- ivreg(iv_formula, data = data)
  true_coef <- coef(true_model)[endog]
    jack_df <- data.frame(coef = jackknife_coefs)
  
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

print(jk1$plot)
print(jk2$plot)
print(jk3$plot)
print(jk4$plot)

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

# ggsave("combined_jack.png", plot = combined_jack, 
       width = 12, height = 4.5, dpi = 300, units = "in")
# ggsave("combined_jack2.png", plot = combined_jack2, 
       width = 12, height = 4.5, dpi = 300, units = "in")

#####----------------------------------------------------######
#---------------FIRST-STAGE HETEROGENEITY---------------------#
#####-----------------------------------------------------#####

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


#####----------------------------------------------------######
#-------------ROBUSTNESS CHECK: FILTER BOTH LISTS ------------#
#####-----------------------------------------------------#####

IV_DATA_B <- IV_DATA %>%
  filter(PROG_LISTA == 1 & CONS_LISTA == 1)
IV_DATA_B_small <- IV_DATA_small %>%
  filter(PROG_LISTA == 1 & CONS_LISTA == 1)

summary(IV_DATA_small$ABS_INDEX_NAT)

quantile(IV_DATA_B$ABS_POLAR_NAT)
sd(IV_DATA_B$ABS_POLAR_NAT)

quantile(IV_DATA_B$PRAGMATIC_SHARE)
sd(IV_DATA_B$PRAGMATIC_SHARE)

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



#####----------------------------------------------------######
#-----------------------QUADRATIC MODEL ----------------------#
#####-----------------------------------------------------#####

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

model_data <- model.frame(iv_model_quad)
model_data$fitted <- predict(iv_model_quad)

ggplot(model_data, aes(x = ABS_INDEX_NAT, y = fitted)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(
    title = "Fitted Relationship: ABS_INDEX_NAT vs PRAGMATIC_SHARE",
    x = "ABS_INDEX_NAT",
    y = "Fitted PRAGMATIC_SHARE"
  )

abs_seq <- seq(
  quantile(IV_DATA_B$ABS_INDEX_NAT, 0.02, na.rm = TRUE),
  quantile(IV_DATA_B$ABS_INDEX_NAT, 0.99, na.rm = TRUE),
  length.out = 100
)

controls_means <- IV_DATA_B %>%
  summarise(across(c(POP_LOG, INCOME_LOG, FOREIGN_SHARE, TERTIARY_EDU,
                     INDEX_OLD, YEAR_DIFF), ~ mean(.x, na.rm = TRUE)))

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

pred_df_quad$ABS_INDEX <- pred_df_quad$ABS_INDEX_NAT
pred_df_quad$ABS_INDEX_sq <- pred_df_quad$ABS_INDEX_NAT_sq

X_mat <- model.matrix(~ ABS_INDEX_NAT + ABS_INDEX_NAT_sq + POP_LOG + INCOME_LOG +
                        FOREIGN_SHARE + TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                      data = pred_df_quad)

vcov_robust <- vcovCL(iv_model_quad, cluster = IV_DATA_B$COD_PROV)

pred_df_quad$predicted <- predict(iv_model_quad, newdata = pred_df_quad)
se_pred <- sqrt(diag(X_mat %*% vcov_robust %*% t(X_mat)))
pred_df_quad$conf.low <- pred_df_quad$predicted - 1.96 * se_pred
pred_df_quad$conf.high <- pred_df_quad$predicted + 1.96 * se_pred

b1 <- coef(iv_model_quad)["ABS_INDEX_NAT"]
b2 <- coef(iv_model_quad)["ABS_INDEX_NAT_sq"]
peak_x <- -b1 / (2 * b2)
peak_x

pred_iv_quad <- ggplot(pred_df_quad, aes(x = ABS_INDEX_NAT, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray75", alpha = 0.4) +
  geom_line(color = "black", size = 1.2) +
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

# ggsave("pred_iv_quad.png", plot = pred_iv_quad, width = 5.5, height = 5.0, dpi = 300)
# ggsave("pred_iv_quad.png", plot = pred_iv_quad, width = 8, height = 5.0, dpi = 300)



#####----------------------------------------------------######
#--------------------CONLEY 2012 BOUNDS--------------------#
#####-----------------------------------------------------#####

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
  
sign_change <- which(diff(sign(adjusted_betas)) != 0)

  gamma1 <- gamma_seq[sign_change]
gamma2 <- gamma_seq[sign_change + 1]
beta1 <- adjusted_betas[sign_change]
beta2 <- adjusted_betas[sign_change + 1]

gamma_threshold <- gamma1 - beta1 * (gamma2 - gamma1) / (beta2 - beta1)
gamma_threshold

gamma_seq <- seq(-0.2, 0.2, by = 0.01)
z_coef <- coef(first_stage)["ABS_INDEX"]
beta_iv <- coef(iv_model)["ABS_INDEX_NAT"]
adjusted_betas <- beta_iv + gamma_seq / z_coef
conley_df <- data.frame(gamma = gamma_seq, beta = adjusted_betas)

conley_df$robust <- ifelse(conley_df$beta > 0, TRUE, FALSE)

conley_plot <- ggplot(conley_df, aes(x = gamma, y = beta)) +
  geom_ribbon(data = subset(conley_df, robust == TRUE),
              aes(ymin = 0, ymax = beta),
              fill = "gray80", alpha = 0.5) +
    geom_line(color = "gray10", linewidth = 1.2) +
    annotate("point", x = 0, y = beta_iv, color = "gray10", size = 2) +
  geom_hline(yintercept = beta_iv, linetype = "dotted", color = "gray20") +
  annotate("text", x = 0.18, y = beta_iv + 0.05,
           label = "IV estimate", size = 4, family = "serif", color = "gray40") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  annotate("text", x = 0.18, y = 0.05,
           label = "Zero effect", size = 4, family = "serif", color = "gray50") +
    geom_vline(xintercept = gamma_threshold, linetype = "dotted", color = "gray20") +
  annotate("text", x = gamma_threshold -0.03, y = min(adjusted_betas) + 0.31,
           label = sprintf("γ = %.2f", gamma_threshold),
           angle = 90, vjust = 2, size = 4, family = "serif", color = "gray40") +
  labs(
    title = "(b) Absolute Ideology Index",
    # title = "Sensitivity of IV Estimate to Exclusion Restriction Violations",
    # subtitle = "Conley bounds: Adjusted IV estimates across γ (direct effect of instrument)",
    x = "γ (Direct Effect of Historical Ideology on Pragmatic Vote Share)",
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

z_coef <- coef(first_stage)["ABS_POLAR"]  # instrument to endogenous
beta_iv <- coef(iv_model)["ABS_POLAR_NAT"]  # new IV estimate (~ -0.4)

gamma_seq <- seq(-0.2, 0.2, by = 0.01)

adjusted_betas <- beta_iv + gamma_seq / z_coef
conley_df <- data.frame(gamma = gamma_seq, beta = adjusted_betas)

conley_df$robust <- conley_df$beta < 0

sign_change <- which(diff(sign(adjusted_betas)) != 0)
gamma1 <- gamma_seq[sign_change]
gamma2 <- gamma_seq[sign_change + 1]
beta1 <- adjusted_betas[sign_change]
beta2 <- adjusted_betas[sign_change + 1]
gamma_threshold <- gamma1 - beta1 * (gamma2 - gamma1) / (beta2 - beta1)

conley_plot_pol <- ggplot(conley_df, aes(x = gamma, y = beta)) +
  geom_ribbon(data = subset(conley_df, robust == TRUE),
              aes(ymin = beta, ymax = 0),
              fill = "gray80", alpha = 0.5) +
    geom_line(color = "gray10", linewidth = 1.2) +
    annotate("point", x = 0, y = beta_iv, color = "gray10", size = 2) +
  geom_hline(yintercept = beta_iv, linetype = "dotted", color = "gray20") +
  annotate("text", x = 0.18, y = beta_iv + 0.05,
           label = "IV estimate", size = 4, family = "serif", color = "gray40") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  annotate("text", x = 0.09, y = 0.05,  # moved from x = 0.18 to x = 0.13
           label = "Zero effect", size = 4, family = "serif", color = "gray50") +
    geom_vline(xintercept = gamma_threshold, linetype = "dotted", color = "gray20") +
  annotate("text", x = gamma_threshold, y = min(adjusted_betas) + 0.15,
           label = sprintf("γ = %.2f", gamma_threshold),
           angle = 90, vjust = -0.7, size = 4, family = "serif", color = "gray40") +
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

# ggsave("conley_combined2.png", plot = combined_plot2, width = 6.5, height = 7.2, dpi = 300)
