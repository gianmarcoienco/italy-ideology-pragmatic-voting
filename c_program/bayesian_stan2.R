data <- read.csv("IV_DATA_small.csv")

quantile(data$ABS_POLAR_NAT)

complete_rows <- complete.cases(
  data$PRAGMATIC_SHARE,
  data$ABS_POLAR_NAT,
  data$ABS_POLAR,
  data$POP_LOG,
  data$INCOME_LOG,
  data$FOREIGN_SHARE,
  data$TERTIARY_EDU,
  data$INDEX_OLD,
  data$YEAR_DIFF,
  data$MACROAREA
)

data <- data[complete_rows, ]

# library(cmdstanr)

y <- data$PRAGMATIC_SHARE
x <- data$ABS_POLAR_NAT      
z <- data$ABS_POLAR

controls <- model.matrix(~ POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                           TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                         data = data)[, -1]  # remove intercept column

# compiled model
model <- cmdstan_model("iv_dependent_prior2.stan")

prior_sds_gamma <- c(0.01, 0.03, 0.05, 0.1, 0.15, 0.2, 0.3, 0.5, 1.0)
prior_sds_beta <- c(0.01, 0.03, 0.05, 0.1, 0.15, 0.2, 0.3, 0.5, 1.0)
prior_corrs <- c(0.0, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95)

results <- data.frame()

for (sd_gamma in prior_sds_gamma) {
  for (sd_beta in prior_sds_beta) {
    for (rho in prior_corrs) {
      if (sd_gamma < 1e-6 || sd_beta < 1e-6) next
      
      message(sprintf(">>> Fitting model with Sigma = %.3f | Tau = %.3f | Rho = %.2f",
                      sd_gamma, sd_beta, rho))
      
      stan_data <- list(
        y = y,
        x = x,
        z = z,
        controls = controls,
        N = length(y),
        K = ncol(controls),
        prior_sd_gamma = sd_gamma,
        prior_sd_beta = sd_beta,
        prior_corr = rho
      )
      
      fit <- tryCatch({
        model$sample(
          data = stan_data,
          chains = 4,
          iter_warmup = 1000,
          iter_sampling = 2000,
          seed = 123,
          refresh = 0
        )
      }, error = function(e) {
        warning(sprintf("Model failed: Sigma = %.3f | Tau = %.3f | Rho = %.2f", sd_gamma, sd_beta, rho))
        return(NULL)
      })
      
      if (is.null(fit)) next
      
      diag_summary <- fit$diagnostic_summary()
      posterior <- as_draws_df(fit)
      
      beta_vals <- posterior$beta
      gamma_vals <- posterior$gamma
      
      results <- rbind(results, data.frame(
        Prior_SD_Gamma = sd_gamma,
        Prior_SD_Beta = sd_beta,
        Prior_Corr = rho,
        Beta_Mean = mean(beta_vals),
        Beta_Lower = quantile(beta_vals, 0.025),
        Beta_Upper = quantile(beta_vals, 0.975),
        Gamma_Mean = mean(gamma_vals),
        Gamma_Lower = quantile(gamma_vals, 0.025),
        Gamma_Upper = quantile(gamma_vals, 0.975),
        Divergences = sum(diag_summary$divergent__)
      ))
      
      # write.csv(results, "BAYESIAN_STAN_PROGRESS.csv", row.names = FALSE)
    }
  }
}

# write.csv(results, file.path(A, "BAYESIAN_STAN_EXTENDED.csv"), row.names = FALSE)
# message("All models completed and results saved to 'BAYESIAN_STAN_EXTENDED.csv'.")

print(results)

results_imp <- read.csv("BAYESIAN_STAN_EXTENDED.csv")

################ SUMMARIES #########################

summary_tau3 <- results %>%
  filter(Prior_SD_Beta == 0.30)

summary_by_beta_prior <- results %>%
  group_by(Prior_SD_Beta) %>%
  summarise(
    n = n(),
    beta_mean_avg = mean(Beta_Mean, na.rm = TRUE),
    beta_mean_sd = sd(Beta_Mean, na.rm = TRUE),
    beta_ci_lower_avg = mean(Beta_Lower, na.rm = TRUE),
    beta_ci_upper_avg = mean(Beta_Upper, na.rm = TRUE),
    beta_ci_includes_zero = mean(Beta_Lower < 0 & Beta_Upper > 0),
    
    gamma_mean_avg = mean(Gamma_Mean, na.rm = TRUE),
    gamma_ci_lower_avg = mean(Gamma_Lower, na.rm = TRUE),
    gamma_ci_upper_avg = mean(Gamma_Upper, na.rm = TRUE),
    gamma_ci_includes_zero = mean(Gamma_Lower < 0 & Gamma_Upper > 0)
  ) %>%
  arrange(Prior_SD_Beta)

summary_by_gamma_prior <- results %>%
  group_by(Prior_SD_Gamma) %>%
  summarise(
    n = n(),
    gamma_mean_avg = mean(Gamma_Mean, na.rm = TRUE),
    gamma_mean_sd = sd(Gamma_Mean, na.rm = TRUE),
    gamma_ci_lower_avg = mean(Gamma_Lower, na.rm = TRUE),
    gamma_ci_upper_avg = mean(Gamma_Upper, na.rm = TRUE),
    gamma_ci_includes_zero = mean(Gamma_Lower < 0 & Gamma_Upper > 0),
    
    beta_mean_avg = mean(Beta_Mean, na.rm = TRUE),
    beta_ci_lower_avg = mean(Beta_Lower, na.rm = TRUE),
    beta_ci_upper_avg = mean(Beta_Upper, na.rm = TRUE),
    beta_ci_includes_zero = mean(Beta_Lower < 0 & Beta_Upper > 0)
  ) %>%
  arrange(Prior_SD_Gamma)

print(summary_by_gamma_prior)
print(summary_by_beta_prior)

# Full summary
summary_all <- results_imp %>%
  group_by(Prior_SD_Gamma, Prior_SD_Beta) %>%
  summarise(
    n = n(),
    beta_mean_avg = mean(Beta_Mean, na.rm = TRUE),
    gamma_mean_avg = mean(Gamma_Mean, na.rm = TRUE),
    gamma_ci_lower_avg = mean(Gamma_Lower, na.rm = TRUE),
    gamma_ci_upper_avg = mean(Gamma_Upper, na.rm = TRUE),
    beta_ci_lower_avg = mean(Beta_Lower, na.rm = TRUE),
    beta_ci_upper_avg = mean(Beta_Upper, na.rm = TRUE),
    beta_ci_includes_zero = mean(Beta_Lower < 0 & Beta_Upper > 0, na.rm = TRUE),
    gamma_ci_includes_zero = mean(Gamma_Lower < 0 & Gamma_Upper > 0, na.rm = TRUE),
    .groups = "drop"
  )
summary_all

try <- results_imp %>%
  filter(Prior_SD_Beta > 0.25)
summary(try$Beta_Mean)

summary_restricted2 <- results %>%
  filter(Prior_SD_Beta == 0.3) %>%
  group_by(Prior_SD_Gamma, Prior_SD_Beta) %>%
  summarise(
    n = n(),
    beta_mean_avg = mean(Beta_Mean, na.rm = TRUE),
    gamma_mean_avg = mean(Gamma_Mean, na.rm = TRUE),
    beta_ci_includes_zero = mean(Beta_Lower < 0 & Beta_Upper > 0, na.rm = TRUE),
    gamma_ci_includes_zero = mean(Gamma_Lower < 0 & Gamma_Upper > 0, na.rm = TRUE),
    .groups = "drop"
  )

summary_restricted2

summary_restricted <- results %>%
  filter(Prior_SD_Beta > 0.1) %>%
  group_by(Prior_SD_Gamma, Prior_SD_Beta) %>%
  summarise(
    n = n(),
    beta_mean_avg = mean(Beta_Mean, na.rm = TRUE),
    gamma_mean_avg = mean(Gamma_Mean, na.rm = TRUE),
    beta_ci_includes_zero = mean(Beta_Lower < 0 & Beta_Upper > 0, na.rm = TRUE),
    gamma_ci_includes_zero = mean(Gamma_Lower < 0 & Gamma_Upper > 0, na.rm = TRUE),
    .groups = "drop"
  )

summary_comparison <- bind_rows(
  summary_all %>% mutate(type = "All"),
  summary_restricted %>% mutate(type = "Restricted")
) %>%
  group_by(type) %>%
  summarise(
    beta_mean_avg = mean(beta_mean_avg),
    gamma_mean_avg = mean(gamma_mean_avg),
    beta_ci_include_rate = mean(beta_ci_includes_zero),
    gamma_ci_include_rate = mean(gamma_ci_includes_zero)
  )

print(summary_comparison)


######################## HEAT MAP ###########################


heat <- ggplot(summary_all, aes(x = factor(Prior_SD_Beta), y = factor(Prior_SD_Gamma), fill = beta_mean_avg)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Posterior Mean (β)", option = "D") +
    labs(
    # title = "Posterior Mean of β Across Prior Settings",
    x = expression("Prior SD on " * beta ~ (tau)),
    y = expression("Prior SD on " * gamma ~ (sigma[gamma]))
  ) +
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.ticks = element_line(color = "gray40"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(heat)

# ggsave("heat_map.png", plot = heat, width = 8, height = 5)

############# PLOT 3D STATIC ######################

# library(plotly)
# library(reshape2)

plot_tau_001 <- plot_ly(
  x = ~x_vals,
  y = ~y_vals,
  z = ~z_matrix_tau_001,
  type = "surface",
  colorscale = "Viridis",
  showscale = FALSE
) %>%
  layout(
    # title = list(text = "(a) τ = 0.01", x = 0.5),
      # text = "Posterior Mean of β (τ = 0.01)", x = 0.5)
    scene = list(
      xaxis = list(title = "Prior Correlation (ρ)", titlefont = list(family = "serif")),
      yaxis = list(title = "Prior SD of γ (σᵧ)", titlefont = list(family = "serif")),
      zaxis = list(title = "Posterior Mean of β", titlefont = list(family = "serif")),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 0.8))
    ),
    font = list(family = "serif", size = 12)
  )
plot_tau_03 <- plot_ly(
  x = ~x_vals,
  y = ~y_vals,
  z = ~z_matrix_tau_03,
  type = "surface",
  colorscale = "Viridis",
  showscale = FALSE
) %>%
  layout(
    # title = list(text = "(b) τ = 0.3", x = 0.5),
      # text = "Posterior Mean of β (τ = 0.3)", x = 0.5)
    scene = list(
      xaxis = list(title = "Prior Correlation (ρ)", titlefont = list(family = "serif")),
      yaxis = list(title = "Prior SD of γ (σᵧ)", titlefont = list(family = "serif")),
      zaxis = list(title = "Posterior Mean of β", titlefont = list(family = "serif")),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 0.8))
    ),
    font = list(family = "serif", size = 12)
  )

print(plot_tau_001)
print(plot_tau_03)

# orca(plot_tau_001, file = "beta_tau_001.png")
# orca(plot_tau_03, file = "beta_tau_03.png")
