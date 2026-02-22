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

data <- read.csv("IV_DATA_small.csv")

quantile(data$ABS_POLAR_NAT)

# Identify complete cases across all key variables
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

# Filter data
data <- data[complete_rows, ]

# library(cmdstanr)

# Define variables
y <- data$PRAGMATIC_SHARE
x <- data$ABS_POLAR_NAT      
z <- data$ABS_POLAR

controls <- model.matrix(~ POP_LOG + INCOME_LOG + FOREIGN_SHARE + 
                           TERTIARY_EDU + INDEX_OLD + YEAR_DIFF + MACROAREA,
                         data = data)[, -1]  # remove intercept column

# Load compiled model
model <- cmdstan_model("iv_dependent_prior.stan")

# Define prior grid
prior_sds <- c(0.01, 0.03, 0.05, 0.1, 0.15, 0.2, 0.3, 0.5, 1.0)
prior_corrs <- c(0.0, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95)

# Initialize results
results <- data.frame()

# Loop over prior SDs and correlations
for (sd in prior_sds) {
  for (rho in prior_corrs) {
    if (sd < 1e-6) next  # skip invalid SDs
    
    message(sprintf(">>> Fitting model with Prior SD = %.3f | Corr = %.2f", sd, rho))
    
    stan_data <- list(
      y = y,
      x = x,
      z = z,
      controls = controls,
      N = length(y),
      K = ncol(controls),
      prior_sd = sd,
      prior_corr = rho
    )
    
    fit <- model$sample(
      data = stan_data,
      chains = 4,
      iter_warmup = 1000,
      iter_sampling = 2000,
      seed = 123,
      refresh = 0
    )
    
    # Check for divergent transitions
    diag_summary <- fit$diagnostic_summary()
    if (any(diag_summary$divergent__ > 0)) {
      warning(sprintf("WARNING: Divergences detected at SD = %.3f, Corr = %.2f", sd, rho))
    }
    
    posterior <- as_draws_df(fit)
    
    beta_vals <- posterior$beta
    gamma_vals <- posterior$gamma
    
    results <- rbind(results, data.frame(
      Prior_SD = sd,
      Prior_Corr = rho,
      Beta_Mean = mean(beta_vals),
      Beta_Lower = quantile(beta_vals, 0.025),
      Beta_Upper = quantile(beta_vals, 0.975),
      Gamma_Mean = mean(gamma_vals),
      Gamma_Lower = quantile(gamma_vals, 0.025),
      Gamma_Upper = quantile(gamma_vals, 0.975),
      Divergences = sum(diag_summary$divergent__)
    ))
    
    # Optional: Uncomment to pause between runs
    # readline(prompt = "Press [Enter] to continue to next model...")
  }
}

# Save output to file
write.csv(results, "/Users/gianmarcoienco/Desktop/personal/projects/project_govt/a_microdata/BAYESIAN_STAN.csv", row.names = FALSE)
message("All models completed and results saved to 'BAYESIAN_STAN.csv'.")

print(results)

results_imp2 <- read.csv("BAYESIAN_STAN.csv")

try2 <- results_imp2 %>%
  filter(Prior_SD > 0.25)
summary(try2$Beta_Mean)

plot1 <- ggplot(results, aes(x = Prior_SD, y = Beta_Mean, color = factor(Prior_Corr), fill = factor(Prior_Corr))) +
  # 95% Credible interval ribbons
  geom_ribbon(aes(ymin = Beta_Lower, ymax = Beta_Upper), alpha = 0.2, color = NA) +
  
  # Posterior mean lines
  geom_line(linewidth = 1.2) +
  
  # Vertical line to mark robustness threshold
  geom_vline(xintercept = 0.3, linetype = "dotted", color = "gray40") +
  annotate("text", x = 0.28, y = min(results$Beta_Lower, na.rm = TRUE) + 0.02,
           label = "Stabilization threshold", angle = 90, hjust = 0, vjust = -0.2,
           size = 3.5, family = "serif", color = "gray20") +
  
  # Horizontal line for zero effect
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  
  annotate("text", x = max(results$Prior_SD) * 0.95, y = 0.02, 
           label = "Zero effect", family = "serif", size = 4, color = "gray50") +
  
  # Axes and legend
  labs(
    # title = "Bayesian Sensitivity of Treatment Effect (β)",
    # subtitle = "By prior SD and structural correlation with γ",
    x = "Prior SD on γ (Direct Effect of Instrument)",
    y = "Posterior Mean of β with 95% Credible Interval",
    color = "Prior Correlation", fill = "Prior Correlation"
  ) +
  
  scale_x_continuous(breaks = unique(results$Prior_SD)) +
  
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.ticks = element_line(color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

print(plot1)

plot_dep_df <- results %>%
  pivot_longer(
    cols = c(Beta_Mean, Gamma_Mean, Beta_Lower, Beta_Upper, Gamma_Lower, Gamma_Upper),
    names_to = c("Parameter", ".value"),
    names_pattern = "(Beta|Gamma)_(.*)"
  ) %>%
  mutate(
    Parameter = factor(
      case_when(
        Parameter == "Beta" ~ "Treatment Effect (β)",
        Parameter == "Gamma" ~ "Exclusion Violation (γ)",
        TRUE ~ Parameter
      ),
      levels = c("Treatment Effect (β)", "Exclusion Violation (γ)")
    )
  )

# Plot
bayesian_dep_plot <- ggplot(plot_dep_df, aes(x = Prior_SD, y = Mean)) +
  # 95% credible interval
  geom_ribbon(aes(ymin = Lower, ymax = Upper, group = Prior_Corr), fill = "gray80", alpha = 0.5) +
  geom_line(aes(group = Prior_Corr), color = "gray10", linewidth = 1.2) +
  
  # Zero effect reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  annotate("text", x = max(plot_dep_df$Prior_SD)*0.9, y = 0.02,
           label = "Zero effect", size = 4, family = "serif", color = "gray50") +
  
  geom_vline(xintercept = 0.3, linetype = "dotted", color = "gray50") +
annotate("text", x = 0.32, y = -0.25, label = "Robustness threshold", 
         angle = 90, hjust = 0, size = 3, family = "serif", color = "gray50") +
  
  # Facet and styling
  facet_wrap(~Parameter, scales = "free_y", nrow = 2) +
  labs(
    # title = "Bayesian Sensitivity Analysis with Dependent Priors",
    x = "Prior SD on γ (Direct Effect of Instrument)",
    y = "Posterior Mean and 95% Credible Interval"
  ) +
  scale_x_continuous(breaks = unique(plot_dep_df$Prior_SD)) +
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

print(bayesian_dep_plot)

ggsave("beta_sensitivity_plot.pdf", plot = plot1, width = 8, height = 5)
ggsave("bayesian_dep_plot.pdf", plot = bayesian_dep_plot, width = 8, height = 5)


