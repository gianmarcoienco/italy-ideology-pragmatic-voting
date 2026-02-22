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
model <- cmdstan_model("iv_dependent_prior2.stan")

# Define prior grids
prior_sds_gamma <- c(0.01, 0.03, 0.05, 0.1, 0.15, 0.2, 0.3, 0.5, 1.0)
prior_sds_beta <- c(0.01, 0.03, 0.05, 0.1, 0.15, 0.2, 0.3, 0.5, 1.0)
prior_corrs <- c(0.0, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95)

# Initialize results
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
      
      # Save intermediate progress
      write.csv(results, "BAYESIAN_STAN_PROGRESS.csv", row.names = FALSE)
    }
  }
}

# Save results
write.csv(results, file.path(A, "BAYESIAN_STAN_EXTENDED.csv"), row.names = FALSE)
message("All models completed and results saved to 'BAYESIAN_STAN_EXTENDED.csv'.")

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

# Restricted summary: excluding small values of tau (e.g., ≤ 0.05)
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

# Restricted summary: excluding small values of tau (e.g., ≤ 0.05)
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

# Aggregated comparison
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
  
  # Labels using plotmath expressions
  labs(
    # title = "Posterior Mean of β Across Prior Settings",
    x = expression("Prior SD on " * beta ~ (tau)),
    y = expression("Prior SD on " * gamma ~ (sigma[gamma]))
  ) +
  
  # Styling to match your aesthetic
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

ggsave("heat_map.png", plot = heat, width = 8, height = 5)

############# PLOT 3D STATIC ######################

# library(plotly)
# library(reshape2)

# Create the surface plot for τ = 0.01
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

# Same for τ = 0.3
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

# Save static images of your Plotly plots
orca(plot_tau_001, file = "beta_tau_001.png")
orca(plot_tau_03, file = "beta_tau_03.png")


############# PLOT ANIMATED 3D ###############

# Ensure numeric encoding
results <- results %>%
  mutate(
    Prior_SD_Beta = as.numeric(Prior_SD_Beta),
    Prior_SD_Gamma = as.numeric(Prior_SD_Gamma),
    Prior_Corr = as.numeric(Prior_Corr)
  )

# Prepare animation frames (one per Prior_SD_Beta)
frames <- unique(results$Prior_SD_Beta)
frames <- sort(frames)

plot_data <- lapply(frames, function(beta_sd) {
  df <- results %>% filter(Prior_SD_Beta == beta_sd)
  list(
    z = matrix(df$Beta_Mean, nrow = length(unique(df$Prior_SD_Gamma)), byrow = TRUE),
    x = sort(unique(df$Prior_Corr)),
    y = sort(unique(df$Prior_SD_Gamma)),
    name = paste("τ =", beta_sd)
  )
})

# Build initial plot
p <- plot_ly(
  type = "surface",
  z = plot_data[[1]]$z,
  x = plot_data[[1]]$x,
  y = plot_data[[1]]$y,
  frame = plot_data[[1]]$name,
  colorscale = "Viridis"
) %>%
  layout(
    title = "Posterior Mean of β across Prior Correlation and γ SD",
    scene = list(
      xaxis = list(title = "Prior Correlation (ρ)"),
      yaxis = list(title = "Prior SD of γ"),
      zaxis = list(title = "Posterior Mean of β")
    ),
    updatemenus = list(
      list(
        type = "buttons",
        buttons = list(
          list(method = "animate", args = list(NULL, list(frame = list(duration = 1000, redraw = TRUE))), label = "Play")
        )
      )
    )
  ) %>%
  animation_opts(frame = 1000, transition = 0, redraw = TRUE)

# Add frames for each β SD
for (i in seq_along(plot_data)) {
  p <- add_surface(p,
                   z = plot_data[[i]]$z,
                   x = plot_data[[i]]$x,
                   y = plot_data[[i]]$y,
                   name = plot_data[[i]]$name,
                   showscale = FALSE,
                   frame = plot_data[[i]]$name)
}

p


### GAMMA ###


# Ensure numeric types
results <- results %>%
  mutate(
    Prior_SD_Beta = as.numeric(Prior_SD_Beta),
    Prior_SD_Gamma = as.numeric(Prior_SD_Gamma),
    Prior_Corr = as.numeric(Prior_Corr)
  )

# Axis values
x_vals <- sort(unique(results$Prior_Corr))           # ρ
y_vals <- sort(unique(results$Prior_SD_Gamma))       # σ_γ
frame_vals <- sort(unique(results$Prior_SD_Beta))    # τ

# Define serif-style font
font_serif <- list(family = "serif", size = 12, color = "gray20")

# Build animation frames
frames_list <- lapply(frame_vals, function(beta_sd) {
  df <- results %>% filter(Prior_SD_Beta == beta_sd)
  
  z_matrix <- matrix(df$Gamma_Mean,
                     nrow = length(y_vals),
                     ncol = length(x_vals),
                     byrow = TRUE)
  
  list(
    name = paste0("τ = ", beta_sd),
    data = list(
      list(
        type = "surface",
        x = x_vals,
        y = y_vals,
        z = z_matrix,
        colorscale = "Viridis",
        showscale = TRUE,
        colorbar = list(title = "γ̄", titlefont = font_serif, tickfont = font_serif)
      )
    )
  )
})

# Initial surface (τ = first value)
df_init <- results %>% filter(Prior_SD_Beta == frame_vals[1])
z_init <- matrix(df_init$Gamma_Mean,
                 nrow = length(y_vals),
                 ncol = length(x_vals),
                 byrow = TRUE)

# Axis layout
scene_layout <- list(
  xaxis = list(title = "Prior Correlation (ρ)", titlefont = font_serif, tickfont = font_serif, showgrid = FALSE, zeroline = FALSE),
  yaxis = list(title = "Prior SD of γ (σ_γ)", titlefont = font_serif, tickfont = font_serif, showgrid = FALSE, zeroline = FALSE),
  zaxis = list(title = "Posterior Mean of γ", titlefont = font_serif, tickfont = font_serif, showgrid = FALSE, zeroline = FALSE),
  bgcolor = "white"
)

# Create animated plot
fig <- plot_ly() %>%
  add_surface(
    x = x_vals,
    y = y_vals,
    z = z_init,
    colorscale = "Viridis",
    showscale = TRUE,
    colorbar = list(title = "γ̄", titlefont = font_serif, tickfont = font_serif)
  ) %>%
  layout(
    title = list(text = "Posterior Mean of γ across ρ and σ_γ (animated over τ)", font = font_serif),
    scene = scene_layout,
    updatemenus = list(
      list(
        type = "buttons",
        buttons = list(
          list(
            label = "Play",
            method = "animate",
            args = list(NULL, list(frame = list(duration = 1000, redraw = TRUE)))
          )
        )
      )
    ),
    sliders = list(
      list(
        active = 0,
        steps = lapply(seq_along(frame_vals), function(i) {
          beta_sd <- frame_vals[i]
          list(
            label = paste0("τ = ", beta_sd),
            method = "animate",
            args = list(list(paste0("τ = ", beta_sd)),
                        list(mode = "immediate", frame = list(duration = 1000, redraw = TRUE)))
          )
        })
      )
    )
  ) %>%
  animation_opts(frame = 1000, redraw = TRUE) %>%
  config(displayModeBar = FALSE)

# Attach frames
fig$x$frames <- frames_list

# Show
fig

### BETA ###

# Ensure numeric
results <- results %>%
  mutate(
    Prior_SD_Beta = as.numeric(Prior_SD_Beta),
    Prior_SD_Gamma = as.numeric(Prior_SD_Gamma),
    Prior_Corr = as.numeric(Prior_Corr)
  )

# Define values
x_vals <- sort(unique(results$Prior_Corr))           # ρ
y_vals <- sort(unique(results$Prior_SD_Gamma))       # σ_γ
frame_vals <- sort(unique(results$Prior_SD_Beta))    # τ

# Font
font_serif <- list(family = "serif", size = 12, color = "gray20")

# Animation frames
frames_list_beta <- lapply(frame_vals, function(beta_sd) {
  df <- results %>% filter(Prior_SD_Beta == beta_sd)
  
  z_matrix <- matrix(df$Beta_Mean,
                     nrow = length(y_vals),
                     ncol = length(x_vals),
                     byrow = TRUE)
  
  list(
    name = paste0("τ = ", beta_sd),
    data = list(
      list(
        type = "surface",
        x = x_vals,
        y = y_vals,
        z = z_matrix,
        colorscale = "Viridis",
        showscale = TRUE,
        colorbar = list(title = "β̄", titlefont = font_serif, tickfont = font_serif)
      )
    )
  )
})

# Initial z
df_init_beta <- results %>% filter(Prior_SD_Beta == frame_vals[1])
z_init_beta <- matrix(df_init_beta$Beta_Mean,
                      nrow = length(y_vals),
                      ncol = length(x_vals),
                      byrow = TRUE)

# Layout
scene_layout_beta <- list(
  xaxis = list(title = "Prior Correlation (ρ)", titlefont = font_serif, tickfont = font_serif, showgrid = FALSE),
  yaxis = list(title = "Prior SD of γ (σ_γ)", titlefont = font_serif, tickfont = font_serif, showgrid = FALSE),
  zaxis = list(title = "Posterior Mean of β", titlefont = font_serif, tickfont = font_serif, showgrid = FALSE),
  bgcolor = "white"
)

# Plot
fig_beta <- plot_ly() %>%
  add_surface(
    x = x_vals,
    y = y_vals,
    z = z_init_beta,
    colorscale = "Viridis",
    showscale = TRUE,
    colorbar = list(title = "β̄", titlefont = font_serif, tickfont = font_serif)
  ) %>%
  layout(
    title = list(text = "Posterior Mean of β across ρ and σ_γ (animated over τ)", font = font_serif),
    scene = scene_layout_beta,
    updatemenus = list(
      list(
        type = "buttons",
        buttons = list(
          list(
            label = "Play",
            method = "animate",
            args = list(NULL, list(frame = list(duration = 1000, redraw = TRUE)))
          )
        )
      )
    ),
    sliders = list(
      list(
        active = 0,
        steps = lapply(seq_along(frame_vals), function(i) {
          beta_sd <- frame_vals[i]
          list(
            label = paste0("τ = ", beta_sd),
            method = "animate",
            args = list(list(paste0("τ = ", beta_sd)),
                        list(mode = "immediate", frame = list(duration = 1000, redraw = TRUE)))
          )
        })
      )
    )
  ) %>%
  animation_opts(frame = 1000, redraw = TRUE) %>%
  config(displayModeBar = FALSE)

# Attach frames
fig_beta$x$frames <- frames_list_beta

# Display
fig_beta


htmlwidgets::saveWidget(fig, "/Users/gianmarcoienco/Desktop/personal/projects/project_govt/d_results/3D_plots/posterior_gamma_3d.html")
htmlwidgets::saveWidget(fig_beta, "/Users/gianmarcoienco/Desktop/personal/projects/project_govt/d_results/3D_plots/posterior_beta_3d.html")


### COMPARISON ###

# Ensure numeric
results <- results %>%
  mutate(
    Prior_SD_Beta = as.numeric(Prior_SD_Beta),
    Prior_SD_Gamma = as.numeric(Prior_SD_Gamma),
    Prior_Corr = as.numeric(Prior_Corr)
  )

# Define axes and frame values
x_vals <- sort(unique(results$Prior_Corr))
y_vals <- sort(unique(results$Prior_SD_Gamma))
frame_vals <- sort(unique(results$Prior_SD_Beta))

font_serif <- list(family = "serif", size = 12, color = "gray20")

# Create shared frames with both β and γ surfaces
frames_dual <- lapply(frame_vals, function(beta_sd) {
  df <- results %>% filter(Prior_SD_Beta == beta_sd)
  
  z_beta <- matrix(df$Beta_Mean, nrow = length(y_vals), ncol = length(x_vals), byrow = TRUE)
  z_gamma <- matrix(df$Gamma_Mean, nrow = length(y_vals), ncol = length(x_vals), byrow = TRUE)
  
  list(
    name = paste0("τ = ", beta_sd),
    data = list(
      list(
        type = "surface",
        x = x_vals,
        y = y_vals,
        z = z_beta,
        colorscale = "Viridis",
        showscale = FALSE,
        scene = "scene"
      ),
      list(
        type = "surface",
        x = x_vals,
        y = y_vals,
        z = z_gamma,
        colorscale = "Cividis",
        showscale = FALSE,
        scene = "scene2"
      )
    )
  )
})

# Initial data
init_df <- results %>% filter(Prior_SD_Beta == frame_vals[1])
z_beta_init <- matrix(init_df$Beta_Mean, nrow = length(y_vals), ncol = length(x_vals), byrow = TRUE)
z_gamma_init <- matrix(init_df$Gamma_Mean, nrow = length(y_vals), ncol = length(x_vals), byrow = TRUE)

# Plot
fig <- plot_ly()

# β surface in scene 1
fig <- fig %>% add_surface(
  x = x_vals, y = y_vals, z = z_beta_init,
  colorscale = "Viridis",
  name = "β",
  showscale = FALSE,
  scene = "scene"
)

# γ surface in scene 2
fig <- fig %>% add_surface(
  x = x_vals, y = y_vals, z = z_gamma_init,
  colorscale = "Cividis",
  name = "γ",
  showscale = FALSE,
  scene = "scene2"
)

# Layout
fig <- fig %>%
  layout(
    title = list(text = "Posterior Means of β and γ (animated over τ)", font = font_serif),
    scene = list(
      xaxis = list(title = "ρ", titlefont = font_serif),
      yaxis = list(title = "σ_γ", titlefont = font_serif),
      zaxis = list(title = "β", titlefont = font_serif),
      domain = list(x = c(0, 0.48)),
      bgcolor = "white"
    ),
    scene2 = list(
      xaxis = list(title = "ρ", titlefont = font_serif),
      yaxis = list(title = "σ_γ", titlefont = font_serif),
      zaxis = list(title = "γ", titlefont = font_serif),
      domain = list(x = c(0.52, 1)),
      bgcolor = "white"
    ),
    updatemenus = list(
      list(
        type = "buttons",
        buttons = list(
          list(
            label = "Play",
            method = "animate",
            args = list(NULL, list(frame = list(duration = 1000, redraw = TRUE)))
          )
        )
      )
    ),
    sliders = list(
      list(
        active = 0,
        steps = lapply(seq_along(frame_vals), function(i) {
          beta_sd <- frame_vals[i]
          list(
            label = paste0("τ = ", beta_sd),
            method = "animate",
            args = list(list(paste0("τ = ", beta_sd)),
                        list(mode = "immediate", frame = list(duration = 1000, redraw = TRUE)))
          )
        })
      )
    )
  ) %>%
  animation_opts(frame = 1000, redraw = TRUE) %>%
  config(displayModeBar = FALSE)

# Attach frames
fig$x$frames <- frames_dual

# Show
fig
