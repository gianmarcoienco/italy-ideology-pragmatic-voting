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

c2018 <- read.csv("2018.csv")

parties <- LOAD(dfinput = "parties", pattdir = A)

HIST_IDEOLOGY <- read.csv("HIST_IDEOLOGY.csv")

map_ready <- LOAD(dfinput = "historical", pattdir = A)

# Fix Excel issue with +Europa party
c2018 <- c2018 %>%
  mutate(
    LISTA = ifelse(LISTA == "#NAME?", "+EUROPA", LISTA)
  )

c2018 <- c2018 %>% RENAME2() %>% NORMALIZE()

# Add provinces

HIST_IDEOLOGY <- HIST_IDEOLOGY %>%
  select(ID, COMUNE, PROVINCIA, COD_PROV, COD_UTS)

c2018 <- c2018 %>%
  left_join(HIST_IDEOLOGY, by = "COMUNE")

c2018 %>%
  filter(is.na(PROVINCIA)) %>%
  distinct(COMUNE) %>%
  arrange(COMUNE)

# Fix Valle d'Aosta data issue

c2018 <- c2018 %>%
  mutate(VOTI_LISTA = coalesce(VOTI_LISTA, VOTI_CANDIDATO))

# Attribute Ideology

c2018 <- c2018 %>%
  left_join(parties, by = c("LISTA" = "PARTY")) %>%
  rename(IDEOLOGY = IDEOLOGY)

ideology_2018 <- IDEOLOGY(c2018)

ideology_2018 %>%
  count(COMUNE) %>%
  filter(n > 1)

summary(ideology_2018$net_relative_ideology)

ideology_2018 <- ideology_2018 %>%
  left_join(HIST_IDEOLOGY, by = "COMUNE")

skewness(ideology_2018$net_absolute_ideology)
skewness(ideology_2018$net_relative_ideology)
skewness(ideology_2018$abs_polarization)
skewness(ideology_2018$rel_polarization)

ideology_2018 <- ideology_2018 %>%
  mutate(
    abs_pol_sqrt = sqrt(abs(abs_polarization)),
    rel_pol_sqrt = sqrt(abs(rel_polarization))
  )

NAT_IDEOLOGY <- ideology_2018 %>%
  select(ID, COMUNE, PROVINCIA, COD_PROV, COD_UTS, net_absolute_ideology, net_relative_ideology, abs_polarization, abs_pol_sqrt,
         rel_polarization, rel_pol_sqrt) %>%
  rename(
    ABS_INDEX_NAT = net_absolute_ideology,
    REL_INDEX_NAT = net_relative_ideology,
    ABS_POLAR_NAT = abs_polarization,
    ABS_POLAR_SQRT_NAT = abs_pol_sqrt,
    REL_POLAR_NAT = rel_polarization,
    REL_POLAR_SQRT_NAT = rel_pol_sqrt
  )

SAVE(dfx = NAT_IDEOLOGY, pattdir = A)

write.csv(NAT_IDEOLOGY, "/Users/gianmarcoienco/Desktop/personal/projects/project_govt/a_microdata/NAT_IDEOLOGY.csv", row.names = F)

####### FINAL MAPPING ##################

map_ideology_2018 <- map_ready %>%
  left_join(ideology_2018, by = c("COMUNE", "PROVINCIA", "COD_PROV", "COD_UTS"))

# Reassign sf class if it's dropped
if (!inherits(map_ideology_2018, "sf")) {
  map_ideology_2018 <- sf::st_as_sf(map_ideology_2018)
}

ideology_2018 %>%
  filter(!(COMUNE %in% map_ideology_2018$COMUNE))

class(map_ideology_2018)

province_sf <- map_ideology_2018 %>%
  group_by(COD_PROV) %>%  # Adjust to your province identifier
  summarise(geometry = sf::st_union(geometry))

# map_baseline <- readRDS("MAP_BASELINE.rds")
# saveRDS(map_ideology_2018, "MAP_BASELINE.rds")

############# MAP FINAL 1 - ABS_INDEX ###########

# # Shared limits and breaks
# shared_limits <- range(
#   c(map_ideology_2018$abs_index, map_ideology_2018$net_absolute_ideology),
#   na.rm = TRUE
# )
# breaks <- c(-1, 0, 1)
# labels <- c("−1", "0", "+1")
# 
# # Define the fill scale as a shared object
# fill_scale <- scale_fill_gradient2(
#   low = "red", mid = "white", high = "blue",
#   midpoint = 0,
#   limits = shared_limits,
#   breaks = breaks,
#   labels = labels,
#   name = "Absolute Ideological Index",
#   guide = guide_colorbar(
#     title.position = "left",
#     barheight = unit(100, "pt"),
#     barwidth = unit(10, "pt"),
#     title.theme = element_text(angle = 90, size = 10)
#   )
# )
# 
# # Map (a): Historical Ideology
# map_hist <- ggplot(map_ideology_2018) +
#   geom_sf(aes(fill = abs_index), color = NA) +
#   geom_sf(data = province_sf, fill = NA, color = "black", linewidth = 0.1) +
#   fill_scale +
#   labs(title = "(a) Historical Ideological Orientation") +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "serif"),
#     plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
#     legend.position = "right",
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   )
# 
# # Map (b): 2018 Ideology
# map_2018 <- ggplot(map_ideology_2018) +
#   geom_sf(aes(fill = net_absolute_ideology), color = NA) +
#   geom_sf(data = province_sf, fill = NA, color = "black", linewidth = 0.1) +
#   fill_scale +
#   labs(title = "(b) Ideological Orientation (2018)") +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "serif"),
#     plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
#     legend.position = "none",
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   )
# 
# # Combine maps with shared legend
# combined <- map_hist + map_2018 + plot_layout(guides = "collect") &
#   theme(legend.position = "right")
# 
# # Save
# ggsave("ideology_comparison_maps4.pdf", plot = combined, width = 11, height = 6, dpi = 300)
# print(combined)

############# MAP FINAL 1.1 - ABS_INDEX ###########

# # Shared limits and breaks
# shared_limits <- range(
#   c(map_ideology_2018$abs_index, map_ideology_2018$net_absolute_ideology),
#   na.rm = TRUE
# )
# breaks <- c(-0.5, 0, 0.5)
# labels <- c("−0.5", "0", "+0.5")
# 
# # Define the shared fill scale with a visible and well-formatted guide
# fill_scale <- scale_fill_gradient2(
#   low = "red", mid = "white", high = "blue",
#   midpoint = 0,
#   limits = shared_limits,
#   breaks = breaks,
#   labels = labels,
#   name = "Absolute Ideological Index",
#   guide = guide_colorbar(
#     title.position = "top",
#     title.theme = element_text(size = 10, face = "bold", family = "serif"),
#     label.theme = element_text(size = 8, family = "serif"),
#     barheight = unit(0.3, "in"),
#     barwidth = unit(3, "in")
#   )
# )
# 
# # Map (a): Historical Ideology
# map_hist <- ggplot(map_ideology_2018) +
#   geom_sf(aes(fill = abs_index), color = NA) +
#   geom_sf(data = province_sf, fill = NA, color = "black", linewidth = 0.1) +
#   fill_scale +
#   labs(title = "(a) Historical Ideological Orientation") +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "serif"),
#     plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
#     legend.position = "bottom",
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   )
# 
# # Map (b): 2018 Ideology
# map_2018 <- ggplot(map_ideology_2018) +
#   geom_sf(aes(fill = net_absolute_ideology), color = NA) +
#   geom_sf(data = province_sf, fill = NA, color = "black", linewidth = 0.1) +
#   fill_scale +
#   labs(title = "(b) Ideological Orientation (2018)") +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "serif"),
#     plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
#     legend.position = "none",
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   )
# 
# # Combine maps with shared horizontal legend at the bottom
# combined <- map_hist + map_2018 + plot_layout(guides = "collect") &
#   theme(legend.position = "bottom")
# 
# # Save and print
# ggsave("ideology_comparison_maps_updated.pdf", plot = combined, width = 11, height = 6, dpi = 300)
# ggsave("ideology_comparison_maps.png", plot = combined, width = 11, height = 6, dpi = 300)
# print(combined)

################ MAP FINAL 1.2 - ABS_INDEX (facet) #######################

# # Combine both variables into one column with labels
# map_data_combined <- bind_rows(
#   map_ideology_2018 %>%
#     mutate(fill_value = abs_index, period = "(a) Historical Ideological Orientation"),
#   map_ideology_2018 %>%
#     mutate(fill_value = net_absolute_ideology, period = "(b) Ideological Orientation (2018)")
# )
# 
# # Shared limits and breaks
# shared_limits <- range(map_data_combined$fill_value, na.rm = TRUE)
# breaks <- c(-0.5, 0, 0.5)
# labels <- c("−0.5", "0", "+0.5")
# 
# # Shared fill scale
# fill_scale <- scale_fill_gradient2(
#   low = "red", mid = "white", high = "blue",
#   midpoint = 0,
#   limits = shared_limits,
#   breaks = breaks,
#   labels = labels,
#   name = "Absolute Ideology Index",
#   guide = guide_colorbar(
#     title.position = "right",  # moves title above the colorbar vertically
#     title.theme = element_text(size = 10, face = "bold", family = "serif", angle = 90),
#     label.theme = element_text(size = 8, family = "serif"),
#     barheight = unit(3, "in"),  # taller bar
#     barwidth = unit(0.25, "in") # narrower width
#   )
# )
# 
# # Plot with facet
# combined_map <- ggplot(map_data_combined) +
#   geom_sf(aes(fill = fill_value, geometry = geometry), color = NA) +
#   geom_sf(data = province_sf, fill = NA, color = "black", linewidth = 0.1) +
#   fill_scale +
#   facet_wrap(~period, nrow = 1) +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "serif"),
#     strip.text = element_text(size = 13, face = "bold", family = "serif"),
#     legend.position = "right",
#     legend.title = element_text(size = 9, hjust = 0.5),
#     legend.text = element_text(size = 8),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   )
# 
# # Save and print
# ggsave("ideology_comparison_facet.pdf", plot = combined_map, width = 11, height = 6, dpi = 300)
# ggsave("ideology_comparison_facet.png", plot = combined_map, width = 11, height = 6, dpi = 300)
# print(combined_map)

################ MAP FINAL 1.3 - ABS_INDEX (M5S) #########################

# # Combine both variables into one column with labels
# map_data_combined <- bind_rows(
#   map_baseline %>%
#     mutate(fill_value = net_absolute_ideology, period = "(a) M5S Progressive (2018)"),
#   map_ideology_2018 %>%
#     mutate(fill_value = net_absolute_ideology, period = "(b) M5S Excluded (2018)")
# )
# 
# # Shared limits and breaks
# shared_limits <- range(map_data_combined$fill_value, na.rm = TRUE)
# breaks <- c(-0.5, 0, 0.5)
# labels <- c("−0.5", "0", "+0.5")
# 
# # Shared fill scale
# fill_scale <- scale_fill_gradient2(
#   low = "red", mid = "white", high = "blue",
#   midpoint = 0,
#   limits = shared_limits,
#   breaks = breaks,
#   labels = labels,
#   name = "Absolute Ideology Index",
#   guide = guide_colorbar(
#     title.position = "right",  # moves title above the colorbar vertically
#     title.theme = element_text(size = 10, face = "bold", family = "serif", angle = 90),
#     label.theme = element_text(size = 8, family = "serif"),
#     barheight = unit(3, "in"),  # taller bar
#     barwidth = unit(0.25, "in") # narrower width
#   )
# )
# 
# # Plot with facet
# combined_map <- ggplot(map_data_combined) +
#   geom_sf(aes(fill = fill_value, geometry = geometry), color = NA) +
#   geom_sf(data = province_sf, fill = NA, color = "black", linewidth = 0.1) +
#   fill_scale +
#   facet_wrap(~period, nrow = 1) +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "serif"),
#     strip.text = element_text(size = 13, face = "bold", family = "serif"),
#     legend.position = "right",
#     legend.title = element_text(size = 9, hjust = 0.5),
#     legend.text = element_text(size = 8),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   )
# 
# # Save and print
# ggsave("ideology_comparison_facet_M5S.pdf", plot = combined_map, width = 11, height = 6, dpi = 300)
# ggsave("ideology_comparison_facet_M5S.png", plot = combined_map, width = 11, height = 6, dpi = 300)
# print(combined_map)
# 

############ MAP FINAL 2 - POL_INDEX (facet) ###########

# # Combine data first with a shared fill column
# map_data_combined <- bind_rows(
#   map_ideology_2018 %>% mutate(fill_value = abs_polarization_index, period = "(a) Historical Ideological Polarization"),
#   map_ideology_2018 %>% mutate(fill_value = abs_polarization, period = "(b) Ideological Polarization (2018)")
# )
# 
# final_polar <- ggplot(map_data_combined) +
#   geom_sf(aes(fill = fill_value, geometry = geometry), color = NA) +
#   geom_sf(data = province_sf, fill = NA, color = "black", linewidth = 0.1) +
#   scale_fill_gradient(
#     low = "white",
#     high = "purple4",
#     limits = c(0, 1),
#     breaks = c(0, 0.5, 1),
#     labels = c("0", "0.5", "1"),
#     name = "Polarization Index",
#     guide = guide_colorbar(
#       title.position = "right",
#       title.theme = element_text(size = 10, face = "bold", family = "serif", angle = 90),
#       label.theme = element_text(size = 8, family = "serif"),
#       barheight = unit(3, "in"),
#       barwidth = unit(0.25, "in")
#     )
#   ) +
#   facet_wrap(~ period, nrow = 1) +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "serif"),
#     strip.text = element_text(size = 13, face = "bold", family = "serif"),
#     legend.position = "right",
#     legend.title = element_text(size = 9, hjust = 0.5),
#     legend.text = element_text(size = 8),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   )
# 
# # Save and print
# ggsave("polarization_comparison_facet.pdf", plot = final_polar, width = 11, height = 6, dpi = 300)
# ggsave("polarization_comparison_facet.png", plot = final_polar, width = 11, height = 6, dpi = 300)
# print(final_polar)

############ MAP FINAL 2.1 - POL_INDEX (M5S) ######################

# # Combine data first with a shared fill column
# map_data_combined <- bind_rows(
#   map_baseline %>% mutate(fill_value = abs_polarization, period = "(a) M5S Progressive (2018)"),
#   map_ideology_2018 %>% mutate(fill_value = abs_polarization, period = "(b) M5S Excluded (2018)")
# )
# 
# final_polar <- ggplot(map_data_combined) +
#   geom_sf(aes(fill = fill_value, geometry = geometry), color = NA) +
#   geom_sf(data = province_sf, fill = NA, color = "black", linewidth = 0.1) +
#   scale_fill_gradient(
#     low = "white",
#     high = "purple4",
#     limits = c(0, 1),
#     breaks = c(0, 0.5, 1),
#     labels = c("0", "0.5", "1"),
#     name = "Polarization Index",
#     guide = guide_colorbar(
#       title.position = "right",
#       title.theme = element_text(size = 10, face = "bold", family = "serif", angle = 90),
#       label.theme = element_text(size = 8, family = "serif"),
#       barheight = unit(3, "in"),
#       barwidth = unit(0.25, "in")
#     )
#   ) +
#   facet_wrap(~ period, nrow = 1) +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "serif"),
#     strip.text = element_text(size = 13, face = "bold", family = "serif"),
#     legend.position = "right",
#     legend.title = element_text(size = 9, hjust = 0.5),
#     legend.text = element_text(size = 8),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   )
# 
# # Save and print
# ggsave("polarization_comparison_facet_M5S.pdf", plot = final_polar, width = 11, height = 6, dpi = 300)
# ggsave("polarization_comparison_facet_M5S.png", plot = final_polar, width = 11, height = 6, dpi = 300)
# print(final_polar)


############ MAP FINAL 3 - REL_INDEX ######################

# # Shared limits and breaks
# shared_limits <- range(
#   c(map_ideology_2018$rel_index, map_ideology_2018$net_relative_ideology),
#   na.rm = TRUE
# )
# breaks <- c(-1, 0, 1)
# labels <- c("−1", "0", "+1")
# 
# # Define the fill scale as a shared object
# fill_scale <- scale_fill_gradient2(
#   low = "red", mid = "white", high = "blue",
#   midpoint = 0,
#   limits = shared_limits,
#   breaks = breaks,
#   labels = labels,
#   name = "Relative Ideological Index",
#   guide = guide_colorbar(
#     title.position = "left",
#     barheight = unit(100, "pt"),
#     barwidth = unit(10, "pt"),
#     title.theme = element_text(angle = 90, size = 10)
#   )
# )
# 
# # Map (a): Historical Ideology
# map_hist <- ggplot(map_ideology_2018) +
#   geom_sf(aes(fill = rel_index), color = NA) +
#   geom_sf(data = province_sf, fill = NA, color = "black", linewidth = 0.1) +
#   fill_scale +
#   labs(title = "(a) Historical Relative Ideological Orientation") +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "serif"),
#     plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
#     legend.position = "right",
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   )
# 
# # Map (b): 2018 Ideology
# map_2018 <- ggplot(map_ideology_2018) +
#   geom_sf(aes(fill = net_relative_ideology), color = NA) +
#   geom_sf(data = province_sf, fill = NA, color = "black", linewidth = 0.1) +
#   fill_scale +
#   labs(title = "(b) Relative Ideological Orientation (2018)") +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "serif"),
#     plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
#     legend.position = "none",
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   )
# 
# # Combine maps with shared legend
# combined <- map_hist + map_2018 + plot_layout(guides = "collect") &
#   theme(legend.position = "right")
# 
# # Save
# ggsave("ideology_comparison_maps_rel.pdf", plot = combined, width = 11, height = 6, dpi = 300)
# print(combined)
# 

############ MAP FINAL 3.1 - REL_INDEX (facet) ######################

# # Combine both variables into one column with labels
# map_data_combined_rel <- bind_rows(
#   map_ideology_2018 %>%
#     mutate(fill_value = rel_index, period = "(a) Historical Relative Ideology"),
#   map_ideology_2018 %>%
#     mutate(fill_value = net_relative_ideology, period = "(b) Relative Ideology (2018)")
# )
# 
# # Shared limits and breaks
# shared_limits <- range(map_data_combined_rel$fill_value, na.rm = TRUE)
# breaks <- c(-0.5, 0, 0.5)
# labels <- c("−0.5", "0", "+0.5")
# 
# # Shared fill scale
# fill_scale <- scale_fill_gradient2(
#   low = "red", mid = "white", high = "blue",
#   midpoint = 0,
#   limits = shared_limits,
#   breaks = breaks,
#   labels = labels,
#   name = "Relative Ideology Index",
#   guide = guide_colorbar(
#     title.position = "right",  # moves title above the colorbar vertically
#     title.theme = element_text(size = 10, face = "bold", family = "serif", angle = 90),
#     label.theme = element_text(size = 8, family = "serif"),
#     barheight = unit(3, "in"),  # taller bar
#     barwidth = unit(0.25, "in") # narrower width
#   )
# )
# 
# # Plot with facet
# combined_map_rel <- ggplot(map_data_combined_rel) +
#   geom_sf(aes(fill = fill_value, geometry = geometry), color = NA) +
#   geom_sf(data = province_sf, fill = NA, color = "black", linewidth = 0.1) +
#   fill_scale +
#   facet_wrap(~period, nrow = 1) +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "serif"),
#     strip.text = element_text(size = 13, face = "bold", family = "serif"),
#     legend.position = "right",
#     legend.title = element_text(size = 9, hjust = 0.5),
#     legend.text = element_text(size = 8),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   )
# 
# # Save and print
# ggsave("ideology_comparison_facet_rel.pdf", plot = combined_map_rel, width = 11, height = 6, dpi = 300)
# ggsave("ideology_comparison_facet_rel.png", plot = combined_map_rel, width = 11, height = 6, dpi = 300)
# print(combined_map_rel)

############## OTHER MAPS #######################

# # 1. Absolute Ideology Map
# ggplot(map_ideology_2018) +
#   geom_sf(aes(fill = net_absolute_ideology), color = NA) +
#   scale_fill_gradient2(
#     low = "red",    # More progressive (negative)
#     mid = "white",  # Neutral
#     high = "blue",  # More conservative (positive)
#     midpoint = 0,
#     limits = c(-max(abs(map_ideology_2018$net_absolute_ideology), na.rm = TRUE),
#                max(abs(map_ideology_2018$net_absolute_ideology), na.rm = TRUE)),
#     name = "Conservative - Progressive\n(Absolute Index)"
#   ) +
#   theme_minimal() +
#   labs(
#     title = "Absolute Ideological Orientation of Municipalities Based on the 2018 Electoral Results",
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
# ggplot(map_ideology_2018) +
#   geom_sf(aes(fill = abs_index), color = NA) +
#   scale_fill_gradient2(
#     low = "red",    # More progressive (negative)
#     mid = "white",  # Neutral
#     high = "blue",  # More conservative (positive)
#     midpoint = 0,
#     limits = c(-max(abs(map_ideology_2018$abs_index), na.rm = TRUE),
#                max(abs(map_ideology_2018$abs_index), na.rm = TRUE)),
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
# # 2. Relative Ideology Map
# ggplot(map_ideology_2018) +
#   geom_sf(aes(fill = net_relative_ideology), color = NA) +
#   scale_fill_gradient2(
#     low = "red",
#     mid = "white",
#     high = "blue",
#     midpoint = 0,
#     limits = c(-max(abs(map_ideology_2018$net_relative_ideology), na.rm = TRUE),
#                max(abs(map_ideology_2018$net_relative_ideology), na.rm = TRUE)),
#     name = "Conservative - Progressive\n(Relative to Province)"
#   ) +
#   theme_minimal() +
#   labs(
#     title = "Relative Ideological Orientation of Municipalities Based on the 2018 Electoral Results",
#     subtitle = "Red = Progressive, Blue = Conservative (relative to provincial average)",
#     caption = "Map: 2019 boundaries"
#   ) +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(size = 16, face = "bold"),
#     plot.subtitle = element_text(size = 13),
#     plot.caption = element_text(size = 9)
#   )
#
# ## Nonlinear re-scaling ##
#
# map_ideology_2018 <- map_ideology_2018 %>%
#   mutate(
#     abs_index_sqrt = sqrt(abs(net_absolute_ideology)) * sign(net_absolute_ideology),
#     rel_index_sqrt = sqrt(abs(net_relative_ideology)) * sign(net_relative_ideology)
#   )
#
# # 1a. Absolute Ideology Map
# ggplot(map_ideology_2018) +
#   geom_sf(aes(fill = abs_index_sqrt), color = NA) +
#   scale_fill_gradient2(
#     low = "red",    # More progressive (negative)
#     mid = "white",  # Neutral
#     high = "blue",  # More conservative (positive)
#     midpoint = 0,
#     limits = c(-max(abs(map_ideology_2018$abs_index_sqrt), na.rm = TRUE),
#                max(abs(map_ideology_2018$abs_index_sqrt), na.rm = TRUE)),
#     name = "Conservative - Progressive\n(Absolute Index)"
#   ) +
#   theme_minimal() +
#   labs(
#     title = "Absolute Ideological Orientation of Municipalities Based on the 2018 Electoral Results",
#     subtitle = "Red = Progressive, Blue = Conservative",
#     caption = "Nonlinear re-scaling | Map: 2019 boundaries"
#   ) +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(size = 16, face = "bold"),
#     plot.subtitle = element_text(size = 13),
#     plot.caption = element_text(size = 9)
#   )
#
# # 2a. Relative Ideology Map
# ggplot(map_ideology_2018) +
#   geom_sf(aes(fill = rel_index_sqrt), color = NA) +
#   scale_fill_gradient2(
#     low = "red",
#     mid = "white",
#     high = "blue",
#     midpoint = 0,
#     limits = c(-max(abs(map_ideology_2018$rel_index_sqrt), na.rm = TRUE),
#                max(abs(map_ideology_2018$rel_index_sqrt), na.rm = TRUE)),
#     name = "Conservative - Progressive\n(Relative to Province)"
#   ) +
#   theme_minimal() +
#   labs(
#     title = "Relative Ideological Orientation of Municipalities Based on the 2018 Electoral Results",
#     subtitle = "Red = Progressive, Blue = Conservative (relative to provincial average)",
#     caption = "Nonlinear re-scaling | Map: 2019 boundaries"
#   ) +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(size = 16, face = "bold"),
#     plot.subtitle = element_text(size = 13),
#     plot.caption = element_text(size = 9)
#   )
