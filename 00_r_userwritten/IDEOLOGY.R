IDEOLOGY <- function(data) {
  library(dplyr)
  library(tidyr)
  
  data_clean <- data %>%
    filter(!is.na(VOTI_LISTA), !is.na(VOTANTI), VOTANTI > 0, !is.na(IDEOLOGY)) %>%
    mutate(COMUNE = toupper(COMUNE), PROVINCIA = toupper(PROVINCIA))
  
  vote_by_comune <- data_clean %>%
    group_by(COMUNE, IDEOLOGY) %>%
    summarise(votes = sum(VOTI_LISTA, na.rm = TRUE), .groups = "drop")
  
  total_votes_by_comune <- vote_by_comune %>%
    group_by(COMUNE) %>%
    summarise(total = sum(votes), .groups = "drop")
  
  vote_shares <- vote_by_comune %>%
    left_join(total_votes_by_comune, by = "COMUNE") %>%
    mutate(share = votes / total) %>%
    select(COMUNE, IDEOLOGY, share)
  
  absolute <- vote_shares %>%
    pivot_wider(names_from = IDEOLOGY, values_from = share, values_fill = 0) %>%
    rename(abs_progressive = Progressive, abs_conservative = Conservative, abs_other = Other) %>%
    mutate(net_absolute_ideology = abs_conservative - abs_progressive)
  
  province_mean <- data_clean %>%
    group_by(PROVINCIA, IDEOLOGY) %>%
    summarise(votes = sum(VOTI_LISTA, na.rm = TRUE), .groups = "drop") %>%
    group_by(PROVINCIA) %>%
    mutate(total = sum(votes), prov_share = votes / total) %>%
    select(PROVINCIA, IDEOLOGY, prov_share) %>%
    pivot_wider(names_from = IDEOLOGY, values_from = prov_share, values_fill = 0) %>%
    rename(prov_progressive = Progressive, prov_conservative = Conservative, prov_other = Other) %>%
    mutate(prov_net_ideology = prov_conservative - prov_progressive)
  
  final <- absolute %>%
    left_join(data_clean %>% select(COMUNE, PROVINCIA) %>% distinct(), by = "COMUNE") %>%
    left_join(province_mean, by = "PROVINCIA") %>%
    mutate(
      net_relative_ideology = net_absolute_ideology - prov_net_ideology,
      abs_polarization = abs(abs_conservative - abs_progressive),
      prov_abs_polarization = abs(prov_conservative - prov_progressive),
      rel_polarization = abs(abs_polarization - prov_abs_polarization)
    ) %>%
    select(
      COMUNE,
      abs_progressive, abs_conservative, abs_other,
      net_absolute_ideology, net_relative_ideology,
      abs_polarization, rel_polarization
    )
  
  return(final)
}
