gini_simpson <- function(data) {
  species_props <- data %>%
    st_drop_geometry() %>%
    group_by(gap_sts, habitat_type, observation_date, scientific_name) %>%
    summarise(species_obs = sum(observation_count, na.rm = TRUE), .groups = "drop") %>%
    filter(species_obs > 0) %>%                                          # drop here
    group_by(gap_sts, habitat_type, observation_date) %>%
    mutate(proportion = species_obs / sum(species_obs)) %>%
    summarise(
      richness  = n_distinct(scientific_name),
      gini      = 1 - sum(proportion^2),
      rich_gini = richness * gini,
      .groups   = "drop"
    )
  
  left_join(data, species_props, by = c("gap_sts", "habitat_type", "observation_date"))
}