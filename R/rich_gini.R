
  gini_simpson <- function(data) {
    
      species_props <- data %>%
        st_drop_geometry() %>%
        group_by(gap_sts, habitat_type) %>%
        mutate(total_obs = sum(observation_count, na.rm = TRUE)) %>%
        group_by(gap_sts, habitat_type, scientific_name) %>%  
        summarise(
          species_obs = sum(observation_count, na.rm = TRUE),
          total_obs = first(total_obs),
          proportion = species_obs / total_obs,
          area = as.numeric(first(area))/ 1000000,
          .groups = 'drop'
        )
      
      index <- species_props %>%
        group_by(gap_sts, habitat_type) %>%
        summarise(
          richness = n_distinct(scientific_name) / first(area),
          gini = 1 - sum(proportion ^ 2),
          rich_gini = richness * (1 - sum(proportion ^ 2)),
          .groups = 'drop'
        )
      
      left_join(data, index)
    }


