
# Function reads the vector and prepares dataframe 
population_trends <- function(data, species_list) {
  data %>%
    inner_join(species_list, by = c("common_name", "habitat_type")) %>%
    group_by(common_name, year_collected, protection_sts, gap_sts, study_area, survey_type, sample_effort, geometry) %>%
    summarise(total_obs = sum(observation_count, na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Center years for models 
    mutate(year_scaled = year_collected - min(year_collected),
           protection_sts = factor(protection_sts, 
                                   levels = c("protected", "unprotected"))) %>% 
    dplyr::select(common_name, year_collected, year_scaled, total_obs, protection_sts,gap_sts,study_area, survey_type, sample_effort, geometry)
}
