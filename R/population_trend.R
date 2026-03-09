
population_trends <- function(data, species_list, years_list){
  
  data %>% 
    st_drop_geometry() %>% 
    filter(common_name %in% species_list,
           year_collected %in% years_list) %>% 
    group_by(common_name, year_collected) %>% 
    mutate(total_obs = sum(observation_count, na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(common_name, year_collected, gap_sts, habitat_type, total_obs)
}