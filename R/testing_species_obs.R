sp_obs_habitat <- function(data1, data2){
focal_sp_obs <- list()

for(i in unique(data2$group)){
 sp_list <-  data2 %>% 
   filter(habitat_type == i) %>% 
   select(common_name)
 
 sp_obs_habitat <- data1 %>% 
<<<<<<< HEAD
   filter(habitat_type == habitat_type) %>% 
=======
   st_drop_geometry() %>% 
   filter(habitat_type == i) %>% 
>>>>>>> 5dfe080fc8e3976517b0f45ee1fc58bb32d0e2e1
   filter(common_name %in% c(sp_list)) %>% 
   group_by(common_name) %>%  
   summarize(sum = sum(observation_count))

append(focal_sp_obs, sp_obs_habitat)

  }
return(focal_sp_obs)
}

