fit_disagg <- function(dat){
  
  habitats <- unique(dat$habitat_type)
  tmp <- vector(mode = 'list', length = 0)
  model <- vector(mode = 'list', length = 0)
  
  for (i in habitats){
    
    tmp[[i]] <- dat %>% 
      st_drop_geometry() %>% 
      filter(habitat_type == i)
    
    if (n_distinct(tmp[[i]]$gap_sts) < 2) { 
      message("Skipping: ", i)
      next
    }
    
    model[[i]] <- tryCatch(
      glmmTMB(rich_gini ~ gap_sts, family = beta_family(), data = tmp[[i]]),
      error = function(e) {
        message("Error at: ", i, " | ", e$message)
        NULL
      }
    )
    
  }
  return(model)
}