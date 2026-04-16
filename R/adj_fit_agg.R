adj_fit_agg <- function(dat, sample_size){
  habitats <- unique(dat$habitat_type)
  tmp <- vector(mode = 'list', length = 0)
  model <- vector(mode = 'list', length = 0)
  
  for (i in habitats){
    
    tmp[[i]] <- dat %>% 
      st_drop_geometry() %>% 
      filter(habitat_type == i)
    
    if (n_distinct(tmp[[i]]$protection) < 2) { 
      message("Skipping: ", sample_size, ' ', i)
      next
    }
    
    model[[i]] <- tryCatch(
      glmmTMB(rich_gini ~ protection + (1|study_area) + (1|year_collected),
              family = beta_family(), data = tmp[[i]]),
      error = function(e) {
        message("Trying BFGS for: ", sample_size, " ", i)
        tryCatch(
          glmmTMB(rich_gini ~ protection + (1|study_area) + (1|year_collected),
                  family = beta_family(), data = tmp[[i]],
                  control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS"))),
          error = function(e2) {
            message("Falling back for: ", sample_size, " ", i, " | ", e2$message)
            glmmTMB(rich_gini ~ protection, family = beta_family(), data = tmp[[i]])
          }
        )
      }
    )
  }
  return(model)
}