fit_disagg <- function(dat) {
  habitats <- unique(dat$habitat_type)
  model <- vector(mode = 'list', length = 0)
  
  for (i in habitats) {
    tmp <- dat %>%
      st_drop_geometry() %>%
      filter(habitat_type == i) %>% 
      filter(habitat_type == i, rich_gini > 0, !is.na(rich_gini))
    
    if (n_distinct(tmp$protection) < 2) {
      message("Skipping: ", i)
      next
    }
    
    model[[i]] <- tryCatch(
      glm(rich_gini ~ gap_sts + area, data = tmp, family = Gamma(link = "log")),
      error = function(e) {
        message("Error at: ", i, " | ", e$message)
        NULL
      }
    )
  }
  model
}