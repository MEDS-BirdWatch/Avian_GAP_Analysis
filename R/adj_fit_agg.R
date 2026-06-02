#' adj_fit_agg
#'
#' @param dat a data frame containing habitat_type, rich_gini, protection, 
#' study_area, and year_collected variables
#'
#' @returns
#' A model with a gamma distribution as in the glmm_tmb package
#' @export
#'
#' @examples
adj_fit_agg <- function(dat) {
  
  habitats <- unique(dat$habitat_type)
  
  # Set up blank model
  model <- vector(mode = 'list', length = 0)
  
  
  # Iterate over habitats
  for (i in habitats) {
    
    # Make temp df with no geometry and grab habitat types
    tmp <- dat %>%
      st_drop_geometry() %>%
      filter(habitat_type == i, rich_gini > 0, !is.na(rich_gini))
    
    
    # If there are less than two protection statuses we skip
    if (n_distinct(tmp$protection_sts) < 2) {
      message("Skipping: ", i)
      next
    }
    
    # Run gamma glmm with study area and year random effects
    model[[i]] <- tryCatch( 
      glmmTMB(rich_gini ~ protection_sts + (1|study_area) + (1|year_collected),
              family = Gamma(link = "log"), data = tmp),
      error = function(e) {
        message("Trying BFGS for: ", i)
        
        # This try catch runs if the optimizer doesnt work. This does not happen with this data but at a smaller scale may 
        tryCatch(
          glmmTMB(rich_gini ~ protection  + (1|study_area) + (1|year_collected),
                  family = Gamma(link = "log"), data = tmp,
                  control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS"))),
          error = function(e2) {
            message("Falling back for: ", i, " | ", e2$message)
            glmmTMB(rich_gini ~ protection, family = Gamma(link = "log"), data = tmp)
          }
        )
      }
    )
  }
  return(model)
}