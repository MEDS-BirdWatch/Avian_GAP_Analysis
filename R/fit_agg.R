#' fit_agg
#'
#' @param dat a data frame containing habitat_type, rich_gini, protection, 
#' study_area, and year_collected variables
#'
#' @returns
#' A model with a gamma distribution as in the glmm_tmb package
#' @export
#'
#' @examples
fit_agg <- function(dat) {
  
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
    
    # Run gamma with log link
    model[[i]] <- tryCatch(
      glm(rich_gini ~ protection_sts + area, data = tmp, family = Gamma(link = "log")),
      error = function(e) {
        message("Error at: ", i, " | ", e$message)
        NULL
      }
    )
  }
  model
}