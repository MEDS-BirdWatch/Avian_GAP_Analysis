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
  model <- vector(mode = 'list', length = 0)
  
  for (i in habitats) {
    tmp <- dat %>%
      st_drop_geometry() %>%
      filter(habitat_type == i, rich_gini > 0, !is.na(rich_gini)) 
    
    if (n_distinct(tmp$protection) < 2) {
      message("Skipping: ", i)
      next
    }
    
    model[[i]] <- tryCatch(
      glmmTMB(rich_gini ~ protection + (1|study_area) + (1|year_collected),
              family = Gamma(link = "log"), data = tmp),
      error = function(e) {
        message("Trying BFGS for: ", i)
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