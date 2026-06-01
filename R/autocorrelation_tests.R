#' run_spatial_temporal_tests
#'
#' @param models a list of models to be evaluated from either fit/adj_fit agg/disagg
#' @param model_data_map a list of data for the models to be compared against
#' @param habitats a list of habitats to be run over
#'
#' @returns a dataframe containing: model_name, habitat, durbin-watson score,
#' Moran's Index, Moran's Index P-Value, Standard Deviation, Beta, and Dispersion
#' 
#' @export
#'
#' @examples models <- list
#' point_disag  = point_disag,
#' area_disag   = area_disag,
#' point_agg    = point_agg,
#' area_agg     = area_agg,
#' bird_summary = bird_summary
#' 
#' spatial_temporal_tests <- list
#'
#' model_data_map <- list
#'  point_disag  = biodiv_point_count_disag,
#'  area_disag   = biodiv_area_search_disag,
#'  point_agg    = biodiv_point_count_agg,
#'  area_agg     = biodiv_area_search_agg,
#'  bird_summary = birds_rich
#' 
#' spatial_temporal_tests <- run_spatial_temporal_tests
#'  models         = models,
#'  model_data_map = model_data_map,
#'  habitats       = habitats


run_spatial_temporal_tests <- function(models, model_data_map, habitats) {
  
  spatial_temporal_tests <- list()
  
  for (model_name in names(models)) {
    for (i in habitats) {
      model <- models[[model_name]][[i]]
      
      if (is.null(model)) {
        message("Skipping: ", model_name, " / ", i)
        next
      }
      
      res     <- residuals(model, type = "pearson")
      dw_stat <- dwtest(res ~ 1)$statistic
      
      mf      <- model.frame(model)
      mf$.res <- res
      mf$.row <- as.integer(rownames(mf))
      
      
      dat <- model_data_map[[model_name]] %>%
        filter(habitat_type == i) %>%
        mutate(.row = row_number()) %>%
        inner_join(mf %>% select(.row, .res), by = ".row") %>%
        rename(res = .res)
      
      if (is.null(dat)) {
        message("No data found in model_data_map for: ", model_name, " — check names match")
        next
      }
      
      beta <- tryCatch(fixef(model)$cond[2], 
                       error = function(e) {
        tryCatch(coef(model)[2], 
                 error = function(e2) NA_real_)
      })
      
      disp <- tryCatch(sigma(model), 
                       error = function(e) {
        tryCatch(summary(model)$dispersion, 
                 error = function(e2) NA_real_)
      })
      
      sd <- tryCatch(summary(model)$coefficients$cond[2, 2], 
                     error = function(e) {
        tryCatch(summary(model)$coefficients[2, 2], 
                 error = function(e2) NA_real_)
      })
      
      dat_site <- dat %>%
        group_by(study_area) %>%
        summarize(
          res      = mean(res, na.rm = TRUE),
          geometry = first(geometry),
          .groups  = "drop"
        ) %>%
        st_as_sf()
      
      moran_I <- NA_real_
      moran_p <- NA_real_
      if (nrow(dat_site) > 4) {
        coords  <- st_coordinates(dat_site)
        nb      <- knn2nb(knearneigh(coords, k = 4))
        listw   <- nb2listw(nb, style = "W")
        moran   <- moran.test(dat_site$res, listw)
        moran_I <- moran$estimate["Moran I statistic"]
        moran_p <- moran$p.value
      }
      
      spatial_temporal_tests <- bind_rows(spatial_temporal_tests, tibble(
        model_name = model_name,
        habitat    = i,
        dw         = as.numeric(dw_stat),
        moran_I    = as.numeric(moran_I),
        moran_p    = as.numeric(moran_p),
        sd = sd,
        beta = beta,
        dispersion = disp
      ))
    }
  }
  
  return(spatial_temporal_tests)
}