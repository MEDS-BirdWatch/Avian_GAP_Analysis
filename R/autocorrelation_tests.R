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
  # Create blank slate
  spatial_temporal_tests <- list()
  
  # Iterate through the models
  for (model_name in names(models)) {
    
    # And the habitat
    for (i in habitats) {
      # Grab the model from our model list
      model <- models[[model_name]][[i]]
      
      # If there is no data there skip
      if (is.null(model)) {
        message("Skipping: ", model_name, " / ", i)
        next
      }
      
      # Grab residuals
      res     <- residuals(model, type = "pearson")
      
      # Run durbin watson test-----
      dw_stat <- dwtest(res ~ 1)$statistic
      
      # Grab the frame
      mf      <- model.frame(model)
      
      # overide the residuals
      mf$.res <- res
      
      # and the row names
      mf$.row <- as.integer(rownames(mf))
      
      pval   <- summary(model)$coefficients$cond
      
      # Grab model stats -----
      
      # set data frame and rename columns
      dat <- model_data_map[[model_name]] %>%
        filter(habitat_type == i) %>%
        mutate(.row = row_number()) %>%
        inner_join(mf %>% select(.row, .res), by = ".row") %>%
        rename(res = .res)
      
      # If that is empty aka no data at that habitat skip it
      if (is.null(dat)) {
        message("No data found in model_data_map for: ", model_name, " — check names match")
        next
      }
      
      # Grab beta (if there is one)
      beta <- tryCatch(fixef(model)$cond[2], 
                       error = function(e) {
        tryCatch(coef(model)[2], 
                 error = function(e2) NA_real_)
      })
      
      # Grab dispersion (if there is one)
      disp <- tryCatch(sigma(model), 
                       error = function(e) {
        tryCatch(summary(model)$dispersion, 
                 error = function(e2) NA_real_)
      })
      
      # Grab the standard deviation (if there is one)
      sd <- tryCatch(summary(model)$coefficients$cond[2, 2], 
                     error = function(e) {
        tryCatch(summary(model)$coefficients[2, 2], 
                 error = function(e2) NA_real_)
      })
      
      # Morans I -----
      dat_site <- dat %>%
        group_by(study_area) %>%
        summarize(
          res      = mean(res, na.rm = TRUE),
          geometry = first(geometry),
          .groups  = "drop"
        ) %>%
        st_as_sf()
      
      # Make sure these are null 
      moran_I <- NA_real_
      moran_p <- NA_real_
      
      # If there are more than 4 rows we run this (makes sure we can run knn without crashing)
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
        pvalue = pval,
        dispersion = disp
      ))
    }
  }
  
  return(spatial_temporal_tests)
}