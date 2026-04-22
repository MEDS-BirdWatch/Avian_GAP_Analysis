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
        nb      <- knn2nb(knearneigh(coords, k = 2))
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
        moran_p    = as.numeric(moran_p)
      ))
    }
  }
  
  return(spatial_temporal_tests)
}