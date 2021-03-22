
make_basin_plots_small_multiples <- function(models_basin_data){
  
  models_basin_data %>%
    iwalk(function(tidy_pfl, model_name){
      if (nrow(tidy_pfl) < 0) {
        p <- ggplot(data = tidy_pfl, mapping = aes(x = Y, y = elevation)) +
          geom_line() +
          labs(x = "", y = "", title = paste0("Model: ", model_name)) +
          facet_wrap(~ timestep) +
          theme_bw()
        
        ggsave(
          filename = glue("{model_name}.png"), 
          path = here::here("visualizations/basin_plots/small_multiples"), 
          plot = p, device = "png",
          width = 25, height = 20, units = "cm", dpi = 200
        )
      }
    })
  
}

make_basin_plotly <- function(models_basin_data){
  
  models_basin_data %>% 
    
    iwalk(function(tidy_pfl, model_name){
      
      if (nrow(tidy_pfl) < 0) {
        
        basin <- pivot_wider(tidy_pfl, names_from = "timestep", values_from = "elevation") %>%
          select(-(Y)) %>%
          as.matrix(.)
        
        p <- plot_ly(z = ~ basin) %>% 
          add_surface(
            contours = list(
              z = list(
                show = TRUE,
                usercolormap = TRUE,
                highlightcolor = "#ff0000",
                project = list(z = TRUE)
              )
            )
          ) %>% 
          plotly::layout(
            scene = list(
              xaxis = list(title = "Timestep"),
              yaxis = list(title = "Y (km)"),
              zaxis = list(title = "Elevation (m)"),
              camera=list(
                eye = list(x=1.87, y=0.88, z=-0.64)
              )
            )
          )
        
        htmlwidgets::saveWidget(p, here::here(sprintf("visualizations/basin_plotly/%s.html", model_name)))
      }
    })
  
}

make_basin_plots_small_multiples_with_midpoint <- function(models_basin_data){
  
  models_basin_data %>%
    iwalk(function(tidy_pfl, model_name){
      if (nrow(tidy_pfl) < 0) {
        midpoint_data <- tidy_pfl %>%
          group_by(timestep) %>%
          summarize(midpoint = ceiling(median(Y, na.rm = TRUE)))
        
        p <- ggplot(data = tidy_pfl, mapping = aes(x = Y, y = elevation)) +
          geom_line() +
          geom_vline(data = midpoint_data, mapping = aes(xintercept = midpoint), linetype = 2, color = "red") +
          labs(x = "", y = "", title = paste0("Model: ", model_name)) +
          facet_wrap(~ timestep, scales = "free") +
          theme_bw()
        
        ggsave(
          filename = glue("{model_name}.png"), 
          path = here::here("visualizations/basin_plots/small_multiples_with_midpoint/"), 
          plot = p, device = "png",
          width = 25, height = 20, units = "cm", dpi = 200
        )
      }
    })
  
}


make_basin_plots_individual <- function(models_basin_data){
  
  model_names <- names(models_basin_data)
  dir_create(glue(here("visualizations/basin_plots/individual/{model_names}")))
  
  model_tbl <- imap(models_basin_data, function(tidy_pfl, model_name){
    tidy_pfl %>%
      group_nest(timestep) %>%
      mutate(
        basin_plots = map2(data, timestep, function(tidy_pfl, time){
          ggplot(data = tidy_pfl, mapping = aes(x = Y, y = elevation)) +
            geom_line() +
            labs(x = "", y = "", 
                 title = paste0("Model: ", model_name),
                 subtitle = paste0("Timestep: ", time)) +
            theme_bw()
        })
      )
  })
  
  iwalk(model_tbl, function(tbl, model_name){
    
    walk2(tbl$basin_plots, as.character(tbl$timestep), function(basin_plot, timestep){
      
      ggsave(
        filename = glue("{timestep}.png"), 
        path = here::here(glue("visualizations/basin_plots/individual/{model_name}")),
        plot = basin_plot, device = "png",
        width = 25, height = 20, units = "cm"
      )
      
      cli_alert_success(glue("Model: {model_name}\tTimestep: {timestep}"))
      
    })
    
  })
  
}


plot_basin_depth <- function(models_basin_geometry){
  
  d <- models_basin_geometry %>%
    imap(~ .x %>% mutate(model_name = .y)) %>%
    bind_rows() %>%
    group_by(model_name) %>%
    mutate(period = row_number())
  
  p <- ggplot(data = d, aes(x = period, y = basin_depth, color = model_name)) +
    geom_line(size = .5) +
    # scale_x_continuous(breaks = d$period, labels = d$timestep) +
    geom_hline(yintercept = -1000, linetype = "dashed", color = "red") +
    labs(x = "Timestep (My)", y = "Depth (m)", title = "Basin depth over time") +
    facet_wrap(~ class) + # changed model_name to class , testing now
    theme_bw() 
  
  ggsave(
    filename = "basin_depth.png", 
    path = here::here("visualizations/basin_depth"),
    plot = p, device = "png",
    width = 25, height = 20, units = "cm"
  )
  
}


plot_basin_length <- function(models_basin_geometry){
  
  d <- models_basin_geometry %>%
    imap(~ .x %>% mutate(model_name = .y)) %>%
    bind_rows() %>%
    group_by(model_name) %>%
    mutate(period = row_number())
  
  p <- ggplot(data = d, aes(x = period, y = basin_length, color = model_name)) +
    geom_line(size = .5) +
    labs(x = "Timestep (My)", y = "Width (km)", title = "Basin length over time") +
    facet_wrap(~ class) +
    theme_bw()
  
  ggsave(
    filename = "basin_length.png", 
    path = here::here("visualizations/basin_length"),
    plot = p, device = "png",
    width = 25, height = 20, units = "cm"
  )
  
}



plot_basin_symmetry_data <- function(models_basin_symmetry_data){
  
  d <- models_basin_symmetry_data %>%
    imap(~ .x %>% mutate(model = .y, t = row_number())) %>%
    bind_rows()
  
  
  p <- ggplot(data = d, aes(x = t, y = ratio, color = model)) +
    geom_line(size = .5) +
    geom_hline(yintercept = 1, linetype = 2, color = "red") +
    labs(x = "Timestep (My)", y = "Thrustward Volume:Hinterlandward Volume", title = "Basin Symmetry") +
    facet_wrap(~ class) +
    theme_bw()
  
  ggsave(
    filename = glue("basin_symmetry.png"),
    path = here::here("visualizations/basin_symmetry/"),
    plot = p, device = "png",
    width = 25, height = 20, units = "cm"
  )
  
}

plot_geohistory_curves <- function(subsidence_data){
  
  # subsidence_data[[4]] <- NULL
  
  d <- subsidence_data %>%
    imap(~ .x %>% mutate(model_name = .y)) %>%
    bind_rows() %>%
    group_by(model_name)
  
  
  p <- ggplot(data = d, aes(x = timestep)) +
    geom_line(aes(y = -sub_total, color = "Total subsidence")) +
    geom_line(aes(y = -spline_sub_tec, color = "Spline tectonic subsidence")) +
    geom_area(aes(y = -spline_water_depth, fill = "Water depth"), size = 0.2, alpha = 0.5, color = "darkblue") +
    scale_color_manual(
      values = c("Total subsidence" = "black",
                 "Spline tectonic subsidence" = "red")
    ) +
    scale_fill_manual(values = c("Water depth" = "blue")) +
    labs(x = "Time step (My)", y = "Depth (m)", title = "Basin subsidence", subtitle = "per model", fill = NULL, color = NULL) +
    facet_wrap(~ model_name) +
    theme_bw()
  
  
  ggsave(
    filename = glue("geohistory_curves.png"),
    path = here::here("visualizations/geohistory_curves/"),
    plot = p, device = "png",
    width = 25, height = 20, units = "cm"
  )
  
}
