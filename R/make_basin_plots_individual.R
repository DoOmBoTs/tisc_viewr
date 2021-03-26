#' make_basin_plots_individual
#'
#' @param models_basin_data output from the models_basin_data function
#' @param vis_dir the directory path to where you would like to save your plots, end the path with an /
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
make_basin_plots_individual <- function(models_basin_data, vis_dir){

  model_names <- names(models_basin_data)

  purrr::map(model_names, function(x){
    fs::dir_create(glue::glue(vis_dir, x))
  })

  model_tbl <- purrr::imap(models_basin_data, function(tidy_pfl, model_name){
    tidy_pfl %>%
      dplyr::group_nest(timestep) %>%
      dplyr::mutate(
        basin_plots = purrr::map2(data, timestep, function(tidy_pfl, time){
          ggplot2::ggplot(data = tidy_pfl, mapping = ggplot2::aes(x = Y, y = elevation)) +
            ggplot2::geom_line() +
            ggplot2::labs(x = "", y = "",
                 title = paste0("Model: ", model_name),
                 subtitle = paste0("Timestep: ", time)) +
            ggplot2::theme_bw()
        })
      )
  })

  purrr::iwalk(model_tbl, function(tbl, model_name){

    purrr::walk2(tbl$basin_plots, as.character(tbl$timestep), function(basin_plot, timestep){

      ggplot2::ggsave(
        filename = glue::glue("{timestep}.png"),
        path = here::here(glue::glue("visualizations/basin_plots/individual/{model_name}")),
        plot = basin_plot, device = "png",
        width = 25, height = 20, units = "cm"
      )

      cli_alert_success(glue::glue("Model: {model_name}\tTimestep: {timestep}"))

    })

  })

}
