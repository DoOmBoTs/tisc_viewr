#' make_basin_plots_small_multiples
#'
#' @param models_basin_data output from wrangle_pfl
#' @param vis_dir the directory path to where you would like to save your plots, end the path with an /
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
make_basin_plots_small_multiples <- function(models_basin_data, vis_dir){

  models_basin_data %>%
    purrr::iwalk(function(tidy_pfl, model_name){
      if (nrow(tidy_pfl) < 0) {
        p <- ggplot2::ggplot(data = tidy_pfl, mapping = ggplot2::aes(x = Y, y = elevation)) +
          ggplot2::geom_line() +
          ggplot2::labs(x = "", y = "", title = paste0("Model: ", model_name)) +
          ggplot2::facet_wrap(~ timestep) +
          ggplot2::theme_bw()

        ggplot2::ggsave(
          filename = glue::glue("{model_name}.png"),
          path = vis_dir,
          plot = p, device = "png",
          width = 25, height = 20, units = "cm", dpi = 200
        )
      }
    })

}
