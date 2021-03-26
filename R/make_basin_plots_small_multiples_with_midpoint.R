#' make_basin_plots_small_multiples_with_midpoint
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
make_basin_plots_small_multiples_with_midpoint <- function(models_basin_data, vis_dir){

  models_basin_data %>%

    iwalk(function(tidy_pfl, model_name){
      if (nrow(tidy_pfl) < 0) {
        midpoint_data <- tidy_pfl %>%
          dplyr::group_by(timestep) %>%
          dplyr::summarize(midpoint = ceiling(stats::median(Y, na.rm = TRUE)))

        p <- ggplot2::ggplot(data = tidy_pfl, mapping = aes(x = Y, y = elevation)) +
          ggplot2::geom_line() +
          ggplot2::geom_vline(data = midpoint_data, mapping = aes(xintercept = midpoint), linetype = 2, color = "red") +
          ggplot2::labs(x = "", y = "", title = paste0("Model: ", model_name)) +
          ggplot2::facet_wrap(~ timestep, scales = "free") +
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
