#' plot_basin_symmetry_data
#'
#' @param models_basin_symmetry_data output from models_basin_symmetry_data
#' @param vis_dir directory where you would like to place your plots
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
plot_basin_symmetry_data <- function(models_basin_symmetry_data, vis_dir){

  d <- models_basin_symmetry_data %>%
    purrr::imap(~ .x %>% dplyr::mutate(model = .y, t = row_number())) %>%
    dplyr::bind_rows()


  p <- ggplot2::ggplot(data = d, aes(x = t, y = ratio, color = model)) +
    ggplot2::geom_line(size = .5) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2, color = "red") +
    ggplot2::labs(x = "Timestep (My)", y = "Thrustward Volume:Hinterlandward Volume", title = "Basin Symmetry") +
    ggplot2::facet_wrap(~ class) +
    ggplot2::theme_bw()

  ggplot2::ggsave(
    filename = glue("basin_symmetry.png"),
    path = vis_dir,
    plot = p, device = "png",
    width = 25, height = 20, units = "cm"
  )

}
