#' plot_basin_length
#'
#' @param models_basin_geometry output from models_basin_geometry
#' @param vis_dir directory where you want to place your figures
#'
#' @return
#' @export
#'
#' @examples
plot_basin_length <- function(models_basin_geometry, vis_dir){

  d <- models_basin_geometry %>%
    purrr::imap(~ .x %>% dplyr::mutate(model_name = .y)) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(model_name) %>%
    dplyr::mutate(period = dplyr::row_number())

  p <- ggplot2::ggplot(data = d, aes(x = period, y = basin_length, color = model_name)) +
    ggplot2::geom_line(size = .5) +
    ggplot2::labs(x = "Timestep (My)", y = "Width (km)", title = "Basin length over time") +
    ggplot2::facet_wrap(~ class) +
    ggplot2::theme_bw()

  ggplot2::ggsave(
    filename = "basin_length.png",
    path = vis_dir,
    plot = p, device = "png",
    width = 25, height = 20, units = "cm"
  )

}
