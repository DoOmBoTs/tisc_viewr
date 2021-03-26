#' plot_basin_depth
#'
#' @param models_basin_geometry output from models_basin_geometry
#'  @param vis_dir directory where you want to place your figures
#'
#' @return
#' @export
#'
#' @examples
plot_basin_depth <- function(models_basin_geometry, vis_dir){

  d <- models_basin_geometry %>%
    purrr::imap(~ .x %>% dplyr::mutate(model_name = .y)) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(model_name) %>%
    dplyr::mutate(period = dplyr::row_number())

  p <- ggplot2::ggplot(data = d, aes(x = period, y = basin_depth, color = model_name)) +
    ggplot2::geom_line(size = .5) +
    # scale_x_continuous(breaks = d$period, labels = d$timestep) +
    ggplot2::geom_hline(yintercept = -1000, linetype = "dashed", color = "red") +
    ggplot2::labs(x = "Timestep (My)", y = "Depth (m)", title = "Basin depth over time") +
    ggplot2::facet_wrap(~ class) + # changed model_name to class , testing now
    ggplot2::theme_bw()

  ggplot2::ggsave(
    filename = "basin_depth.png",
    path = vis_dir,
    plot = p, device = "png",
    width = 25, height = 20, units = "cm"
  )

}
