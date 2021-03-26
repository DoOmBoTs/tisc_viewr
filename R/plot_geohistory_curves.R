#' plot_geohistory_curves
#'
#' @param subsidence_data output from get_subsidence_data
#' @param vis_dir directory where you would like to place your plots
#'
#' @return
#' @export
#'
#' @examples
plot_geohistory_curves <- function(subsidence_data, vis_dir){

  # subsidence_data[[4]] <- NULL

  d <- subsidence_data %>%
    purrr::imap(~ .x %>% dplyr::mutate(model_name = .y)) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(model_name)


  p <- ggplot2::ggplot(data = d, aes(x = timestep)) +
    ggplot2::geom_line(aes(y = -sub_total, color = "Total subsidence")) +
    ggplot2::geom_line(aes(y = -spline_sub_tec, color = "Spline tectonic subsidence")) +
    ggplot2::geom_area(aes(y = -spline_water_depth, fill = "Water depth"), size = 0.2, alpha = 0.5, color = "darkblue") +
    ggplot2::scale_color_manual(
      values = c("Total subsidence" = "black",
                 "Spline tectonic subsidence" = "red")
    ) +
    ggplot2::scale_fill_manual(values = c("Water depth" = "blue")) +
    ggplot2::labs(x = "Time step (My)", y = "Depth (m)", title = "Basin subsidence", subtitle = "per model", fill = NULL, color = NULL) +
    ggplot2::facet_wrap(~ model_name) +
    ggplot2::theme_bw()


  ggplot2::ggsave(
    filename = glue("geohistory_curves.png"),
    path = vis_dir,
    plot = p, device = "png",
    width = 25, height = 20, units = "cm"
  )

}
