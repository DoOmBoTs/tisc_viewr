#' make_basin_plotly
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
make_basin_plotly <- function(models_basin_data, vis_dir = NULL){

  Ps <- models_basin_data %>%

     purrr::imap(function(tidy_pfl, model_name){

      if (nrow(tidy_pfl) > 0) {

        basin <- tidyr::pivot_wider(tidy_pfl, names_from = "timestep", values_from = "elevation") %>%
          dplyr::select(-(Y)) %>%
          as.matrix(.)

        p <- plotly::plot_ly(z = ~ basin) %>%
          plotly::add_surface(
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
              camera = list(
                eye = list(x=1.87, y=0.88, z=-0.64)
              )
            )
          )

      }

       # browser()
       file_name <- paste0(vis_dir, "/", model_name, ".html")

       if(!is.null(vis_dir)){
         htmlwidgets::saveWidget(p, file_name)
       }

      p

    })

    Ps



}
