donut_asset_classes <- function(df){
     
     dat <- df |>
          dplyr::left_join(readr::read_csv("cma_data_rank.csv"), by = "asset_class") |>
          dplyr::arrange(rank) |>
          dplyr::mutate(
               asset_class = paste0(asset_class, " (", round(weight, 1), "%)"),
               asset_class = asset_class |> forcats::as_factor())
     
     legend_columns <- (length(dat$asset_class)/16) |> ceiling()
     
     p <- dat |>
          ggplot2::ggplot(ggplot2::aes(x = 3, y = weight, fill = asset_class)) +
          ggplot2::geom_col(color = "white", linewidth = 1) +
          ggplot2::coord_polar(theta = "y") +
          ggplot2::xlim(c(0.2, 3.5)) +
          ggplot2::scale_fill_manual(values = dat$fill) +
          ggplot2::guides(fill = ggplot2::guide_legend(ncol = legend_columns)) +
          ggplot2::theme(
               plot.margin = ggplot2::margin(t=0, r=0, b=0, l=0),
               panel.background = ggplot2::element_rect(fill = "white"),
               panel.grid       = ggplot2::element_blank(),
               axis.title       = ggplot2::element_blank(),
               axis.ticks       = ggplot2::element_blank(),
               axis.text        = ggplot2::element_blank(),
               legend.text      = ggplot2::element_text(
                    family = "IBM Plex Sans",
                    size = 12),
               legend.position  = "right",
               legend.title     = ggplot2::element_blank())
     
     return(p)
     
}