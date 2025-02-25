selectInput_sleeve <- function(id, df){
     
     excluded_strategies <- c(
          "CUSTOM - MA Multifactor US Large Cap (SMA)",
          "MA 1 Year CD Ladder (SMA)",
          "MA 1 Year Treasury Ladder (SMA)",
          "MA 5 Year CD Ladder (SMA)",
          "MA 5 Year Treasury Ladder (SMA)")
     
     sleeves <- df |>
          dplyr::arrange(model_agg) |>
          dplyr::filter(!model_agg %in% excluded_strategies) |>
          dplyr::pull(model_agg) |>
          unique()
     
     span(shiny::selectInput(
          inputId = id, 
          label = NULL, 
          choices = c("Choose Sleeve" = "", sleeves),
          selectize = TRUE,
          width = "100%"),
          style = "font-family:IBM Plex Sans")
     
}