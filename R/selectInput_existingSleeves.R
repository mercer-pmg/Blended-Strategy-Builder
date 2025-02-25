selectInput_existingSleeves <- function(df = platform){
     
     excluded_strategies <- c(
          "CUSTOM MA Multifactor US Large Cap (SMA)",
          "MA 1 Year CD Ladder (SMA)",
          "MA 1 Year Treasury Ladder (SMA)",
          "MA 5 Year CD Ladder (SMA)",
          "MA 5 Year Treasury Ladder (SMA)")
     
     included_strategies <- df |>
          dplyr::select(strategy, model, portfolio) |>
          dplyr::mutate(
               model = dplyr::if_else(
                    condition = is.na(model),
                    true = strategy,
                    false = model)) |>
          dplyr::arrange(model, dplyr::desc(portfolio)) |>
          dplyr::distinct() |>
          dplyr::filter(!strategy %in% excluded_strategies) |>
          dplyr::pull(strategy)
     
     
  span(shiny::selectInput(
    inputId = "existingStrategy",
    label   = "Start from an existing strategy",
    choices = c("", included_strategies),
    width   = "100%"),
    style   = "font-family:IBM Plex Sans")
}