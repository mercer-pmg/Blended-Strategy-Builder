expected_return <- function(allocation){
     
     cma_return_risk <- readr::read_csv("cma_data.csv", show_col_types = FALSE) |>
          dplyr::select(Asset, `Arithmetic Return`) |>
          dplyr::mutate(
               `Arithmetic Return` = `Arithmetic Return` |> 
                    stringr::str_remove("%") |> 
                    as.numeric())
     

     cma_data <- dplyr::select(cma_return_risk, Asset, `Arithmetic Return`)
     
     allocation <- allocation |> dplyr::left_join(cma_data, by = "Asset")

     portfolio_expected_return <- allocation |>
          dplyr::mutate(return_contribution = Weight/100*`Arithmetic Return`/100) |>
          dplyr::summarise(expected_return = sum(return_contribution)) |>
          dplyr::pull(expected_return) |>
          scales::percent(accuracy = 0.01)
     
     return(portfolio_expected_return)
     
}