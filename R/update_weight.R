update_weight <- function(i, df){
     
     value <- df |> dplyr::pull(value) |> dplyr::nth(i)
     total <- df |> dplyr::pull(total_value) |> dplyr::nth(i)
     
     new_weight <- value/total |> round(4)
     
     updateNumericInput(
          inputId = paste0("weight", i),
          value   = new_weight
     )
}