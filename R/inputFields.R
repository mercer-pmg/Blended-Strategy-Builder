inputFields <- function(id, df){
     
     column(9,
            column(6, paste0("sleeve", id)  |> purrr::map(selectInput_sleeve, df)),
            column(2, paste0("weight", id)  |> purrr::map(numericInputIcon_weight)),
            column(2, paste0("value", id)   |> purrr::map(numericInputIcon_value)),
            column(2, paste0("message", id) |> purrr::map(textOutput) |> span(style ="color:#FF96D7; font-family:IBM Plex Sans"))
     )
     
}