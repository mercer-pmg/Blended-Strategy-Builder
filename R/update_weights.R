update_weights <- function(df){
     
     dat <- df |>
          dplyr::select(sleeve, value, total_value) |>
          dplyr::mutate(weight = value/total_value*100) |>
          dplyr::pull(weight)
     
     for(i in 1:length(dat)){
          
          updateNumericInput(
               inputId = paste0("weight", i),
               value   = dat[i])
          
          js$backgroundCol(paste0("weight", i),"white")
          
     }
}