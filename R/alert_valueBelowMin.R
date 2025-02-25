alert_valueBelowMin <- function(id, df){
     
     
     
     x_js <- paste0("value", id)
     # x    <- df |> dplyr::pull(value)
     # min  <- df |> dplyr::pull(minimum)
     # if(length(x) > 0 & length(min) >0) {
     #      if(x[i] < min[i]){
     #           js$backgroundCol(x_js,"#FFD8EF")} else {
     #                js$backgroundCol(x_js,"white")
     #           }
     # }
     
     js$backgroundCol(x_js,"white")
     
     
     if(nrow(df) >= id){
          if(df$value[[id]] > 0){
               if(!is.na(df$minimum[[id]])){
                    if(df$value[[id]] < df$minimum[[id]]){
                         js$backgroundCol(x_js,"#FFD8EF")
                    } 
               }
          }
     } 
}