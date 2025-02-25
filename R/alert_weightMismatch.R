alert_weightMismatch <- function(i, df){
     
     req(sum(df$value) > 0)
     
     x_js <- paste0("weight", i)
     x    <- df |> dplyr::pull(weight)
     y    <- df |> dplyr::pull(imp_weight) |> round(2)
     

     
     if(length(x) > 0 & length(y) > 0) {
          
          if(!isTRUE(all.equal(x[i], y[i]))){
               
               js$backgroundCol(x_js,"#E9E9FF")} else {
                    
                    js$backgroundCol(x_js,"white")
               }
     }
}