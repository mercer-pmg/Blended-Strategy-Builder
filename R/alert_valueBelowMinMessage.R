alert_valueBelowMinMessage <- function(id, df){
     
     
     if(nrow(df) >= id){
          if(df$value[[id]] > 0){
               if(!is.na(df$minimum[[id]])){
                    if(df$value[[id]] < df$minimum[[id]]){
                         paste(df$minimum[[id]] |> scales::dollar(), "min")
                    } else {
                         
                    }
               }
          }
     }
}