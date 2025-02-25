sync_inputs <- function(i, df){
     
     req(df[i, "weight"] > 0)
     # req(df[i, "value"]  > 0)
     
     if(df[i, "weight"] != df[i, "imp_weight"]){
          
          print("weights mismatch")
          
          updateNumericInput(
               inputId = paste("weight", i),
               value   = df[i, "imp_weight"]
          )
     } 
     
     if(df[i, "value"] != df[i, "imp_value"]){
          
          print("values mismatch")
          
          updateNumericInput(
               inputId = paste0("value", i),
               value  = df[i, "imp_value"]
          )
     } 
     
}