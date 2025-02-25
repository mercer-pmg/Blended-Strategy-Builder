update_value <- function(id, weight, total){

     new_value <- weight*total/100
     
     updateNumericInput(
          inputId = id,
          value   = new_value
     )
}