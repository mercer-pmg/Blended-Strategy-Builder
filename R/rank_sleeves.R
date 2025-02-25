rank_sleeves <- function(df){
     
     df <- df |>dplyr::mutate(
          
          rank = 500,
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Global"),
               true = 110,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "All Cap"),
               true = 110,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Large Cap"),
               true = 120,
               false = rank),  
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Mid Cap"),
               true = 130,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Small Cap"),
               true = 140,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Int'l Developed"),
               true = 150,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Intl ADR"),
               true = 150,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "International ADR"),
               true = 150,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Emerging Markets"),
               true = 160,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Emerg Mrkts"),
               true = 160,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Fixed Income"),
               true = 200,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Year"),
               true = 250,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Options"),
               true = 300,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Interval Funds"),
               true = 300,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Preferred"),
               true = 300,
               false = rank), 
          
          rank = dplyr::if_else(
               condition = stringr::str_detect(model_agg, "Cash Mgmt"),
               true = 400,
               false = rank)
     )
     
     return(df)
}



     