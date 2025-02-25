next_blended <- function(){
     
     s3BucketName <- "ma-pmg-blended-strategy-builder"
     
     existing_blended <- aws.s3::get_object(
          region = Sys.getenv("AWS_DEFAULT_REGION"),
          key    = Sys.getenv("AWS_ACCESS_KEY_ID"),
          secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
          
          object = "Blended Strategy Key.csv", 
          bucket = s3BucketName) |>
          readBin("character") |>
          readr::read_csv(show_col_types = FALSE) |>
          
          tidyr::separate_wider_delim(
               cols  = `Orion Name`, 
               delim = " - ", 
               names = c("trash", "number")) |>
          
          dplyr::pull(number) |>
          
          as.numeric() |>
          
          max()
     
     next_blended <- existing_blended + 1
     
     next_blended <- next_blended |>
          stringr::str_pad(
          width = 5, side  = "left", pad   = "0")
     
     next_blended <- paste0("Blended - ", next_blended)
     
     
     return(next_blended)
}

