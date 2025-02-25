# Write a downloaded strategy to file

update_s3 <- function(){
     
     s3BucketName <- "ma-pmg-blended-strategy-builder"
     
     
     existing_blended <- aws.s3::get_object(
          region = Sys.getenv("AWS_DEFAULT_REGION"),
          key    = Sys.getenv("AWS_ACCESS_KEY_ID"),
          secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
          
          object = "Blended Strategy Key.csv", 
          bucket = s3BucketName) |>
          readBin("character") |>
          readr::read_csv(show_col_types = FALSE) 
     
     new_strategy <- tibble::tibble(
          `App Name` = paste0("Blended - ", format(Sys.time(), format = "%Y%m%d%H%M%S")),
          `Orion Name` = next_blended()
     )
     
     updated_table <- dplyr::bind_rows(existing_blended, new_strategy)
     
     tmp <- tempfile()
     on.exit(unlink(tmp))
     readr::write_csv(updated_table, file = tmp)
     
     
     aws.s3::put_object(
          file = tmp,
          object = "Blended Strategy Key.csv",
          bucket = s3BucketName
     )
}

