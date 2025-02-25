expected_risk <- function(allocation){
     
     cmas <- readr::read_csv("cma_data.csv", show_col_types = FALSE) |>
          tidyr::drop_na(Volatility) 
     
     cma_expected_sd <- cmas |> 
          dplyr::pull(Volatility) |> 
          stringr::str_remove_all("%") |> 
          as.numeric() 
     
     cma_expected_sd <- cma_expected_sd/100
     
     cma_cor_mat <- cmas |> 
          dplyr::select(-Asset, 
                        -`Index / Proxy`, 
                        -`Compound Return`, 
                        -`Arithmetic Return`, 
                        -Volatility)
     
     asset_classes <- names(cma_cor_mat)
     
     cov.mat <- diag(cma_expected_sd) %*% as.matrix(cma_cor_mat) %*% diag(cma_expected_sd)
     cov.mat <- cov.mat |> as.data.frame()
     names(cov.mat)     <- asset_classes
     row.names(cov.mat) <- asset_classes
     
     cov.mat <- cov.mat[allocation$Asset, allocation$Asset]
     cov.mat <- as.matrix(cov.mat)
     
     expected_risk <- (t(allocation$Weight) %*% cov.mat %*% allocation$Weight)
     expected_risk <- sqrt(expected_risk)[1,1]
     
     expected_risk <- expected_risk/100
     
     expected_risk <- expected_risk |> scales::percent(accuracy = 0.01)
     
     
     return(expected_risk)
     
     
}