nm_flt_to_pbwg_regional_traffic <- function(.nm_flt){
  df <- .nm_flt |> 
    dplyr::mutate(
      DATE = lubridate::date(LOBT)
    ) |> 
    dplyr::group_by(DATE) |> 
    dplyr::reframe(FLTS = dplyr::n())
    
  return(df)
}


save_pbwg <- function(
      .df
    , .year   
    , .pth  = "./data/"
    , .reg  = "EUR"){
  
  if(!is.null(.reg)){ out_prefix <- .reg}else{ out_prefix = ""}
  out_name <- paste0(.pth, out_prefix, "-network-traffic-", .year, ".csv")
  
  out_df <- .df |> 
    dplyr::mutate(REG = .reg)
  
  out_df |> readr::write_csv(file = out_name)
}