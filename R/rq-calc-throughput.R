#' Calculate airport throughput
#'
#' @param .aapdf 
#' @param unit 
#'
#' @return tibble of (default) hourly throughputs per airport
#' @export
#'
#' @examples
#' \donotrun{
#'   calc_throughput(my_airport_data)
#' }
calc_throughput <- function(.apdf, unit = "hour"){
  # check input
  min_vars <- c("ADEP", "ADES", "PHASE", "MVT_TIME") 
  check_variables(.apdf, min_vars)
  
  thrus <- .apdf |> 
    dplyr::mutate(
      ICAO = dplyr::case_when(
        PHASE == "ARR" ~ ADES
        , PHASE == "DEP" ~ ADEP
        , .default = as.character(NA))
      , BIN = lubridate::floor_date(MVT_TIME, unit = "hour")
    ) |> 
    dplyr::summarise(
      ARRS = sum(PHASE == "ARR", na.rm = TRUE)
      , DEPS = sum(PHASE == "DEP", na.rm = TRUE)
      , .by = c("ICAO","BIN")
    ) |> 
    dplyr::mutate(FLTS = ARRS + DEPS)
  
  return(thrus)
}