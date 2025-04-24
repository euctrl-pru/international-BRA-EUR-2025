extract_asma_dsma <- function(.apdf){
  trimmed_df <- .apdf |>  prep_apdf() |>    # standard clearning 
    dplyr::select(FLTID, ADEP, ADES, CLASS, RWY, PHASE
                  , MVT_TIME
                  , C40_CROSS_TIME, C40_BEARING
                  , C100_CROSS_TIME, C100_BEARING)
  return(trimmed_df)
}

prepare_eur_asma_dsma <- function(.apdf, .max_travel = 180){
   tmp <- .apdf |> 
     dplyr::mutate(
       C40_TRAVEL = dplyr::case_when(
          PHASE == "DEP" ~ difftime(C40_CROSS_TIME, MVT_TIME, unit = "min") |> as.numeric()
         ,PHASE == "ARR" ~ difftime(MVT_TIME, C40_CROSS_TIME, unit = "min") |> as.numeric()   
           )
      ,C100_TRAVEL = dplyr::case_when(
         PHASE == "DEP" ~ difftime(C100_CROSS_TIME, MVT_TIME, unit = "min") |> as.numeric()
        ,PHASE == "ARR" ~ difftime(MVT_TIME, C100_CROSS_TIME, unit = "min") |> as.numeric()
        )
     ) |> 
    # clean and drop edge cases -------------------------
    dplyr::mutate(
       C40_TRAVEL  = ifelse(C40_TRAVEL  < 0 | C40_TRAVEL  > .max_travel, NA, C40_TRAVEL)
      ,C100_TRAVEL = ifelse(C100_TRAVEL < 0 | C100_TRAVEL > .max_travel, NA, C100_TRAVEL)
    )
  return(tmp)
}

viz_check_asma_sectors <- function(.asmas, .sectors){
  
}  

calc_pct20_cases <- function(.asmas, .lb, .ub){
  tmp <- .asmas |>
    filter(BLOCK_TIME_UTC >= .lb & BLOCK_TIME_UTC <= .ub) |>
    group_by(PHASE, RWY, STND) |>
    summarise(N = n(), REF_20 = quantile(TXXT, p = .2), .groups = "drop")
  return(tmp)
}
