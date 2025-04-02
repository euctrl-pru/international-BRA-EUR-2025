#' Prepare Punctuality Data Set
#' 
#' Based on an airport level APDF data set, determine the delay observed at
#' block, i.e., AOBT - SOBT or AIBT - SIBT ~ BLOCK_D(E)L(A)Y
#'
#' @param .apdf 
#'
#' @return
#' @export
#'
#' @examples
add_delay_and_dlygrp <- function(.apdf){
  tmp <- .apdf |> 
    dplyr::mutate(
      BLOCK_DLY = difftime(BLOCK_TIME, SCHED_TIME, units = "mins") |> as.numeric()
      , DLY_GRP = dplyr::case_when(
        -Inf < BLOCK_DLY & BLOCK_DLY <= -60 ~ "(-INF,-60]"
        ,- 60 < BLOCK_DLY & BLOCK_DLY <= -55 ~ "(-60,-55]"
        ,- 55 < BLOCK_DLY & BLOCK_DLY <= -50 ~ "(-55,-50]"
        ,- 50 < BLOCK_DLY & BLOCK_DLY <= -45 ~ "(-50,-45]"
        ,- 45 < BLOCK_DLY & BLOCK_DLY <= -40 ~ "(-45,-40]"
        ,- 40 < BLOCK_DLY & BLOCK_DLY <= -35 ~ "(-40,-35]"
        ,- 35 < BLOCK_DLY & BLOCK_DLY <= -30 ~ "(-35,-30]"
        ,- 30 < BLOCK_DLY & BLOCK_DLY <= -25 ~ "(-30,-25]"
        ,- 25 < BLOCK_DLY & BLOCK_DLY <= -20 ~ "(-25,-20]"
        ,- 20 < BLOCK_DLY & BLOCK_DLY <= -15 ~ "(-20,-15]"
        ,- 15 < BLOCK_DLY & BLOCK_DLY <= -10 ~ "(-15,-10]"
        ,- 10 < BLOCK_DLY & BLOCK_DLY <= - 5 ~ "(-10,-5]"
        ,-  5 < BLOCK_DLY & BLOCK_DLY <=   0 ~ "(-5,0]"
        ,   0 < BLOCK_DLY & BLOCK_DLY <=   5 ~ "(0,5)"
        ,  5 <= BLOCK_DLY & BLOCK_DLY <   10 ~ "[5,10)"
        , 10 <= BLOCK_DLY & BLOCK_DLY <   15 ~ "[10,15)"
        , 15 <= BLOCK_DLY & BLOCK_DLY <   20 ~ "[15,20)"
        , 20 <= BLOCK_DLY & BLOCK_DLY <   25 ~ "[20,25)"
        , 25 <= BLOCK_DLY & BLOCK_DLY <   30 ~ "[25,30)"
        , 30 <= BLOCK_DLY & BLOCK_DLY <   35 ~ "[30,35)"
        , 35 <= BLOCK_DLY & BLOCK_DLY <   40 ~ "[35,40)"
        , 40 <= BLOCK_DLY & BLOCK_DLY <   45 ~ "[40,45)"
        , 45 <= BLOCK_DLY & BLOCK_DLY <   50 ~ "[45,50)"
        , 50 <= BLOCK_DLY & BLOCK_DLY <   55 ~ "[50,55)"
        , 55 <= BLOCK_DLY & BLOCK_DLY <   60 ~ "[55,60)"
        , 60 <= BLOCK_DLY & BLOCK_DLY <  Inf ~ "[60,INF)"
        , TRUE ~ NA_character_
      ) # end case_when
    )
}

sort_vector <- c(
  "(-INF,-60]", "(-60,-55]", "(-55,-50]", "(-50,-45]", "(-45,-40]", "(-40,-35]"
  ,"(-35,-30]", "(-30,-25]", "(-25,-20]", "(-20,-15]", "(-15,-10]", "(-10,-5]"
  ,   "(-5,0]",     "(0,5)",    "[5,10)",   "[10,15)",   "[15,20)",  "[20,25)"
  ,  "[25,30)",   "[30,35)",   "[35,40)",   "[40,45)",   "[45,50)",  "[50,55)"
  ,  "[55,60)",   "[60,INF)"
)

package_pbwg_punc_analytic <- function(.punc_with_dly, .sort_vector = sort_vector){
  .punc_with_dly |> 
    dplyr::mutate(DATE = lubridate::date(BLOCK_TIME), COUNT = 1) |> 
    dplyr::group_by(ICAO, DATE, PHASE, DLY_GRP) |> 
    dplyr::summarise(COUNT = sum(COUNT), .groups = "drop") |> 
    tidyr::pivot_wider(
      id_cols = c("ICAO", "DATE", "PHASE")
      , names_from = DLY_GRP
      , values_from = COUNT
      , values_fill = 0) |> 
    dplyr::select(ICAO, DATE, PHASE, dplyr::all_of(.sort_vector)) |> 
    dplyr::mutate(
      N_VALID = rowSums(dplyr::pick(dplyr::all_of(sort_vector)))
      , .after = PHASE)
}
