prepare_eur_txxts <- function(.txxts, .max_txxt = 180){
  tmp <- .txxts |> 
    dplyr::select(PHASE, RWY, STND, MVT_TIME_UTC, BLOCK_TIME_UTC) |> 
    dplyr::mutate(TXXT = dplyr::case_when(
       PHASE == "DEP" ~ difftime(MVT_TIME_UTC, BLOCK_TIME_UTC, unit = "min") |> as.numeric()
      ,PHASE == "ARR" ~ difftime(BLOCK_TIME_UTC, MVT_TIME_UTC, unit = "min") |> as.numeric()   
    ) 
    ) |> 
    # clean and drop edge cases -------------------------
    drop_na() |> 
    filter(0 < TXXT, TXXT < .max_txxt)
  return(tmp)
}

calc_pct20_cases <- function(.txxts, .lb, .ub){
  tmp <- .txxts |> 
    filter(BLOCK_TIME_UTC >= .lb & BLOCK_TIME_UTC <= .ub) |> 
    group_by(PHASE, RWY, STND) |> 
    summarise(N = n(), REF_20 = quantile(TXXT, p = .2), .groups = "drop")
  return(tmp)
}

# calc_various_refs <- function(.ds){
#   refs_2019 <- .ds |> bind_rows() |> prepare_eur_txxts() |> 
#     calc_pct20_cases(ymd_hms("2019-01-01 00:00:00"),ymd_hms("2019-12-31 23:59:59")) |> 
#     rename(N_2019 = N, REF_20_2019 = REF_20)
#   refs_2020 <- .ds |> bind_rows() |> prepare_eur_txxts() |> 
#     calc_pct20_cases(ymd_hms("2020-01-01 00:00:00"),ymd_hms("2020-12-31 23:59:59")) |> 
#     rename(N_2020 = N, REF_20_2020 = REF_20)
#   refs_2021 <- .ds |> bind_rows() |> prepare_eur_txxts() |> 
#     calc_pct20_cases(ymd_hms("2021-01-01 00:00:00"),ymd_hms("2021-12-31 23:59:59")) |> 
#     rename(N_2021 = N, REF_20_2021 = REF_20)
#   refs_2022 <- .ds |> bind_rows() |> prepare_eur_txxts() |> 
#     calc_pct20_cases(ymd_hms("2022-01-01 00:00:00"),ymd_hms("2022-12-31 23:59:59"))|> 
#     rename(N_2022 = N, REF_20_2022 = REF_20)
#   refs_1920 <- .ds |> bind_rows() |> prepare_eur_txxts() |> 
#     calc_pct20_cases(ymd_hms("2019-01-01 00:00:00"),ymd_hms("2020-12-31 23:59:59")) |> 
#     rename(N_1920 = N, REF_20_1920 = REF_20)
#   refs_1921 <- .ds |> bind_rows() |> prepare_eur_txxts() |> 
#     calc_pct20_cases(ymd_hms("2019-01-01 00:00:00"),ymd_hms("2021-12-31 23:59:59")) |> 
#     rename(N_1921 = N, REF_20_1921 = REF_20)
#   refs_1922 <- .ds |> bind_rows() |> prepare_eur_txxts() |> 
#     calc_pct20_cases(ymd_hms("2019-01-01 00:00:00"),ymd_hms("2022-12-31 23:59:59")) |> 
#     rename(N_1922 = N, REF_20_1922 = REF_20)
#   
#   refs <- refs_2019 |> 
#     full_join(refs_2020, by = join_by(PHASE, RWY, STND)) |> 
#     full_join(refs_2021, by = join_by(PHASE, RWY, STND)) |> 
#     full_join(refs_2022, by = join_by(PHASE, RWY, STND)) |> 
#     full_join(refs_1920, by = join_by(PHASE, RWY, STND)) |> 
#     full_join(refs_1921, by = join_by(PHASE, RWY, STND)) |> 
#     full_join(refs_1922, by = join_by(PHASE, RWY, STND))
#   return(refs)
# }
# 
# refs <- ds |> calc_various_refs()
# write_csv(refs, paste0("./data/BRA-EUR-PBWG10-REF-TXXT-", apt, ".csv"))
