### ------------ DATA PREPARATION - PBWG FORMAT ------- EUR AIRPORTS

# check if files are still there
# pth_apdf <- here::here(here::here() |> dirname(), "__DATA","APDF")

read_apdf_parquet <- function(){}


# -------------- helper functions ------------------------------------------
trim_vec  <- c(
    "AP_C_FLTID"
  , "ADEP_ICAO", "ADES_ICAO"
  ,"AP_C_REG","ARCTYP","AC_CLASS"
  , "AP_C_RWY", "AP_C_STND"
  , "MVT_TIME_UTC", "BLOCK_TIME_UTC", "SCHED_TIME_UTC"
  , "C40_CROSS_TIME", "C40_BEARING"
  , "C100_CROSS_TIME", "C100_BEARING"
  , "SRC_PHASE"
  , "IM_SAMAD_ID" 
)

trim_apdf <- function(.apdf, .trim_vec = trim_vec){
  trimmed_apdf <- .apdf |> 
    dplyr::select(dplyr::any_of(.trim_vec))
  
  return(trimmed_apdf)
}

make_nice_names <- function(.apdf){
  names(.apdf) <- gsub(
    pattern = "(AC_)|(AP_C_)|(_UTC)|(_ICAO)|(SRC_)"
    ,replacement = ""
    ,names(.apdf)
  )
  return(.apdf)
}

add_dof <- function(.apdf){
  .apdf <- .apdf %>% 
    mutate(DATE = case_when(
       !is.na(BLOCK_TIME) ~ lubridate::date(BLOCK_TIME)
      , is.na(BLOCK_TIME) & !is.na(MVT_TIME) ~ lubridate::date(MVT_TIME)
      , TRUE ~ as.Date(NA)
    ))
  
  return(.apdf)
}

prep_apdf <- function(.apdf){
  this_ds <- .apdf |> 
    trim_apdf() |> 
    make_nice_names() |> 
    add_dof()
  
  return(this_ds)
}

ecac_2digits <- function(){
  #------------- identification of domestic (aka regional) traffic == ECAC
  ECAC_North_West <- c("EB", "ED", "ET", "EG", "EH", "EI", "EK", "EL", "LF", "LN", "LO", "LS")
  ECAC_South_West <- c("GC", "GE", "LE", "LP", "LX")
  ECAC_North_East <- c("EE", "EF", "EN", "EP", "ES", "EV", "EY", "LK", "LZ", "UK")
  ECAC_South_East <- c("LA", "LB", "LC", "LD", "LG", "LH", "LI", "LJ", "LM", "LQ", "LR", "LT",
                       "LU", "LW", "LY", "UB", "UD", "UG")
  
  # TODO check remaining airports and fix ECAC Oceanic
  ecac <- c(ECAC_North_West,ECAC_North_East, ECAC_South_West, ECAC_South_East)
}

# ----------------- END HELPER FUNCTIONS --------------------------------------


extract_daily_stats <- function(.apdf, .apt = apt, .yr = yr, ...){
  
  ecac <- ecac_2digits()
  
  .apdf <- .apdf %>% mutate( ICAO = .apt)
  
  arr_dep <- .apdf  %>% 
    group_by(ICAO, DATE) %>%
    summarise( ARRS     = sum(PHASE == "ARR", na.rm = TRUE)
               ,DEPS     = sum(PHASE == "DEP", na.rm = TRUE)
               ,SRC_NA   = sum(is.na(PHASE))
               ,.groups = "drop"
    )
  
  reg_arrs <- .apdf %>% filter(PHASE == "ARR") %>%
    mutate(ADEP_REG = case_when(
      stringr::str_extract(ADEP, pattern = "^[A-Z]{2}") %in% ecac ~ "EUR")
    ) %>%
    group_by(ICAO, DATE) %>%
    summarise(ARRS_REG = sum(ADEP_REG %in% "EUR"))
  
  reg_deps <- .apdf %>% filter(PHASE == "DEP") %>%
    mutate(ADES_REG = case_when(
      stringr::str_extract(ADES, pattern = "^[A-Z]{2}") %in% ecac ~ "EUR")
    ) %>%
    group_by(ICAO, DATE) %>%
    summarise(DEPS_REG = sum(ADES_REG %in% "EUR"))
  
  hml <- .apdf %>% group_by(ICAO, DATE) %>%
    summarise( HEL = sum(CLASS %in% "HEL", na.rm = TRUE)
               ,H = sum(CLASS %in% c("H"), na.rm = TRUE)
               ,M = sum(CLASS %in% c("M","MJ","MT"), na.rm = TRUE)
               ,L = sum(CLASS %in% c("L","LJ","LT","LP"), na.rm = TRUE)
               ,'NA' = sum(is.na(CLASS))
               , .groups = "drop"
    )
  
  reg_tfc <- reg_arrs %>% left_join(reg_deps, by = c("ICAO","DATE"))
  apt_tfc <- arr_dep %>%  left_join(reg_tfc,  by = c("ICAO","DATE"))
  apt_tfc <- apt_tfc %>%  left_join(hml,      by = c("ICAO","DATE"))
  
  apt_tfc <- apt_tfc %>% filter(lubridate::year(DATE) == .yr)
  
  return(apt_tfc)
}

write_out_daily_apt_tfc <- function(
    .dly_tfc, .apt, .yr
    ,.postfix = NULL
    ,.out_type = "csv"
    ){
  out_name <- paste0("./data-prep-eur/apt-tfc-", .apt, "-", .yr)
  if(!is.null(.postfix)){ out_name <- paste0(out_name, "-", .postfix)}
  out_name <- paste0(out_name,".", .out_type)
  
  readr::write_csv(.dly_tfc, out_name)
}

# eddf_2023 <- read_zip(pth_apdf, "apdf-2023.zip", "EDDF_APDF_2023.gz.parquet") |> tibble::tibble()

read_and_write_apt_tfc <- function(.apt_apdf, .apt, .yr){
  .apt_apdf |> prep_apdf() |> 
    extract_daily_stats(.apt,.yr) |> 
    write_out_daily_apt_tfc(.apt,.yr)
}
