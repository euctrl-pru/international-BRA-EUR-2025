#install.packages("tinytex")
library(tinytex)
library(RColorBrewer)

source(here::here("_chapter-setup.R"))


##################################################################################################
### Punctuality
##################################################################################################

# helper function - aggregate punctuality groups
add_dly_early_late_groupings <- function(.punc_df){
  df <- .punc_df |>  
    dplyr::mutate(
      EARLY        = rowSums(across(.cols = `(-INF,-60]`:`(-20,-16]`), na.rm = TRUE) / N_VALID
      ,EARLY_M15M05 = (`(-16,-10]` + `(-10,-5]`) / N_VALID
      ,EARLY_M05M00 =  `(-5,0]`                  / N_VALID
      ,LATE_P00P05  =  `(0,5)`                   / N_VALID
      ,LATE_P05P15  = (`[5,10)` + `[10,15)`)     / N_VALID
      ,LATE         = rowSums(across(.cols = `[15,20)`:`[60,INF)`), na.rm = TRUE) / N_VALID
      ,WITHIN_M05P05= (`(-5,0]` + `(0,5)`)       / N_VALID
      ,WITHIN_M15P15= (`(-16,-10]`+`(-10,-5]`+`(-5,0]`+`(0,5)`+`[5,10)`+`[10,15)`) / N_VALID
    )
}

add_dly_early_late_groupings_month <- function(.punc_df){
  df <- .punc_df |>  
    dplyr::mutate(
      EARLY        = rowSums(across(.cols = `(-INF,-60]`:`(-20,-16]`), na.rm = TRUE) / N_VALID_MONTH
      ,EARLY_M15M05 = (`(-16,-10]` + `(-10,-5]`) / N_VALID_MONTH
      ,EARLY_M05M00 =  `(-5,0]`                  / N_VALID_MONTH
      ,LATE_P00P05  =  `(0,5)`                   / N_VALID_MONTH
      ,LATE_P05P15  = (`[5,10)` + `[10,15)`)     / N_VALID_MONTH
      ,LATE         = rowSums(across(.cols = `[15,20)`:`[60,INF)`), na.rm = TRUE) / N_VALID_MONTH
      ,WITHIN_M05P05= (`(-5,0]` + `(0,5)`)       / N_VALID_MONTH
      ,WITHIN_M15P15= (`(-16,-10]`+`(-10,-5]`+`(-5,0]`+`(0,5)`+`[5,10)`+`[10,15)`) / N_VALID_MONTH
    )
}

# --------- clean ==> why did we move to 16 ~ CODA convention <> PBWG -------
add_dly_early_late_groupings2 <- function(.punc_df){
  df <- .punc_df |>  
    dplyr::mutate(
      EARLY        = rowSums(across(.cols = `(-INF,-60]`:`(-20,-15]`), na.rm = TRUE) / N_VALID
      ,EARLY_M15M05 = (`(-15,-10]` + `(-10,-5]`) / N_VALID
      ,EARLY_M05M00 =  `(-5,0]`                  / N_VALID
      ,LATE_P00P05  =  `(0,5)`                   / N_VALID
      ,LATE_P05P15  = (`[5,10)` + `[10,15)`)     / N_VALID
      ,LATE         = rowSums(across(.cols = `[15,20)`:`[60,INF)`), na.rm = TRUE) / N_VALID
      ,WITHIN_M05P05= (`(-5,0]` + `(0,5)`)       / N_VALID
      ,WITHIN_M15P15= (`(-15,-10]`+`(-10,-5]`+`(-5,0]`+`(0,5)`+`[5,10)`+`[10,15)`) / N_VALID
    )
}
 ########################

#--------
# check this, rewritten code introduces monthly counts, grouping over annual 
# counts and monthly counts ~~ not generalisable or we have to change format /
# deviate from PBWG
#
# think about generalising DATE and not introduce new variables
# e.g. YEAR = floor(DATE, unit = year)
#
#--------------------------------

punc_bra <- read_csv("./data/BRA-punc.csv") |>  
  mutate(REGION = "BRA") |> 
  group_by(APT, PHASE, YEAR = lubridate::year(DATE), REGION, N_VALID) |> 
  summarise(across(.cols = `(-INF,-60]`:`[60,INF)`, .fns = sum), .groups = "drop") |> 
  add_dly_early_late_groupings()

punc_bra_month <- read_csv("./data/BRA-punc.csv") |>  
  mutate(REGION = "BRA", YEAR=lubridate::year(DATE), MONTH=lubridate::month(DATE)) |> 
  group_by(APT, PHASE, YEAR=lubridate::year(DATE), MONTH=lubridate::month(DATE), REGION, N_VALID_MONTH) |> 
  summarise(across(.cols = `(-INF,-60]`:`[60,INF)`, .fns = sum), .groups = "drop") |> 
  add_dly_early_late_groupings_month()

punc_eur_data <- list.files(path = "data", pattern = "PBWG-EUR-PUNC", full.names = TRUE) |> 
  purrr::map(.f = ~ readr::read_csv(.x, show_col_types = FALSE)) |> dplyr::bind_rows()

punc_eur <- punc_eur_data |> rename(APT = ICAO) |>  
  mutate(REGION = "EUR") |> 
  group_by(APT, PHASE, YEAR = lubridate::year(DATE), REGION) |>  #, N_VALID) |> 
  summarise(across(.cols = N_VALID:`[60,INF)`, .fns = sum), .groups = "drop") |> 
  add_dly_early_late_groupings2()

punc_eur_month <- punc_eur_data |> rename(APT = ICAO) |>  
  mutate(REGION = "EUR", YEAR=lubridate::year(DATE), MONTH=lubridate::month(DATE)) |> 
  group_by(APT, PHASE, YEAR=lubridate::year(DATE)
           , MONTH=lubridate::month(DATE), REGION) |>  # , N_VALID) |> 
  summarise(across(.cols = N_VALID:`[60,INF)`, .fns = sum), .groups = "drop") |> 
  add_dly_early_late_groupings2() # _month()



# punctuality plot
punc_plot <- function(.puncdf, .debug = FALSE){
  punc_plot <- .puncdf %>% 
    ggplot(aes(x = SHARE, y = AIRPORT)) +
    geom_col(aes(fill = SLOT))
  
  if(.debug){
    punc_plot <- punc_plot +
      geom_text(aes(label = LABEL), position = position_stack(vjust = 0.5), size = 2)
  }
  
  punc_plot <- punc_plot  +
    scale_x_continuous(labels = scales::percent) +
    scale_fill_brewer(
      palette = "RdYlBu"
      , name = "Time horizon"
      , labels = c("Late", "Late 5-15", "Within 5", "Early 15-5", "Early")
    ) +
    facet_wrap(.~REGION, scales = "free_y") +
    labs(x = NULL, y = NULL) +
    theme( legend.position = "top"
           ,legend.title    = element_text(size = 8) 
           ,legend.text     = element_text(size = 8)
           ,legend.key.size = unit(0.3, "cm"))
  
  return(punc_plot)
}

prepare_punc_plot_data <- function(.punc, .phase, .year){
  tmp <- .punc |> 
    filter(PHASE == .phase) |> 
    select( AIRPORT = APT, YEAR, N_VALID
            , EARLY, EARLY_1505 = EARLY_M15M05
            , WITHIN_5 = WITHIN_M05P05
            , LATE_0515 = LATE_P05P15, LATE
            , REGION) |> 
    pivot_longer(cols = EARLY:LATE, names_to = "SLOT", values_to = "SHARE") |> 
    mutate(
      SLOT = factor(
        SLOT
        , levels = c("LATE","LATE_0515","WITHIN_5","EARLY_1505","EARLY")
      )
      ,LABEL = paste0(SLOT, "\n", round(SHARE, 2))) |> filter(YEAR == .year) 
  return(tmp)
}


#punc <- punc_bra
#punc <- punc_eur
punc <- bind_rows(punc_bra, punc_eur)

# Função para criar o plot
create_punc_plot <- function(event, year, debug) {
  punc |>
    prepare_punc_plot_data(event, year) |>
    punc_plot(.debug = debug) +
    labs(caption = paste(ifelse(event == "ARR", "arrival punctuality", "departure punctuality"), year)) +
    theme(legend.position = "none")
}




##################################################################################################
######### Punc Evolution
####################################################################################################

# Note --> code functions like interfaces, i.e., do not use "absolute" to
# aovid side-effects

#punc_evolution_lineplot <- function(event,limits){
#  tmp <-  punc_bra |> filter(PHASE == event)
 
punc_evolution_lineplot <- function(this_data, event, limits){
  tmp <- this_data |> filter(PHASE == event)
  
  # get color order
  tmp_col <- tmp |> 
    filter(YEAR == 2019) |> select(REGION, APT, N_VALID) |> 
    arrange(REGION, desc(N_VALID)) |> 
   # mutate(COL_SORT = 1:10, .by = REGION)
    mutate(COL_SORT = row_number())
  
  # position airport labels
  tmp2 <-  tmp |> 
    left_join(tmp_col, by = join_by(APT, REGION)) |> 
    mutate( COL_SORT = as.character(COL_SORT))
  
  lbl_df <- tmp2 |> 
    mutate( LBL_YR = 2023  # default - end of line
            ,LBL_YR = case_when(
              APT == "SBGR" ~ 2021
              , APT == "SBBR" ~ 2022
              , APT == "SBCF" ~ 2021
              , APT == "SBRJ" ~ 2021
              , APT == "SBCT" ~ 2022
              , APT == "SBKP" ~ 2022
              , APT == "SBSP" ~ 2020
              , APT == "SBPA" ~ 2020
              , APT == "LEMD" ~ 2020
              , APT == "LSZH" ~ 2020
              , APT == "EDDF" ~ 2020
              , APT == "LFPG" ~ 2020
              , APT == "EHAM" ~ 2021
              ,.default = LBL_YR
            )
    ) |> 
    filter(LBL_YR == YEAR) |> select(REGION, APT, LBL_YR, WITHIN_M15P15, COL_SORT)
  
  p1 <- tmp2 |> 
    ggplot() + 
    geom_line(aes(x = YEAR, y = WITHIN_M15P15, group = APT, color = COL_SORT)) +
    geom_point(aes(x = YEAR, y = WITHIN_M15P15, group = APT, color = COL_SORT)) +
    geom_label_repel(
      data = lbl_df
      , aes(x = LBL_YR, y = WITHIN_M15P15, color = COL_SORT, label = APT)
      , vjust = 0.6, hjust = 0.8
    ) +
    scale_color_brewer(palette = "Paired") +
    scale_y_continuous(
      limits = limits
      , labels = scales::percent_format(scale = 100)
    ) +
    facet_wrap(. ~ REGION) +
    labs(x = NULL, y = "arrival punctuality -/+15 min") +
    theme(legend.position = "none")
  return(p1)
  }
  

####################################################################################################
########### Per Month
####################################################################################################

# punc_per_month <- function(.APT, .YEAR, event, limits){
#   tmp_month <-  punc_bra_month |> filter(PHASE == event, , APT%in%.APT, YEAR==.YEAR)

punc_per_month <- function(.punc_month,.APT, .YEAR, event, limits){
    tmp_month <-  .punc_month |> filter(PHASE == event, , APT%in%.APT, YEAR==.YEAR)
    
    
  # Lista com os nomes dos meses
  month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  p1 <- tmp_month |> 
    ggplot() + 
    geom_line(aes(x = factor(MONTH), y = WITHIN_M15P15, group = APT, color=APT)) +  # Converta MONTH para fator
    geom_point(aes(x = factor(MONTH), y = WITHIN_M15P15, group = APT)) +
    scale_x_discrete(labels = month_names) +  # Define os labels como os nomes dos meses
    scale_color_brewer(palette = "Paired") +
    scale_y_continuous(
      limits = limits,
      labels = scales::percent_format(scale = 100)
    ) +
    # facet_wrap(. ~ REGION) +
    labs(x = NULL, y = "arrival punctuality -/+15 min")
  
  return(p1)
}


####################################################################################################
########### Mov vs Punc per month
####################################################################################################

month_tfc_bra <- read_csv("./data/study_apt_lvl_month.csv")
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


month_tfc_bra <- month_tfc_bra |>
  mutate(YEAR=as.character(YEAR)) |>
  rename(c("APT"="ICAO"))

tmp_month <-  punc_bra_month |> 
  filter(PHASE == "ARR") |>
  select(APT, PHASE, YEAR, MONTH, WITHIN_M15P15) |>
  mutate(YEAR = as.character(YEAR))

punc_mov_month <- month_tfc_bra |>
  left_join(tmp_month)

punc_mov_month_plot <- function(.years, .apt){
  p_tfc_month <- punc_mov_month |>
    filter(YEAR %in% .years, APT == .apt) |>
    mutate(MONTH = factor(MONTH, levels = 1:12, labels = month_names)) |>
    ggplot() +
    geom_col(aes(x = MONTH, y = TOT_FLTS_MONTH, fill = YEAR), 
             position = position_dodge()) +
    geom_line(aes(x = factor(MONTH), y = WITHIN_M15P15 * 30000, group = YEAR, color = YEAR), size = 1) +
    geom_point(aes(x = factor(MONTH), y = WITHIN_M15P15 * 30000, group = YEAR, color = YEAR)) +
    scale_x_discrete(labels = month_names) +
    scale_y_continuous(
      name = "Total Flights",
      sec.axis = sec_axis(~ . / 30000, name = "Percentage Within M15P15")
    ) +
    scale_fill_brewer(palette = "Blues") +     # Paleta automática para as colunas
    scale_color_brewer(palette = "Set1") +     # Paleta automática diferente para as linhas
    labs(x = NULL, y = NULL, fill = NULL, color = NULL, title = .apt) +
    guides(
      fill = guide_legend(order = 1),           # Ordem da legenda para as colunas
      color = guide_legend(order = 2)           # Ordem da legenda para as linhas
    ) +
    theme(
      legend.position = "top",
      legend.box = "vertical",                  # Coloca as legendas em caixa vertical
      legend.key.size = unit(0.5, "line")
    )
  
  p_tfc_month

}


####################################################################################################
########### Mov vs Punc per APT
####################################################################################################

study_apt_lvl  <- read_csv("./data/study_apt_lvl.csv")

punc_mov_apt_plot <- function(.years){
  study_apt_lvl <- study_apt_lvl |>  
    mutate(YEAR = as.character(YEAR)) |> # coerce YEAR to discrete variable
    filter(YEAR %in% .years) |>
    rename(c("APT"="ICAO"))
  
  tmp_annual <-  punc_bra |> 
    filter(PHASE == "ARR") |>
    select(APT, PHASE, YEAR, WITHIN_M15P15) |>
    mutate(YEAR = as.character(YEAR))
  
  punc_mov_year <- study_apt_lvl |>
    left_join(tmp_annual)

  p_tfc_year <- punc_mov_year |>
    filter(YEAR %in% .years) |>
    ggplot() +
    geom_col(aes(x = APT, y = TOT_FLTS_YEAR, fill = YEAR), 
             position = position_dodge()) +
    geom_line(aes(x = factor(APT), y = WITHIN_M15P15 * 300000, group = YEAR, color = YEAR), size = 1) +
    geom_point(aes(x = factor(APT), y = WITHIN_M15P15 * 300000, group = YEAR, color = YEAR)) +
    scale_y_continuous(
      name = "Total Flights",
      sec.axis = sec_axis(~ . / 300000, name = "Percentage Within M15P15")
    ) +
    scale_fill_brewer(palette = "Blues") +     # Paleta automática para as colunas
    scale_color_brewer(palette = "Set1") +     # Paleta automática diferente para as linhas
    labs(x = NULL, y = NULL, fill = NULL, color = NULL) +
    guides(
      fill = guide_legend(order = 1),           # Ordem da legenda para as colunas
      color = guide_legend(order = 2)           # Ordem da legenda para as linhas
    ) +
    theme(
      legend.position = "top",
      legend.box = "vertical",                  # Coloca as legendas em caixa vertical
      legend.key.size = unit(0.5, "line")
    )
  
  p_tfc_year  
}



####################################################################################################
########### Early vs Late
####################################################################################################

plot_early_vs_late <- function(.early_vs_late, .phase, .year
                               , .limits = c(-.5,0.4)
                               , .pretty_label_number = 3){
  if(.phase == "ARR") my_cap <- paste("early vs late arrivals in ", .year)
  if(.phase == "DEP") my_cap <- paste("early vs late departures in ", .year)
  
  tmp <-.early_vs_late |> 
    mutate(SLOT_ON_X = if_else(SLOT == "EARLY", -SHARE, SHARE))
  # get nice cuts from spread
  pretty_breaks <- pretty(tmp$SLOT_ON_X, n = .pretty_label_number)
  # set max range
  range_limits <- .limits #c(min(tmp$SLOT_ON_X), max(tmp$SLOT_ON_X))
  
  viz <- tmp |> 
    ggplot() + 
    geom_col(aes(y = AIRPORT
                 , x = SLOT_ON_X
                 , group = SLOT, fill = SLOT)
    ) +
    #scale_x_continuous(labels = scales::percent_format(scale = 100)) +
    scale_x_continuous(
      limits = range_limits
      , breaks =         pretty_breaks
      , labels = paste0( pretty_breaks|> abs() *100, "%")
    ) +
    labs(x = NULL, y = NULL, fill = NULL
         ,caption = my_cap ) +
    theme( legend.position = "top"
           ,legend.title    = element_text(size = 8) 
           ,legend.text     = element_text(size = 8)
           ,legend.key.size = unit(0.3, "cm"))
  return(viz)
}

# Função para criar o plot de 'EARLY' e 'LATE'
create_earlylate_plot <- function(event, year) {
  punc_bra |>
    prepare_punc_plot_data(event, year) |>
    filter(SLOT %in% c("EARLY", "LATE")) |>
    plot_early_vs_late(event, year)
}

####################################################################################################
########### Early vs Late per month
####################################################################################################


prepare_punc_plot_data_month <- function(.punc, .phase, .year, .month){
  tmp <- .punc |> 
    filter(PHASE == .phase) |> 
    select( AIRPORT = APT, YEAR, MONTH, N_VALID_MONTH
            , EARLY, EARLY_1505 = EARLY_M15M05
            , WITHIN_5 = WITHIN_M05P05
            , LATE_0515 = LATE_P05P15, LATE
            , REGION) |> 
    pivot_longer(cols = EARLY:LATE, names_to = "SLOT", values_to = "SHARE") |> 
    mutate(
      SLOT = factor(
        SLOT
        , levels = c("LATE","LATE_0515","WITHIN_5","EARLY_1505","EARLY")
      )
      ,LABEL = paste0(SLOT, "\n", round(SHARE, 2))) |> filter(YEAR == .year, MONTH==.month) 
  return(tmp)
}


# Função para criar o plot de 'EARLY' e 'LATE' para um mês específico
create_earlylate_month_plot <- function(event, year, month) {
  punc_bra_month |>
    prepare_punc_plot_data_month(event, year, month) |>
    filter(SLOT %in% c("EARLY", "LATE")) |>
    plot_early_vs_late(event, paste(year, "/", month))
}




















