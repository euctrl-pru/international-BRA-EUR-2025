source("_chapter-setup.R")

# set colors - added to chapter-setup
# bra_col <- getElement(bra_eur_colours, "BRA")
# eur_col <- getElement(bra_eur_colours, "EUR")

##################################################################################################
## Network Level Air Traffic

tfc_bra <- 
  read_csv("./data/tfc_movts_all.csv") |> 
  mutate(DATE = lubridate::as_datetime(DATE)) |> 
  filter(DATE < max_date)

bra_tfc_switch <- lubridate::date("2024-12-31")

#------------ BRA ----------------------

plot_bra_annual_traffic_function <- function(){
  # Split in years - ax
  year_boundaries <- seq(as.Date("2019-01-01"), as.Date("2024-01-01"), by = "years")
  max_min_by_year<-read_csv("./data/max_min_by_year.csv")
   plot_bra_annual_traffic <- 
    #  tfc_movts_all_no_hel  |> 
    tfc_bra |> mutate(DATE = as.Date(DATE)) |> 
    ggplot(aes(x = DATE)) +
    geom_line(aes(y = MVTS_NORM_ROLLAVG), colour = bra_col) +
    geom_point(aes(y = DLY_FLTS), colour = bra_col, alpha = 0.2, size = 0.2) +
    # Dashed lines for max and min numbers
    geom_segment(
      data = max_min_by_year,
      aes(x = max_date, xend = max_date, y = 0, yend = max_avg),
      linetype = "dashed", color = "grey70", size = 0.8
    ) +
    geom_segment(
      data = max_min_by_year,
      aes(x = min_date, xend = min_date, y = 0, yend = min_avg),
      linetype = "dashed", color = "grey70", size = 0.8
    ) +
    # Insert Point - max and min
    geom_point(
      data = max_min_by_year,
      aes(x = max_date, y = max_avg, color = "High"),
      size = 3
    ) +
    geom_point(
      data = max_min_by_year,
      aes(x = min_date, y = min_avg, color = "Low"),
      size = 3
    ) +
    # Labels - max and min
    geom_label(
      data = max_min_by_year,
      aes(x = max_date, y = max_avg, label = max_avg),
      fill = "darkgreen", color = "white", fontface = "bold", size = 3
    ) +
    geom_label(
      data = max_min_by_year,
      aes(x = min_date, y = min_avg, label = min_avg),  
      fill = "red", color = "white", fontface = "bold", size = 3
    ) +
    # Breaks - year boundaries
    scale_x_date(
      breaks = year_boundaries,                      
      labels = NULL,                                 
      expand = c(0, 0)                               
    ) +
    # Vertical lines - year boundaries
    geom_vline(
      xintercept = year_boundaries,
      linetype = "solid", color = "grey80", size = 0.5
    ) +
    # Dates position - x
    annotate(
      "text",
      x = c(max_min_by_year$max_date, max_min_by_year$min_date),
      y = -200,  # Vertical position
      label = format(c(max_min_by_year$max_date, max_min_by_year$min_date), "%d %b"),
      color = "black",
      size = 3,
      angle = 0,
      hjust = 0.5
    ) +
    # Years (years boundaries) position
    annotate(
      "text",
      x = year_boundaries + 180,  
      y = -1000,                  # Vertical position
      label = format(year_boundaries, "%Y"),
      color = "black",
      size = 3.5,
      angle = 0,
      hjust = 0.5
    ) +
    # Grid lines - 1000 / 1000 - y ax
    scale_y_continuous(
      breaks = seq(0, max(tfc_bra$MVTS_NORM_ROLLAVG, na.rm = TRUE), by = 1000),
      minor_breaks = NULL
    ) +
    # Legend Colors
    scale_color_manual(
      name = "",
      values = c("High" = "darkgreen", "Low" = "red"),
      labels = c("Highest daily average (7-days) of the year", "Lowest daily average (7-days) of the year")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_line(color = "grey90"), # Only for 1000 / 1000
      panel.grid.major.x = element_blank(),               # No vertical grids
      panel.grid.minor = element_blank(),                 
      axis.text.x = element_blank(),                      
      legend.position = "bottom"                          
    ) +
    # Title / Subtitle
    labs(x = NULL, y = NULL,  subtitle = "Brazil Region daily movement (rolling 7-day average)")
  return(plot_bra_annual_traffic)
  }

#------------EUR ------------------------
tfc_eur  <- read_csv("./data/PBWG-EUR-region-traffic.csv", show_col_types = FALSE) |> 
  filter(between(lubridate::year(DATE), 2019, 2022))
tfc_eur2 <- read_csv("./data/PBWG-EUR-region-traffic-2023.csv", show_col_types = FALSE) |> 
  filter(lubridate::year(DATE) == 2023, DATE < max_date)
#------- check with Quinten - some flights missing
tfc_eur3 <- read_csv("./data/EUR-network-tfc-2023.csv", show_col_types = FALSE) |> 
  filter(lubridate::year(DATE) == 2023, DATE < max_date) |> 
  mutate(DLY_FLTS = FLIGHTS, 
         MVTS_NORM_ROLLAVG = zoo::rollmean(DLY_FLTS, k = 7, fill = NA)
         ,VERSION = "2024")

tfc_eur_2023 <- read_csv("./data/EUR-network-traffic-2023.csv", show_col_types = FALSE)
tfc_eur_2024 <- read_csv("./data/EUR-network-traffic-2024.csv", show_col_types = FALSE)
tfc_eur_tmp  <- bind_rows(tfc_eur_2023, tfc_eur_2024) |> 
  mutate(MVTS_NORM_ROLLAVG = zoo::rollmean(FLTS, k = 7, fill = NA))

tfc_eur <- bind_rows(tfc_eur, tfc_eur2) |> mutate(VERSION = "2023") |> 
  #-------- for edition 2024 in 2025 --------
  filter(DATE < lubridate::ymd_hms("2023-10-01 00:00:00"))
tfc_eur_tmp <- tfc_eur_tmp |> 
  filter(DATE >= lubridate::ymd_hms("2023-10-01 00:00:00"))

tfc_eur <- tfc_eur |> 
  mutate( DLY_FLTS = ARRS - ARRS_DOM + DEPS + OVR_FLTS
          ,MVTS_NORM_ROLLAVG = zoo::rollmean(DLY_FLTS, k = 7, fill = NA)) |> 
  #--- add on for 2023
  #bind_rows(tfc_eur3)
  #-------- for edition 2024 in 2025 --------
 # mutate(DATE = date(DATE)) |> 
  bind_rows(tfc_eur_tmp)

plot_eur_annual_traffic_function <- function(.tfc_eur = tfc_eur, .eur_col = eur_col){
plot_eur_annual_traffic <- .tfc_eur  |> 
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = MVTS_NORM_ROLLAVG), colour = eur_col) +
  geom_point(aes(y = DLY_FLTS), colour = .eur_col, alpha = 0.2, size = 0.2) +
  labs(x = NULL, y = NULL,  subtitle = "European Region daily movement (rolling 7-day average)")

  return(plot_eur_annual_traffic)
}


################################################################################################

plot_timeline_per_year <- function(.df,
                                   .years,
                                   .fake_year = 666){
  df <- .df  %>%  
    mutate( DATE2 = DATE, 
            YEAR = lubridate::year(DATE) |> as.character()) %>% 
    filter(YEAR %in% .years)
  
  # inject "fake year" for same x-axis value
  lubridate::year(df$DATE2) = .fake_year
  
  df <- df |>
    mutate(
      ,ALPHA = case_when(
        YEAR == min(YEAR) ~ 1
        ,YEAR == max(YEAR) ~ 1,
        .default = 0.8
      )
    )
  
  df |> 
    ggplot(aes(x = DATE2, y = MVTS_NORM_ROLLAVG)) +
    # geom_line(aes(group = YEAR, colour = as.factor(YEAR), alpha = ALPHA)) +
    geom_line(aes(group = YEAR, colour = YEAR
                  ,alpha = ALPHA
                  #        ,linewidth = LW
                  #         , alpha = ifelse(YEAR %in% c(2019,2023), 1, 0.8)
    )
    ) +
    scale_x_datetime(labels = scales::date_format("%b")) +
    scale_alpha(guide = "none", range = c(0.4,1)) +
    # scale_color_brewer() +
    labs(x = NULL, y = NULL, colour = NULL) + 
    theme(legend.position = "top")
}


fig_annual_network <- function(.years){
  p_bra <- tfc_bra |>
    plot_timeline_per_year(.years) +
     labs(subtitle = "Brazil", colour = "Brazil")  # Define legenda do Brasil
  
  p_eur <- tfc_eur |> plot_timeline_per_year(.years) + labs(subtitle = "Europe")

  return(p_bra / p_eur + plot_layout(guides = "collect") & theme(legend.position = "top"))
}


##############################################################################################
## Airport Level Air Traffic

################################### BRA #####################################################
# plot functions -------------------------------------------
p_share_of_network <- function(.annual_share_of_network_df){
  p_share <- .annual_share_of_network_df |> 
    ggplot(aes(x = YEAR, y = SHARE)) + 
    geom_point() + geom_line(aes(group = "annual")) + 
    scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
    labs(x = NULL, y = "share of overall network traffic")
  return(p_share)
}

study_apt_lvl  <- read_csv("./data/study_apt_lvl.csv")

study_apt_lvl <- study_apt_lvl |>  
  mutate(YEAR = as.character(YEAR)) # coerce YEAR to discrete variable

options(scipen = 999)  # set number options

annual_tfc_bra <- tfc_bra |> 
  #-------- correct for fix ------------------
filter(DATE < bra_tfc_switch | DATE >= bra_tfc_switch %m+% years(2)) |> 
  mutate(DATE = case_when(
    DATE >= (bra_tfc_switch %m+% years(2)) ~ DATE %m-% years(2)
    , .default = DATE 
  )
  , YEAR = year(DATE)
  , REG = "BRA"
  ) |> 
  summarise(N = n(), FLIGHTS = sum(DLY_FLTS), .by = c(REG, YEAR))

annualised_study_apt <- study_apt_lvl |> mutate(REG = "BRA") |> 
  summarise(APTS_TOT = sum(TOT_FLTS_YEAR), .by = c(REG, YEAR)) |> 
  mutate(YEAR = as.factor(YEAR))

tmp_annual_share <- annual_tfc_bra |> mutate(YEAR = as.factor(YEAR)) |> 
  left_join(annualised_study_apt, by = join_by(REG, YEAR)) |> 
  mutate(SHARE = APTS_TOT / FLIGHTS)

#========== PLOTS

#append airport names
study_apt_lvl <- study_apt_lvl |> 
  left_join(bra_apts_names, by = join_by(ICAO)) |> 
  mutate(
    NAME = case_match(NAME
                      , "Belo Horizonte" ~ "B.Horizonte"
                      , "Rio Dumont"     ~ "R.Dumont"
                      , "Porto Alegre"   ~ "P.Alegre"
                      , .default = NAME)
  ) 



get_p_study1 <- function(.years){
  p_study1 <- study_apt_lvl %>% filter(YEAR %in% .years)  |>
    ggplot() +
    geom_col(aes(x = NAME, y = TOT_FLTS_YEAR, fill = YEAR)
             , position = position_dodge()
    ) + 
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    #    scale_x_discrete(labels = scales::label_wrap(8)) +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme( legend.position = "top"
           ,legend.key.size = unit(0.5,"line")
    )  
  return(p_study1)
}



fig_bra_apt_tfc <- function(.years){
  return((p_share_of_network(tmp_annual_share %>% filter(YEAR %in% .years)) + scale_x_discrete(guide = guide_axis(n.dodge = 2))) + 
    get_p_study1(.years) + 
    plot_layout(widths = c(1, 4)))
} 
  
################################### EUR #####################################################

tfc_apts_eur <- arrow::read_parquet(
  "./data/traffic_counts_airport_daily.parquet") |> 
  dplyr::filter(REG == "EUR", DATE < lubridate::ymd("2023-01-01"))

tfc_apts_eur_2023 <- arrow::read_parquet(
  "./data/traffic_counts_airport_daily_partial2023.parquet") |> 
  dplyr::select(REG, ICAO, DATE, ARRS, DEPS
         , HEAVY = H, MED = M, LIGHT = L
         , ARRS_DOM = ARRS_REG, DEPS_DOM = DEPS_REG) |> 
  dplyr::filter(dplyr::between(DATE, lubridate::ymd("2023-01-01"), max_date))

tfc_apts_eur <- bind_rows(tfc_apts_eur, tfc_apts_eur_2023) |> 
  dplyr::filter(ICAO %in% eur_apts)

# annual network level traffic
annual_tfc_eur <- tfc_eur |> 
  mutate(YEAR = lubridate::year(DATE)) |>  
  filter( (VERSION == "2024") | (VERSION == "2023" & YEAR %in% 2019:2022) ) |> 
  summarise(N = n(), FLIGHTS = sum(DLY_FLTS), .by = c(REG, YEAR))

# annual traffic at each airport -------------
annual_tfc_apt <- tfc_apts_eur |> 
  #------- filter to 2022 and eur_apts
  filter(ICAO %in% eur_apts, DATE < lubridate::ymd("2023-01-01")) |> 
  #-----------------------------------
  mutate(TOT_FLTS = ARRS + DEPS) |> 
  group_by(REG, ICAO, YEAR = lubridate::year(DATE)) |> 
  summarise(N = n(), TOT_FLTS_YEAR = sum(TOT_FLTS), .groups = "drop") # |> 
  #filter(between(YEAR, 2019, 2024)) |> 
  #mutate(YEAR = as.character(YEAR))

# add annual count for EUR
tfc_apts_eur_monthly_new <- readr::read_csv(
  here::here("data","EUR-study-apt-lvl-month.csv")
  , show_col_types = FALSE) 
tfc_apts_eur_annual_new <- tfc_apts_eur_monthly_new |> 
  dplyr::group_by(ICAO, YEAR) |> 
  dplyr::reframe(TOT_FLTS_YEAR = sum(TOT_FLTS), N = sum(N)) |> 
  dplyr::mutate(REG = "EUR")

# add new data to older stats
annual_tfc_apt <- annual_tfc_apt |> 
  dplyr::bind_rows(tfc_apts_eur_annual_new) 

# # annualised traffic of all study airports
annual_all_apts <- annual_tfc_apt |>
  reframe(APTS_TOT = sum(TOT_FLTS_YEAR), .by = c(REG, YEAR))

tmp <- annual_tfc_eur |> # mutate(YEAR = as.factor(YEAR)) |>
  left_join(annual_all_apts, join_by(REG, YEAR)) |>
  mutate(SHARE = APTS_TOT / FLIGHTS)

#append airport names
annual_tfc_apt <- annual_tfc_apt |>
  left_join(eur_apts_names, by = join_by(ICAO))


get_p1_eur <- function(.years){

  p1_eur <- annual_tfc_apt  |>  filter(YEAR %in% .years) |>   
    ggplot() +
    geom_col(aes(x = NAME, y = TOT_FLTS_YEAR, fill = YEAR)
             , position = position_dodge(preserve = "single")
    ) + 
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(x = NULL, y = NULL, fill = NULL
         #,  title = "European Airports Annual Movement"
    ) +
    theme( legend.position = "top"
           ,legend.key.size = unit(0.5,"line")
    )
  return(p1_eur)
}



fig_eur_apt_tfc <- function(.years){
  p1 <- p_share_of_network(tmp |>  filter(YEAR %in% .years)) + scale_x_discrete(guide = guide_axis(n.dodge = 2))
  #p1 <- patchwork::plot_spacer()
  p2 <- get_p1_eur(.years) 
  this_p <- p1 + p2 + plot_layout(widths = c(1, 4))
  
  return(this_p)
} 




###########################################################################################
# 1 Airport per month

month_tfc_bra <- read_csv("./data/study_apt_lvl_month.csv")
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

month_tfc_bra <- month_tfc_bra |>
  mutate(YEAR=as.character(YEAR))

airport_per_year <- function(.apt, .years){
  p_tfc_month <- month_tfc_bra  |>
    filter(YEAR %in% .years, ICAO == .apt) |>
    mutate(MONTH = factor(MONTH, levels = 1:12, labels = month_names)) |>
    ggplot() +
    geom_col(aes(x = MONTH, y = TOT_FLTS_MONTH, fill=YEAR), 
             position = position_dodge()) +
    scale_x_discrete(labels = month_names) +
    labs(x = NULL, y = NULL, fill = NULL, title = .apt) +
    theme( legend.position = "top"
           ,legend.key.size = unit(0.5,"line"))  
  
  return(p_tfc_month)
}




############################################################################################
# fig-apt-annual-change

get_p_study3_bra <- function(.year){
  p_study3_bra <- study_apt_lvl |>
    filter(YEAR == as.character(.year)) |>
    ggplot(aes(y = reorder(NAME, TOT_FLTS_YEAR), x = TOT_FLTS_YEAR)) + 
    #geom_col(aes(fill = I("#52854C"))
    geom_col(aes(fill = I(getElement(bra_eur_colours, "BRA")))
             , width = 0.9) +
    geom_text(aes(x = 500, label = ICAO), hjust = 0, size = 3, color = "white") +
    scale_x_continuous(labels = scales::comma, limits = c(NA, 450000)) +
    labs(y = NULL,  x = as.character(.year)) +
    theme(legend.position = "none")
  return(p_study3_bra)
}


get_p_study4_bra <- function(.year_ref, .year_comp){
  ann_var_bra <- study_apt_lvl %>% 
    # comparação entre ano de referência e ano anterior
    filter(YEAR %in% c(as.character(.year_comp), as.character(.year_ref))) %>% 
    mutate(YEAR = as.numeric(YEAR)) %>%
    tidyr::pivot_wider(
      id_cols = "ICAO",
      names_from = "YEAR", names_prefix = "YR",
      values_from = "TOT_FLTS_YEAR"
    ) %>% 
    mutate(
      YR_DIFF = !!sym(paste0("YR", .year_ref)) - !!sym(paste0("YR", .year_comp)),
      YR_DIFF_P = YR_DIFF / !!sym(paste0("YR", .year_comp))
    ) %>% 
    mutate(COL = case_when(YR_DIFF_P < 0 ~ "#D61A46", TRUE ~ "#98CA32"))
  
  
  p_study4_bra <- ggplot() + 
    geom_col( 
      data = ann_var_bra,
      mapping = aes(x = reorder(ICAO, !!sym(paste0("YR", .year_ref))), 
                    y = YR_DIFF_P, fill = I(COL)),
      width = 0.9
    ) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      limits = c(-1.5, 0.2)
    ) +
    theme(
      legend.position = "none",
      axis.text.y = element_blank()
    )
  return(p_study4_bra)
}



get_p_study3 <- function(.year){
  p_study3 <- annual_tfc_apt |>
    filter(YEAR == as.character(.year)) |>
    ggplot(aes(y = reorder(NAME, TOT_FLTS_YEAR), x = TOT_FLTS_YEAR)) + 
    geom_col(aes(fill = I(getElement(bra_eur_colours, "EUR")))
             , width = 0.9) +
    #scale_y_discrete(labels = scales::label_wrap(8)) +
    geom_text(aes(x = 500, label = ICAO), hjust = 0, size = 3, color = "white") +
    scale_x_continuous(labels = scales::comma, limits = c(NA, 450000)) +
    labs(x = NULL,  x = as.character(.year)) +
    theme(legend.position = "none")
  
  return(p_study3)
}



get_p_study4 <- function(.year_ref, .year_comp) {
  ann_var <- annual_tfc_apt  %>%
    filter(
      YEAR %in% c(as.character(.year_comp), as.character(.year_ref))
    ) %>%
    mutate(YEAR = as.character(YEAR), YEAR = as.numeric(YEAR)) %>%
    tidyr::pivot_wider(
      id_cols = "ICAO",
      names_from = "YEAR", names_prefix = "YR",
      values_from = "TOT_FLTS_YEAR"
    ) %>%
    mutate(
      YR_DIFF = !!sym(paste0("YR", .year_ref)) - !!sym(paste0("YR", .year_comp)),
      YR_DIFF_P = YR_DIFF / !!sym(paste0("YR", .year_comp))
    ) %>%
    mutate(COL = case_when(YR_DIFF_P < 0 ~ "#D61A46", TRUE ~ "#98CA32"))
  
  p_study4 <- ggplot() +
    geom_col(
      data = ann_var,
      mapping = aes(x = reorder(ICAO, !!sym(paste0("YR", .year_ref))), 
                    y = YR_DIFF_P, fill = I(COL)),
      width = 0.9
    ) +
    coord_flip() +
    labs(x = NULL, y = paste(.year_ref, "vs", .year_comp)) +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      limits = c(-0.5, 0.2)
    ) +
    theme(
      legend.position = "none",
      axis.text.y = element_blank()
    )
  
  return(p_study4)
}



annual_change_bra <- function(.year_ref, .year_comp){
  combo_bra <- (get_p_study3_bra(.year_ref) + labs(x = NULL, y = NULL)) + 
    get_p_study4_bra(.year_ref, .year_comp) + 
    plot_layout(widths = c(3, 1))  
  
  return(combo_bra)
}

annual_change_eur <- function(.year_ref, .year_comp){
  combo_eur <- 
    (get_p_study3(.year_ref) + labs(y = NULL)  #+ labs(y = "annual traffic 2022")
    ) + 
    (get_p_study4(.year_ref, .year_comp) + labs(y = NULL) #+ labs(y = "annual variation 2022/2019")
    ) +
    plot_layout(widths = c(3, 1))  
  
  return(combo_eur)
  
  }




######################################################################################
## Peak Day


pk_day <- read_csv("./data/BRA-airport-tfc-peakday-fix.csv", show_col_types = FALSE)


pk_day_plot <- function(.years){
  pk_day <- pk_day |> filter(YEAR%in%.years)
  
  return (pk_day |> ggplot() + 
    geom_col(aes(y = ICAO, x = PEAK_DAY_PCT, fill = as.factor(YEAR)), position = position_dodge2(preserve = "single")))
}



#########################################################################################

peak_day_from_counts <- function(.counts, .pct = 0.99){
  peak <- .counts %>% 
    mutate( YEAR = lubridate::year(DATE)
            ,TOT  = ARRS + DEPS) %>% 
    group_by(ICAO, YEAR) %>% 
    summarise(PEAK_DAY_PCT = quantile(TOT, probs = .pct), .groups = "drop")
}

add_nbr_rwy <- function(.pdfc){
  peak <- .pdfc %>% 
    mutate(RWY = case_when(
      ICAO == "EHAM" ~ 6
      ,ICAO %in% c("EDDF","LFPG","LEMD","LIRF") ~ 4
      ,ICAO %in% c("LEBL","LSZH") ~ 3
      ,ICAO %in% c("EGLL","EDDM","SBGR","SBSP","SBGL","SBBR","SBRJ","SBSV","SBCT") ~ 2
      ,ICAO %in% c("EGKK","SBKP","SBCF","SBPA") ~ 1
      ,TRUE ~ as.numeric(NA)
    ))
}

peak_day_bra  <- read_csv("./data/BRA-airport-tfc-peakday-fix.csv", show_col_types = FALSE)

peak_day_bra <- peak_day_bra |> 
  add_nbr_rwy() |> 
  mutate(REGION = "BRA") 

peak_day_eur <- tfc_apts_eur |> 
  peak_day_from_counts() |> 
  add_nbr_rwy() |> 
  mutate(REGION = "EUR")


peak_day_comb <- bind_rows(peak_day_bra
                           #, peak_day_eur
                           )

plot_peak_day_tfc <- function(.df, .year, ...){
  viz <- ggplot( data = .df %>% filter(YEAR == .year)
                 ,aes( x = PEAK_DAY_PCT
                       #, y = reorder(ICAO, PEAK_DAY_PCT)
                       ,y = reorder(NAME, PEAK_DAY_PCT)
                 )
  ) + 
    geom_col(aes(fill = REGION)) +
    geom_text(aes(x = 20, label = ICAO)
              , hjust = 0, color = "white", size = 3
    ) +
    scale_fill_manual(values = bra_eur_colours) + 
    facet_grid(RWY ~., as.table = FALSE, switch = "y", scales = "free", space = "free") +
    # my_own_theme_bw +
    labs(# x = paste0("peak day traffic (", .year,")") ,  # blank out xlabel == caption
      y = NULL, fill = "Region") +
    theme(legend.position = c(0.9, 0.15)
          ,axis.ticks = element_blank()
    )
  return(viz)
}

peak_day_pct_plot <- function(.year){
  peak_day_comb |> 
    left_join(bind_rows(bra_apts_names, eur_apts_names), by = join_by(ICAO)) |> 
    plot_peak_day_tfc(.year)
}


#####################################################################################
# fig-change-peak-day


plot_slope_peak_day <- function(.pkday, .base_year = 2019, .current_year = 2024){
  
  tmp <- .pkday |> 
    filter(YEAR %in% c(.base_year, .current_year)) |> 
    mutate(GRP = cur_group_id(), .by = ICAO) |> 
    mutate(YEAR = as.character(YEAR), LEFT = GRP %% 2)
  
  viz <- ggplot(
    data = tmp
    ,aes(x = YEAR, y = PEAK_DAY_PCT)) + 
    #--------- main slope graph
    geom_path(aes(group = ICAO)) + 
    geom_point() +
    #--------- labels on left hand side
    geom_text_repel(data = tmp |> filter(YEAR == .base_year)
                    ,aes(label = ifelse(LEFT == 1, ICAO, NA))
                    ,hjust = "left"
                    ,nudge_x = -0.2
    ) +
    #--------- labels on right hand side
    geom_text_repel(data = tmp |> filter(YEAR == .current_year)
                    ,aes(label = ifelse(LEFT == 0, ICAO, NA))
                    ,hjust = "right"
                    ,nudge_x = +0.2
    ) +
    #--------- beautify
    labs(x = NULL, y = NULL) +
    facet_wrap(.~REGION)
  viz
}


##################################################################################


timeline_peak_day <- function(.years){
  peak_day_comb <- peak_day_comb |> 
    filter(YEAR %in% .years)

  # fix by hand EGKK and LEBL
  fix <- peak_day_comb |> 
    filter(ICAO %in% c("EGKK","LEBL"), YEAR == 2022) |> 
    mutate(YEAR = 2023, PEAK_DAY_PCT = PEAK_DAY_PCT * 1.07)
  
  peak_day_comb <- peak_day_comb |> bind_rows(fix) |> arrange(ICAO, YEAR)
  # ----------------------------- end fix by hand ------------------------  
  
  peak_day_comb <- peak_day_comb |> 
    left_join(bind_rows(bra_apts_names, eur_apts_names))
  
  peak_day_comb_lbls <- peak_day_comb |> 
    # set labels
    mutate(CHECK = case_when(
      # Brazil labels
      ICAO %in% c("SBGR","SBSP","SBBR", "SBSV","SBGL") ~ 2020
      , ICAO %in% c("SBGL","SBCT") ~ 2021
      # Europe labels
      , ICAO %in% c("EHAM", "LEBL", "LIRF") ~ 2019
      , ICAO %in% c("EGLL","LSZH") ~ 2020
      , ICAO %in% c("EGKK") ~ 2021 
      , ICAO %in% c("EDDF") ~ 2022
      # default
      ,.default = 2022
    )
    ) |> 
    filter(CHECK == YEAR)
  
  plot_peakday_timline <- function(.df, .df_lbls = peak_day_comb_lbls){
    pkdaytl <- .df |> 
      
      ggplot() + 
      geom_path(aes(x = YEAR, y = PEAK_DAY_PCT, group = ICAO, color = ICAO)) + 
      geom_text_repel(  data = .df_lbls
                        , aes(x = YEAR, y = PEAK_DAY_PCT
                              , label = (paste0(NAME," (",ICAO,")"))
                              , color = ICAO )
                        , nudge_y = 0.2
                        , hjust = 0
                        , force = 20
                        , size = 3) +
      facet_wrap(. ~ REGION) + 
      scale_y_continuous(limits = c(0,NA)) +
      guides(color = FALSE) +
      labs(x = NULL, y = "peak day traffic")
    
    return(pkdaytl)
  }


  return(peak_day_comb |> plot_peakday_timline())  
}



######################################################################################

## Fleet Mix

fleet_mix_from_counts <- function(.counts, .reg){
  fm <- .counts %>% 
    mutate(YEAR = lubridate::year(DATE)
           ,TOT = HEAVY+MED+LIGHT) %>% 
    group_by(ICAO, YEAR) %>% 
    summarise(
      TOT    = sum(HEAVY) + sum(MED) + sum(LIGHT)
      , H_PERC = sum(HEAVY)/TOT
      , M_PERC = sum(MED) / TOT
      , L_PERC = sum(LIGHT)/TOT
      , .groups = "drop") %>% 
    mutate(REGION = .reg) %>%
    tidyr::pivot_longer(
      cols     = c(H_PERC, M_PERC, L_PERC)
      ,names_to = "WTC"
      ,values_to= "SHARE") %>%
    mutate(WTC = factor(WTC
                        ,levels = c("L_PERC","M_PERC","H_PERC")
                        ,labels = c("Light" ,"Medium","Heavy"))
    )
}


tfc_apts_bra <- read_csv("./data/BRA-airport-tfc.csv", show_col_types = FALSE)
tfc_apts_eur <- read_csv("./data/EUR-airport-tfc.csv", show_col_types = FALSE)

fm_apts_bra <- tfc_apts_bra |> 
  dplyr::rename(HEAVY = H, MED = M, LIGHT = L) |> 
  fleet_mix_from_counts("BRA") 

fm_apts_eur <- tfc_apts_eur |> 
  dplyr::rename(HEAVY = H, MED = M, LIGHT = L) |>
  fleet_mix_from_counts("EUR")

plot_fleet_mix <- function(.fleetmix_airports , .key_year){
  fleet_mix_plot <- 
    ggplot2::ggplot( 
        data    = .fleetmix_airports |> dplyr::filter(YEAR == .key_year)
      , mapping = ggplot2::aes(y = paste(ICAO, NAME), x = SHARE, fill = WTC)) +
    ggplot2::geom_col(position = "stack", width = 0.9)  +
    ggplot2::scale_fill_manual(values = c("#56B4E9", "#009E73","#F0E442")) +
    ggplot2::scale_y_discrete(labels = scales::label_wrap(10)) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    # ggplot2::geom_text(aes(x = 0.05, label = NAME), size = 3, hjust = 0) +
    ggplot2::facet_wrap(.~REGION, scales = "free_y") + 
    #  my_own_theme_minimal +
    ggplot2::theme( 
            legend.position = "top"
           ,legend.title    = ggplot2::element_text(size = 8) 
           ,legend.text     = ggplot2::element_text(size = 8)
           ,legend.key.size = ggplot2::unit(0.3, "cm")) +
    ggplot2::labs(x = NULL, y = NULL)

    return(fleet_mix_plot)
}



##############################################################################################################################

# fleet mix timeline

plot_fleetmix_timeline <- function(.fleetmix){ 
  viz <- .fleetmix |> 
    ggplot(aes(x = YEAR, y = SHARE, group = WTC, color = WTC)) +
    geom_path( ) +
    geom_point() +
    scale_color_manual(values = c("#56B4E9", "#009E73","#F0E442")) +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    facet_wrap(.~paste(ICAO, NAME), ncol = 2) + 
    labs(x = NULL, y = "share [% movements]", color = NULL)
  return(viz)
}

fleet_mix_timeline <- function(.years){
  p_bra <- fm_apts_bra |> 
    filter(YEAR %in% .years)|>
  left_join(bra_apts_names, by = join_by(ICAO)) |> 
    plot_fleetmix_timeline()
  
  # fix for Europe LEBL and EGKK
  fix <- fm_apts_eur |> filter(ICAO %in% c("EGKK","LEBL"), YEAR == 2022) |> 
    mutate(YEAR = 2023)
  fm_apts_eur <- fm_apts_eur |> bind_rows(fix) |> arrange(ICAO, YEAR)
  #---------------------------
  p_eur <- fm_apts_eur |> 
    filter(YEAR %in% .years) |> 
    left_join(eur_apts_names, by = join_by(ICAO)) |> 
    plot_fleetmix_timeline() +
    labs(y = NULL)
  
  p <- p_bra + p_eur + 
    plot_layout(guides = "collect") & 
    theme(legend.position = "top"
          , axis.text = element_text(size = 7)
          ,panel.spacing = unit(0.5, "cm", data = NULL)) & 
    scale_x_continuous(guide = guide_axis(n.dodge = 2))  
  
  return(p)
}



