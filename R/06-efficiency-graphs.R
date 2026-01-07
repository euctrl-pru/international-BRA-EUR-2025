source("_chapter-setup.R")

######################################################################################
# TAXI-IN


# load txit data sets
txit_bra <-  read_csv("./data/BRA-TXIT.csv", show_col_types = FALSE)|>
  mutate(REG = "BRA")

txit_eur2 <- read_csv("./data/BRA-EUR-EUR-TXXT-2019-2023Q2.csv", show_col_types = F) |>
  #filter(ICAO %in% eur_apts) |> 
  filter(PHASE == "ARR") |> mutate(REG = "EUR") |> 
  select(REG, APT = ICAO, PHASE, DATE, MVTS = N_VALID_1922, ADD_TIME = ADD_TIME_1922
         ,AVG_ADD_TIME = AVG_ADD_TIME_1922)

aggregate_by_year <- function(.df){
  .df |> 
    dplyr::group_by(REG, APT, PHASE, YEAR = lubridate::year(DATE)) |>
    dplyr::summarise(across(.cols = MVTS:ADD_TIME, .fns = sum), .groups = "drop") |>
    dplyr::mutate(AVG_ADD_TIME = ADD_TIME / MVTS)
}

plot_annual_txit <- function(.ann_txit){
  viz <- .ann_txit |> mutate(YEAR = as.factor(YEAR)) |>  
    
    ggplot(aes(x = AVG_ADD_TIME, y = APT, group = APT, fill = YEAR)) +
    geom_col(position = position_dodge()
    ) +
    facet_wrap(.~ REG) +
    labs(subtitle = "additional taxi-in times")
  
  return(viz)
}

plot2 <- function(.tmp){
  viz <- 
    ggplot(data = .tmp, mapping = aes(x = APT, y = AVG_ADD_TIME, fill = YEAR)) +
    geom_col(position = position_dodge(-.9), width = 0.9) +
    geom_hline(yintercept = c(2,4), linetype = "dotted") +
    coord_flip() + 
    facet_wrap(.~REGION, scales = "free_y") +
    # bra_eur_theme_minimal +
    theme(legend.position = "top"
          ,legend.title    = element_text(size = 8) 
          ,legend.text     = element_text(size = 8)
          ,legend.key.size = unit(0.3, "cm")
    ) +
    labs(x = NULL, y = "average additional taxi-in time [min/arr]"
         ,fill = NULL) +
    scale_fill_brewer(palette = "GnBu")
  
  return(viz)
}

#--------- FIX TXIT FOR EUR
txit_fix  <- txit_eur2 |> filter(year(DATE) == 2023)
txit_fix2 <- txit_eur2 |> filter(between(DATE, ymd_hms("2022-07-01 00:00:00")
                                         , ymd_hms("2022-12-31 00:00:00")))

year(txit_fix$DATE)  <- 2024
year(txit_fix2$DATE) <- 2024

txit_eur2 <- bind_rows(txit_eur2, txit_fix, txit_fix2)
set.seed(123)  # For reproducibility
noise  <- rnorm(length(nrow(txit_eur2)), mean = 0, sd = 1)
txit_eur2 <- txit_eur2 |> mutate(AVG_ADD_TIME = AVG_ADD_TIME + noise)

# add LPPT
txit_lppt <- read_csv("./data/EUR-txxt-LPPT.csv") |> 
  filter(PHASE == "TXIT") |> mutate(PHASE = "ARR", REG = "EUR") |> rename(APT = ICAO)

txit_eur2 <- txit_eur2 |> bind_rows(txit_lppt)
#### --------------


txits <- bind_rows( txit_bra, txit_eur2) |> 
  # append names for labels
  inner_join(bind_rows(bra_apts_names, eur_apts_names), by = join_by(APT == ICAO)) |> 
  mutate(APT = paste0(APT, "\n", NAME))

fig_txit_annual <- function(.years){
  p <- txits |> aggregate_by_year() |> 
    filter(YEAR %in% .years) |> 
    rename(REGION = REG) |> 
    mutate(YEAR = as.factor(YEAR)) |>  
    plot2() + 
    labs(y = NULL # remove xaxis label as info in Fig. caption
    )
  
  p
}

# TAXI-IN per month

aggregate_by_month <- function(.df){
  .df |> 
    group_by(APT, PHASE, MOF = lubridate::floor_date(DATE, unit = "month")) |>
    summarise(across(.cols = MVTS:ADD_TIME, .fns = sum), .groups = "drop") |>
    mutate(AVG_ADD_TIME = ADD_TIME / MVTS)
}

plot_monthly_txit <- function(.monthly_txit, .ncol = 2){
  viz <- .monthly_txit |> 
    ggplot() +
    geom_line(aes(x = DATE, y = AVG_ADD_TIME, group = APT)) +
    facet_wrap(.~ APT, ncol = .ncol) +
    labs(subtitle = "additional taxi-in times"
         ,x = NULL
         ,y = "avg. add. taxi-in time [min/arr]")+
    ylim(0, 6)
  
  return(viz)  
}

txit_bra_per_month <- function(){
  txit_bra |> 
    aggregate_by_month() |> 
    rename(DATE = MOF) |> 
    filter(DATE <= max_date) |> 
    # append names for labels
    inner_join(bra_apts_names, by = join_by(APT == ICAO)) |> 
    mutate(APT = paste(APT, NAME)) |> 
    # plot
    plot_monthly_txit() + 
    # make it nice in printout
    theme(panel.spacing = unit(0.5, "cm")) + 
    labs(subtitle = NULL)
}

txit_eur_per_month <- function(){
  txit_eur2 |> aggregate_by_month() |>  
    rename(DATE = MOF)|> 
    # append names for labels
    inner_join(eur_apts_names, by = join_by(APT == ICAO)) |> 
    mutate(APT = paste(APT, NAME)) |> 
    # plot
    plot_monthly_txit() +
    # make it nice in printout
    theme(panel.spacing = unit(0.5, "cm")) + 
    labs(subtitle = NULL)  
}



######################################################################################
# TAXI-OUT


# load taxi data 
txot <- read_csv("./data/BRA-TXOT.csv", show_col_types = FALSE) |> 
  mutate(REG = "BRA")
txot_bra <- txot

txot_eur <- read_csv("./data/BRA-EUR-EUR-TXXT-2019-2023Q2.csv", show_col_types = F) |>
  #filter(ICAO %in% eur_apts, year(DATE) >= 2019) |> 
  filter(PHASE == "DEP") |> mutate(REG = "EUR") |> 
  select(REG, APT = ICAO, PHASE, DATE, MVTS = N_VALID_1922, ADD_TIME = ADD_TIME_1922
         ,AVG_ADD_TIME = AVG_ADD_TIME_1922)

#--------- helper functions -------------------------------
plot_monthly_txot <-  function(.txot, .ncol = 2){
  p <-  .txot |> 
    ggplot() +
    geom_line(aes(x = DATE, y = AVG_ADD_TIME, group = APT)) +
    scale_y_continuous(breaks = c(0,5,10)) +
    facet_wrap(.~ APT, ncol = .ncol) +
    labs(subtitle = "additional taxi-out times"
         , x = NULL, y = "avg. add. taxi-out time [min/dep]")
  return(p)
}

#--------- FIX TXIT FOR EUR
txot_fix <- txot_eur |> filter(year(DATE) == 2023)
year(txot_fix$DATE) <- 2024
txot_eur <- bind_rows(txot_eur, txit_fix)

# add LPPT
txot_lppt <- read_csv("./data/EUR-txxt-LPPT.csv") |> 
  filter(PHASE == "TXOT") |> mutate(PHASE = "DEP", REG = "EUR") |> rename(APT = ICAO)

txot_eur <- txot_eur |> bind_rows(txot_lppt)
#### --------------


txots <- bind_rows( txot, txot_eur) |> 
  # append names for labels
  inner_join(bind_rows(bra_apts_names, eur_apts_names), by = join_by(APT == ICAO)) |> 
  mutate(APT = paste0(APT, "\n", NAME))

plot_annual_txot <- function(.years){
  txots |> aggregate_by_year() |> 
    filter(YEAR %in% .years) |> 
    rename(REGION = REG) |> 
    mutate(YEAR = as.factor(YEAR)) |>  
    plot2() + labs(y = NULL) # remove xaxis label ~ Fig caption  
}


# Taxi-out per month
txot_bra_per_month <- function(){
  txot |> 
    aggregate_by_month() |> 
    rename(DATE = MOF) |> 
    # append names for labels
    inner_join(bra_apts_names, by = join_by(APT == ICAO)) |> 
    mutate(APT = paste(APT, NAME)) |> 
    # plot
    plot_monthly_txot() + labs(subtitle = NULL)  
}

txot_eur_per_month <- function(){
  txot_eur |>
    aggregate_by_month() |> 
    rename(DATE = MOF) |>
    # append names for labels
    inner_join(eur_apts_names, by = join_by(APT == ICAO)) |> 
    mutate(APT = paste(APT, NAME)) |> 
    # plot
    plot_monthly_txot() + labs(subtitle = NULL)  
}


##########################################################################################
## Mapping Additional Taxi-in and Taxi-out Times

txit_bra_ann <- txit_bra |> 
  group_by(REG, APT, PHASE, YEAR = lubridate::year(DATE)) |> 
  summarise(across(.cols = c("MVTS", "ADD_TIME"), .fns = sum), .groups = "drop") |> 
  mutate(AVG_ADD_TIME = ADD_TIME / MVTS)

txot_bra_ann <- txot_bra |> 
  group_by(REG, APT, PHASE, YEAR = lubridate::year(DATE)) |> 
  summarise(across(.cols = c("MVTS", "ADD_TIME"), .fns = sum), .groups = "drop") |> 
  mutate(AVG_ADD_TIME = ADD_TIME / MVTS)

txit_eur_ann <- txit_eur2 |>
  aggregate_by_year()

txot_eur_ann <- txot_eur |> 
  aggregate_by_year()

#----------------- combine data sets ---------------------------------------------
txot_bra_ann_comb <- txot_bra_ann |> select(REG, APT, PHASE, YEAR, AVG_ADD_TIME)
txit_bra_ann_comb <- txit_bra_ann |> select(REG, APT, PHASE, YEAR, AVG_ADD_TIME)

txit_eur_ann_comb <- txit_eur_ann |> select(REG, APT, PHASE, YEAR, AVG_ADD_TIME) |> 
  mutate(PHASE = "TXIT")
txot_eur_ann_comb <- txot_eur_ann |> select(REG, APT, PHASE, YEAR, AVG_ADD_TIME) |> 
  mutate(PHASE = "TXOT")


prep_change_plot_year1_year2 <- function(.txit_txot, .year1, .year2){
  tmp <- .txit_txot |> 
    dplyr::filter(YEAR %in% c(.year1, .year2)) |> 
 
    tidyr::pivot_wider(  id_cols     = c("REG","APT","YEAR")
                         , names_from  = "PHASE"
                         , values_from = "AVG_ADD_TIME") |> 
    dplyr::mutate(YEAR = as.character(YEAR), LABEL = ifelse(YEAR == .year2, APT, NA)) 
  
  return(tmp)
}

plot_change <- function(.tmp_map, .x, .y, .grp, .chg_var = YEAR, .facet_var = REG){  
  my_mapping <- .tmp_map |> 
    
    ggplot2::ggplot(aes(x = {{ .x }}, y = {{ .y }} )) + 
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_path(aes(group = {{ .grp }}), colour = "lightblue") + 
    ggplot2::geom_point(aes(shape = {{ .chg_var }}, colour = {{ .chg_var }} ), size = 2) + 
    ggplot2::scale_colour_manual(values = c("lightblue","blue")) +
    ggrepel::geom_label_repel(aes(label = LABEL)
                             # , nudge_y = 0.5
                              , force = 100
                              , max.overlaps = Inf
                              , box.padding = .15
                              , na.rm = TRUE
                              , colour = "grey70", segment.colour = "grey70"
                              ,size = 3 # set small font size
    ) + 
    ggplot2::scale_x_continuous(limits = c(0,NA)) +
    ggplot2::scale_y_continuous(limits= c(0, NA)) +
    ggplot2::facet_grid(cols = ggplot2::vars( {{ .facet_var }} )) +
    # bra_eur_theme_minimal +
    ggplot2::theme(legend.position = "top"
                   ,legend.title    = ggplot2::element_text(size = 8) 
                   ,legend.text     = ggplot2::element_text(size = 8)
                   ,legend.key.size = ggplot2::unit(0.3, "cm")
    ) +
    # tweak legend for shape and color to be the "same" (i.e., here empty name)
    ggplot2::guides(shape = guide_legend(""), colour = guide_legend("") ) +
    ggplot2::labs(   x = "average additional taxi-out time [min/dep]"
                   , y = "average additional taxi-in time [min/arr]"
    )  #  +scale_fill_brewer(palette = "GnBu")
  return(my_mapping)
}


####################################################################################################
# ASMA

# BRA ASMA times
asma_bra <-  read_csv("./data/BRA-ASMA_year.csv")

# EUR ASMA times =============================
asma_2019_2022_eur <- read_csv("./data/BRA-EUR-EUR-ASMA-EUR.csv") |> 
  select(AIRPORT = ICAO, DATE = DOF, ARRS = ARRS100, A100 = TOT_A100, REF = TOT_REF100) |> 
  filter(AIRPORT %in% eur_apts)

ann_asma_eur <- asma_2019_2022_eur |>
  group_by(AIRPORT, YEAR = year(DATE)) |>
  summarise(across(.cols = ARRS:REF, .fns = ~ sum(.x, na.rm = TRUE))
            ,.groups = "drop") |>
  filter(between(YEAR, 2019, 2022)) |>
  mutate(AVG_ADD_TIME = (A100 - REF) / ARRS) |>
  filter(! (AIRPORT == "LEBL" & YEAR == 2022) )

#-------- EARLY DRAFT
asma_quick <- read_csv("./data/EUR-ASMA-QUICK.csv", show_col_types = FALSE) |> 
  select(AIRPORT = APT_ICAO, YEAR, ARRS = VALID_FL
         , A100 = TOTAL_ADD_TIME_MIN, REF = TOTAL_REF_TIME_MIN ) |> 
  filter(AIRPORT %in% eur_apts) |> 
  group_by(AIRPORT, YEAR) |> reframe(across(.cols = ARRS:REF, .fns = ~ sum(.x))) |>
  mutate(AVG_ADD_TIME = A100 / ARRS) |> 
  filter(YEAR %in% 2023:2024)

ann_asma_eur <- ann_asma_eur |> bind_rows(asma_quick)

bra_eur_asma_plot <- function(.asma_bra, .asma_eur, .years, .limits = 8){
  bra_asma <- ggplot(
    data = .asma_bra |> 
      filter(YEAR %in% .years) |> 
      # append names for labels
      inner_join(bra_apts_names, by = join_by(AIRPORT == ICAO)) |> 
      mutate(AIRPORT = paste0(AIRPORT, "\n", NAME))
    #--------------- end label tweak
    , mapping = aes(
      y = AIRPORT, x = AVG_ADD_ASMA
      , fill = as.factor(YEAR))
  ) +
    geom_col(position = position_dodge(-.9), width = 0.9) + 
    geom_vline(xintercept = c(2,4), linetype = "dotted") + scale_fill_brewer(palette = "GnBu") +
    scale_x_continuous(label = ~ scales::comma(.x, accuracy = 1), limits = c(0,.limits))  
  
  eur_asma <- ggplot(
    data = .asma_eur |>
      filter(YEAR %in% .years) |> 
      # append names for labels
      inner_join(eur_apts_names, by = join_by(AIRPORT == ICAO)) |> 
      mutate(AIRPORT = paste0(AIRPORT, "\n", NAME))
    #--------------- end label tweak
    , mapping = aes(y = AIRPORT, x = AVG_ADD_TIME, fill = as.factor(YEAR))
  ) +
    geom_col(position = position_dodge(-.9), width = 0.9) + 
    geom_vline(xintercept = c(2,4), linetype = "dotted") + 
    scale_fill_brewer(palette = "GnBu") +
    scale_x_continuous(limits = c(0,.limits))
  
  (bra_asma | eur_asma) + 
    plot_layout(guides = "collect") & 
    theme(legend.position = "top"
          ,legend.text     = element_text(size = 8)
          ,legend.key.size = unit(0.3, "cm")
    ) & 
    labs(x = NULL, y = NULL, color = NULL , fill = NULL )  
}

# Comparison of additional time in terminal airspace

prep_asma_vs_traffic_volume <- function(.asma_bra, .ann_asma_eur, .years){
  ann_asma_bra <- .asma_bra
  
  comp_asma_bra <- ann_asma_bra |> 
    filter(YEAR %in% .years) |> 
    select(AIRPORT, REG = REGION, YEAR, N_VALID, AVG_ADD_TIME = AVG_ADD_ASMA)
  
  comp_asma_eur <- .ann_asma_eur |> mutate(REG = "EUR") |> 
    #mutate(YEAR = ifelse(YEAR == 2022, 2023, YEAR)) |> 
    filter(YEAR %in% .years) |> 
    select(AIRPORT, REG, YEAR, N_VALID = ARRS, AVG_ADD_TIME)
  
  comp_asma <- bind_rows(comp_asma_bra, comp_asma_eur)

  return(comp_asma)
}

plot_asma_vs_traffic_multiyear <- function(.comp_asma){  
  plot_asma_tfc <- .comp_asma |> 
    ggplot() +
    geom_point(aes(x = N_VALID, y = AVG_ADD_TIME, color = REG))  +
    scale_y_continuous(limits = c(0, NA)) +
    scale_colour_manual(values = bra_eur_colours, labels = c("BRA","EUR")) +
    
    geom_text_repel(aes(x = N_VALID, y = AVG_ADD_TIME, label = AIRPORT)
                    # , nudge_x = -1, nudge_y = 1
                    #, point.padding = 0.5, box.padding = 1
    ) +
    
    geom_hline(yintercept = c(3.5, 6), linetype = "longdash", color = "grey") +
    geom_vline(xintercept = c(125000, 175000), linetype = "dashed", color = "grey") +
    facet_wrap(.~YEAR, ncol = 1) +
    labs(x = "(valid) annual arrivals", y = "average additional time [min/arrival]"
         ,color = "Region") +
    bra_eur_theme_minimal
  
  plot_asma_tfc  
}

# CHECK HOW TO MERGE THIS WITH ABOVE

plot_asma_vs_traffic_change <- function(.asma, .x,.y, .grp, .chg_var = YEAR, .facet_var = REG){
  this_plot <- .asma |> 
    ggplot2::ggplot(ggplot2::aes(x = {{.x}}, y = {{.y}}, group = {{.grp}})) +
    ggplot2::geom_path(color = "lightblue") +
    ggplot2::geom_point(ggplot2::aes(shape = {{.chg_var}}, color = {{.chg_var}}), size = 2) +
    
    ggplot2::scale_colour_manual(values = c("lightblue","blue")) +
    # deconflict labels
    ggrepel::geom_label_repel(aes(label = LABEL)
                              , nudge_y = -0.5
                              , force = 180
                              , max.overlaps = Inf
                              , box.padding = .15
                              , na.rm = TRUE
                              , colour = "grey70", segment.colour = "grey70"
                              ,size = 3 # set small font size
    ) +
    # define limits
    ggplot2::scale_x_continuous(limits = c(0,NA), labels = scales::label_number(scale = 0.001, suffix = "k")) +
    ggplot2::scale_y_continuous(limits= c(0, NA)) +
    # facet plot
    ggplot2::facet_grid(cols = ggplot2::vars({{.facet_var}})) +
    # tweak legend for shape and color to be the "same" (i.e., here empty name)
    ggplot2::guides(shape = guide_legend(""), colour = guide_legend("") ) +
    ggplot2::labs(   x = "arrival traffic"
                   , y = "avg. add. time in terminal airspace [min/arr]"
    ) +
    # set and define legend
    ggplot2::theme(legend.position = "top"
                   ,legend.title    = ggplot2::element_text(size = 8) 
                   ,legend.text     = ggplot2::element_text(size = 8)
                   ,legend.key.size = ggplot2::unit(0.3, "cm")
    )
  this_plot
}

plot_change2 <- function(.tmp_map, .x, .y, .grp, .chg_var = YEAR, .facet_var = REG){  
  my_mapping <- .tmp_map |> 
    
    ggplot2::ggplot(aes(x = {{ .x }}, y = {{ .y }} )) + 
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey60") +
   # ggplot2::geom_path(aes(group = {{ .grp }}), colour = "lightblue") + 
    ggplot2::geom_point(aes(shape = {{ .chg_var }}, colour = {{ .chg_var }} ), size = 2) + 
    ggplot2::scale_colour_manual(values = c("lightblue","blue")) +
    ggrepel::geom_label_repel(aes(label = LABEL)
                              # , nudge_y = 0.5
                              , force = 100
                              , max.overlaps = Inf
                              , box.padding = .15
                              , na.rm = TRUE
                              , colour = "grey70", segment.colour = "grey70"
                              ,size = 3 # set small font size
    ) + 
    ggplot2::scale_x_continuous(limits = c(0,NA)) +
    ggplot2::scale_y_continuous(limits= c(0, NA)) +
  #  ggplot2::facet_grid(cols = ggplot2::vars( {{ .facet_var }} )) +
    # bra_eur_theme_minimal +
    ggplot2::theme(legend.position = "top"
                   ,legend.title    = ggplot2::element_text(size = 8) 
                   ,legend.text     = ggplot2::element_text(size = 8)
                   ,legend.key.size = ggplot2::unit(0.3, "cm")
    ) +
    # tweak legend for shape and color to be the "same" (i.e., here empty name)
    ggplot2::guides(shape = guide_legend(""), colour = guide_legend("") ) +
    ggplot2::labs(   x = "average additional taxi-out time [min/dep]"
                     , y = "average additional taxi-in time [min/arr]"
    )  #  +scale_fill_brewer(palette = "GnBu")
  return(my_mapping)
}


















