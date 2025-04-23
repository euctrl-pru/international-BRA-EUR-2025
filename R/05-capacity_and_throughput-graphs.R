source(here::here("_chapter-setup.R"))

############################################################################
## Peak Declared Capacity
############################################################################

bra_rwys <- tribble(
  ~APT_ICAO, ~ RWY
  , "SBBR", 2
  , "SBGR", 2
  , "SBSP", 2
  , "SBKP", 1
  , "SBRJ", 2
  , "SBGL", 2
  , "SBCF", 1
  , "SBSV", 2
  , "SBPA", 1
  , "SBCT", 2
)

eur_rwys <- tribble(
  ~APT_ICAO, ~ RWY
  , "EGLL", 2
  , "EGKK", 1
  , "EHAM", 6
  , "EDDF", 4
  , "EDDM", 2 
  , "LFPG", 4
  , "LSZH", 3
  , "LEMD", 4
  , "LEBL", 3
  , "LIRF", 4
  , "LPPT", 1
)

# restrict data to study airports
bra_apts <-c("SBBR","SBGR","SBSP","SBKP","SBRJ","SBGL","SBCF","SBSV","SBPA","SBCT")
eur_apts <-c("EHAM","LFPG","EGLL","EDDF","EDDM","LEMD","LPPT","LEBL","EGKK","LSZH") #"LIRF"

bra_cap <- tribble(   
  # CHECK AND VERIFY BRA DATA! # Hugo: Manually updated all airport values referring to DECEA 2021 Report
  ~APT_ICAO, ~YEAR, ~MAX_CAP
  , "SBCT" , 2018 , 24
  , "SBCT" , 2019 , 28
  , "SBCT" , 2020 , 32
  , "SBCT" , 2021 , 32
  , "SBCT" , 2022 , 32
  , "SBPA" , 2018 , 26
  , "SBPA" , 2019 , 30
  , "SBPA" , 2020 , 36
  , "SBPA" , 2021 , 36
  , "SBPA" , 2022 , 36
  , "SBSV" , 2018 , 28
  , "SBSV" , 2019 , 32
  , "SBSV" , 2020 , 36
  , "SBSV" , 2021 , 36
  , "SBSV" , 2022 , 36
  , "SBRJ" , 2018 , 29
  , "SBRJ" , 2019 , 29
  , "SBRJ" , 2020 , 29
  , "SBRJ" , 2021 , 29
  , "SBRJ" , 2022 , 29
  , "SBKP" , 2018 , 31
  , "SBKP" , 2019 , 35
  , "SBKP" , 2020 , 40
  , "SBKP" , 2021 , 40
  , "SBKP" , 2022 , 40
  , "SBCF" , 2018 , 31
  , "SBCF" , 2019 , 35
  , "SBCF" , 2020 , 37
  , "SBCF" , 2021 , 37
  , "SBCF" , 2022 , 37
  , "SBSP" , 2018 , 28
  , "SBSP" , 2019 , 41
  , "SBSP" , 2020 , 42    # Hugo, screenshot ARR +3
  , "SBSP" , 2021 , 44    # Hugo, screenshot ARR +3
  , "SBSP" , 2022 , 44
  , "SBGL" , 2018 , 44
  , "SBGL" , 2019 , 54
  , "SBGL" , 2020 , 60
  , "SBGL" , 2021 , 60
  , "SBGL" , 2022 , 60
  , "SBGR" , 2018 , 47
  , "SBGR" , 2019 , 57
  , "SBGR" , 2020 , 58     # Hugo, screenshot ARR +2
  , "SBGR" , 2021 , 60     # Hugo, screenshot ARR +2
  , "SBGR" , 2022 , 60
  , "SBBR" , 2018 , 52
  , "SBBR" , 2019 , 57
  , "SBBR" , 2020 , 80
  , "SBBR" , 2021 , 80
  , "SBBR" , 2022 , 80
  , "SBRF" , 2018 , 29
  , "SBRF" , 2019 , 34
  , "SBRF" , 2020 , 38
  , "SBRF" , 2021 , 38
  , "SBRF" , 2022 , 38
  , "SBFL" , 2018 , 15
  , "SBFL" , 2019 , 25
  , "SBFL" , 2020 , 26
  , "SBFL" , 2021 , 26
  , "SBFL" , 2022 , 26
)

eur_cap <- tribble(
  ~APT_ICAO, ~YEAR, ~MAX_CAP
  , "EDDF" , 2018 , 100
  , "EDDF" , 2019 , 106
  , "EDDF" , 2020 , 106
  , "EDDF" , 2021 , 106
  , "EDDF" , 2022 , 106
  , "EDDM" , 2018 , 90
  , "EDDM" , 2019 , 90
  , "EDDM" , 2020 , 90
  , "EDDM" , 2021 , 90
  , "EDDM" , 2022 , 90
  , "EGKK" , 2018 , 55
  , "EGKK" , 2019 , 55
  , "EGKK" , 2020 , 55
  , "EGKK" , 2021 , 55
  , "EGKK" , 2022 , 55
  , "EGLL" , 2018 , 88
  , "EGLL" , 2019 , 88
  , "EGLL" , 2020 , 88
  , "EGLL" , 2021 , 88
  , "EGLL" , 2022 , 88
  , "EHAM" , 2018 , 112
  , "EHAM" , 2019 , 112
  , "EHAM" , 2020 , 112
  , "EHAM" , 2021 , 112
  , "EHAM" , 2022 , 112
  , "LEBL" , 2018 , 78
  , "LEBL" , 2019 , 78
  , "LEBL" , 2020 , 78
  , "LEBL" , 2021 , 78
  , "LEBL" , 2022 , 78
  , "LEMD" , 2018 , 100
  , "LEMD" , 2019 , 100
  , "LEMD" , 2020 , 100
  , "LEMD" , 2021 , 100
  , "LEMD" , 2022 , 100
  , "LFPG" , 2018 , 120
  , "LFPG" , 2019 , 120
  , "LFPG" , 2020 , 120
  , "LFPG" , 2021 , 120
  , "LFPG" , 2022 , 120
  , "LIRF" , 2018 , 90
  , "LIRF" , 2019 , 90
  , "LIRF" , 2020 , 90
  , "LIRF" , 2021 , 90
  , "LIRF" , 2022 , 90
  , "LSZH" , 2018 , 66
  , "LSZH" , 2019 , 66
  , "LSZH" , 2020 , 66
  , "LSZH" , 2021 , 66
  , "LSZH" , 2022 , 66
)

# no change to 2022
no_change_cap <- function(.caps, .this_year){
  now_year_cap <- .caps |> 
    filter(YEAR == .this_year - 1) |> 
    mutate(YEAR =  .this_year)
  # append and return
  .caps <- .caps |> bind_rows(now_year_cap)
  return(.caps)
}
bra_cap <- bra_cap |> no_change_cap(2023)
eur_cap <- eur_cap |> no_change_cap(2023)



tmp <- bra_cap %>% 
  add_row(APT_ICAO = "SBGR", YEAR = 2015:2017, MAX_CAP = 47) %>%
  add_row(APT_ICAO = "SBBR", YEAR = 2015:2017, MAX_CAP = 52) %>%
  add_row(APT_ICAO = "SBGL", YEAR = 2015:2017, MAX_CAP = 44) %>%
  add_row(APT_ICAO = "SBSP", YEAR = 2015:2017, MAX_CAP = 28) %>%
  add_row(APT_ICAO = "SBCF", YEAR = 2015:2017, MAX_CAP = 31) %>%
  add_row(APT_ICAO = "SBKP", YEAR = 2015:2017, MAX_CAP = 31) %>%
  add_row(APT_ICAO = "SBRJ", YEAR = 2015:2017, MAX_CAP = 29) %>%
  add_row(APT_ICAO = "SBCT", YEAR = 2015:2017, MAX_CAP = 24) %>%
  add_row(APT_ICAO = "SBRF", YEAR = 2015:2017, MAX_CAP = 29) %>%
  add_row(APT_ICAO = "SBSV", YEAR = 2015:2017, MAX_CAP = 28) %>%
  add_row(APT_ICAO = "SBPA", YEAR = 2015:2017, MAX_CAP = 26) %>%
  add_row(APT_ICAO = "SBFL", YEAR = 2015:2017, MAX_CAP = 15)

eur_cap2 <- eur_cap %>% 
  add_row(APT_ICAO = "EDDF", YEAR = 2015:2017, MAX_CAP = 100) %>%
  add_row(APT_ICAO = "EDDM", YEAR = 2015:2017, MAX_CAP = 90) %>%
  add_row(APT_ICAO = "EGKK", YEAR = 2015:2017, MAX_CAP = 55) %>%
  add_row(APT_ICAO = "EGLL", YEAR = 2015:2017, MAX_CAP = 88) %>%
  add_row(APT_ICAO = "EHAM", YEAR = 2015:2017, MAX_CAP = 112) %>%
  add_row(APT_ICAO = "LEBL", YEAR = 2015:2017, MAX_CAP = 78) %>%
  add_row(APT_ICAO = "LEMD", YEAR = 2015:2017, MAX_CAP = 100) %>%
  add_row(APT_ICAO = "LFPG", YEAR = 2015:2017, MAX_CAP = 120) %>%
  add_row(APT_ICAO = "LIRF", YEAR = 2015:2017, MAX_CAP = 90) %>%
  add_row(APT_ICAO = "LSZH", YEAR = 2015:2017, MAX_CAP = 66)

plot_cap_panel <- function(.df, .ncol = 2){
  g <- ggplot() + 
    geom_line(data = .df, mapping = aes(x = YEAR, y = MAX_CAP)) + 
    facet_wrap(.~APT_ICAO, ncol = .ncol) +
    bra_eur_theme_minimal +
    theme(axis.text.x   = element_text(size = 7)
          ,panel.spacing = unit(1, "lines")   # spacing between facets
    ) +
    labs(x = NULL, y = "declared capacity [movements per hour]")
  return(g)
}

peak_cap_plot <- function(lb, ub, lab_size) {
  tmp |> 
    filter(YEAR >= lb & YEAR <= ub, APT_ICAO %in% bra_apts) |> 
    inner_join(bra_apts_names, by = join_by(APT_ICAO == ICAO)) |> 
    mutate(APT_ICAO = paste(APT_ICAO, NAME)) |> 
    plot_cap_panel() +
    theme(axis.text.x = element_text(size = lab_size, angle = 90, vjust = 0.5))
}

peak_cap_plot_eur <- function(lb, ub, lab_size) {
  eur_cap2 |> 
    filter(YEAR >= lb & YEAR <= ub) |> 
    inner_join(eur_apts_names, by = join_by(APT_ICAO == ICAO)) |> 
    mutate(APT_ICAO = paste(APT_ICAO, NAME)) |> 
    plot_cap_panel() +
    theme(axis.text.x = element_text(size = lab_size, angle = 90, vjust = 0.5))
}



############################################################################
## Max Cap x Num RWY
############################################################################

generate_capacity_plot <- function(this_year) {
  # Define temas customizados
  bra_eur_theme_minimal <- 
    theme_minimal() + 
    theme(axis.title = element_text(size = 9))
  bra_eur_theme_bw <- 
    theme_bw() + 
    theme(axis.title = element_text(size = 9))
  
  # Filtra e organiza os dados
  cap <- bind_rows(
    bra_cap %>% mutate(REGION = "BRA") %>% filter(APT_ICAO %in% bra_apts),
    eur_cap %>% mutate(REGION = "EUR")
  ) %>%
    filter(YEAR == this_year)
  
  # Adiciona informações de pistas
  cap_rwys <- bind_rows(bra_rwys, eur_rwys) |> 
    mutate(YEAR = this_year)
  
  cap <- cap  |> 
    inner_join(cap_rwys, by = c("APT_ICAO", "YEAR")) |>
    inner_join(bind_rows(bra_apts_names, eur_apts_names),
               by = join_by(APT_ICAO == ICAO))
  
  # Gera o gráfico
  plot <- cap |> 
    ggplot(aes(x = MAX_CAP, y = reorder(NAME, MAX_CAP))) + 
    geom_col(aes(fill = REGION)) +
    scale_fill_manual(values = bra_eur_colours) + 
    geom_text(aes(x = 0, label = APT_ICAO),
              hjust = 0, color = "white", size = 3) +
    facet_grid(RWY ~ ., as.table = FALSE, switch = "y", scales = "free", space = "free") +
    bra_eur_theme_bw +
    labs(
      y = NULL, fill = "Region"
    ) +
    theme(
      legend.position = c(0.9, 0.1),
      axis.ticks = element_blank()
    )
  
  return(plot)
}


############################################################################
## Peak Arrival Throughput
############################################################################

# load throughput data
bra_thru <- read_csv("./data/BRA-THRU-analytic.csv", show_col_types = FALSE)
eur_thru <- read_csv("./data/EUR-THRU-analytic.csv", show_col_types = FALSE)

bra_arr_thru <- bra_thru |> 
  select(ICAO, BIN, ARRS) |> 
  group_by(ICAO, YEAR = year(BIN)) |> 
  summarise(PK_THRU = quantile(ARRS, p = 0.95), .groups = "drop") |> 
  mutate(REG = "BRA")

eur_arr_thru <- eur_thru |> 
  rename(ARRS = ARR_THRU) |> 
  select(ICAO, BIN, ARRS) |> 
  filter(between(year(BIN), 2019,2023)) |> 
  mutate(BIN2 = floor_date(BIN, unit = "hour")) |> 
  group_by(ICAO, BIN2) |> 
  summarise(ARRS = sum(ARRS, na.rm = TRUE), .groups = "drop") |> 
  group_by(ICAO, YEAR = year(BIN2)) |> 
  summarise(PK_THRU = quantile(ARRS, p =0.95), .groups = "drop") |> 
  drop_na() |> 
  mutate(REG = "EUR")

plot_timeline <- function(.df, y_legend){
  tmp <- .df
  # Extract unique organizations and assign consistent colors
  icao_colors <- tmp |> 
    distinct(ICAO, REG) |> 
    group_by(REG) |> 
    mutate(COLOR = row_number() |> as.factor())
  
  tmp <- tmp |> left_join(icao_colors)
  
  label_data <- bind_rows(
    tmp |> 
      group_by(ICAO, REG) |> 
      slice_head(n =1)  |> 
      mutate(label_position = "start") |> 
      ungroup() |> 
      group_by(REG) |>  
      filter(row_number() %% 2 != 0) |> 
      ungroup() , 
    tmp |> 
      group_by(ICAO, REG) |> 
      slice_tail(n =1)  |> 
      mutate(label_position = "end") |> 
      ungroup() |> 
      group_by(REG) |>  
      filter(row_number() %% 2 == 0) |> 
      ungroup()
  )
  label_data <- label_data %>%
    mutate(vjust = ifelse(label_position == "start", 0.3, 0.3))
  
  vis <- tmp |> 
    ggplot() +
    geom_line(aes(x = YEAR, y = PK_THRU, group = ICAO, color = COLOR)) +
    geom_label_repel(
      data = label_data, aes(x = YEAR, y = PK_THRU, color = COLOR
                             ,label = ICAO
                             , vjust = vjust)
      ,box.padding = 0.2, point.padding = 0.1
      ,max.overlaps = Inf
      ,direction = "y"
    ) +
    scale_color_brewer(palette = "Dark2") +
    facet_wrap(. ~ REG) + 
    labs(x = NULL, y = y_legend) +
    guides(color = FALSE)
  
  return(vis)
}

tmp_thru <- bind_rows(bra_arr_thru, eur_arr_thru)

arrival_tp_plot <- function(.years){
  tmp_thru <- tmp_thru |> filter(YEAR%in% .years)
  p <- tmp_thru |> plot_timeline("peak arrival throughput")
  return(p)
}



############################################################################
## Peak Departure Throughput
############################################################################


bra_dep_thru <- bra_thru |> 
  select(ICAO, BIN, DEPS) |> 
  group_by(ICAO, YEAR = year(BIN)) |> 
  summarise(PK_THRU = quantile(DEPS, p = 0.95), .groups = "drop") |> 
  mutate(REG = "BRA")

eur_dep_thru <- eur_thru |> rename(DEPS = DEP_THRU) |> 
  select(ICAO, BIN, DEPS) |> 
  # filter(between(year(BIN), 2019,2022)) |> 
  mutate(BIN2 = floor_date(BIN, unit = "hour")) |> 
  group_by(ICAO, BIN2) |> summarise(DEPS = sum(DEPS, na.rm = TRUE), .groups = "drop") |> 
  group_by(ICAO, YEAR = year(BIN2)) |> summarise(PK_THRU = quantile(DEPS, p =0.95), .groups = "drop") |> drop_na() |> 
  mutate(REG = "EUR")

departure_tp_plot <- function(.years){
  tmp_thru <- bind_rows(bra_dep_thru, eur_dep_thru) |> 
    filter(YEAR %in% .years)
  p <- tmp_thru |> plot_timeline("peak departure throughput")  
  
  return(p)
  }


############################################################################
## Declared Capacity and Peak Throughput
############################################################################

cap_peak_tp_plot <- function(key_year){
  thru_arr <- bind_rows(bra_arr_thru, eur_arr_thru)
  
  kawusi <- thru_arr |> 
    rename(AIRPORT = ICAO, REGION = REG
           ,PEAK_ARR_THRU = PK_THRU)
  
  thru_arr_peak <- kawusi %>%  # taken from above arrival throughput
    select(AIRPORT, YEAR, PEAK_ARR_THRU, REGION) %>%
    filter(YEAR == key_year)
  ##################################
  # proxy - max arr capacity = MAX CAP/2
  ##################################
  cap_arr  <- bind_rows(
    bra_cap |> mutate(REGION = "BRA")
    ,eur_cap |> mutate(REGION = "EUR")
  )  |> 
    select(AIRPORT = APT_ICAO, YEAR, MAX_CAP, REGION) %>%
    mutate(ARR_CAP = ceiling(MAX_CAP/2)) |> 
    filter(YEAR == key_year)
  
  tmp <- cap_arr %>% 
    inner_join(thru_arr_peak, by = c("AIRPORT", "YEAR", "REGION")) %>% 
    mutate(DIFF = case_when(PEAK_ARR_THRU < ARR_CAP ~ "Capacity > Throughput"
                            ,TRUE ~ "Throughput >= Capacity")) |> 
    # append labels
    inner_join(bind_rows(bra_apts_names, eur_apts_names), by = join_by(AIRPORT == ICAO)) |> 
    mutate(AIRPORT = paste(AIRPORT, NAME))
  
  
  #### https://www.w3schools.com/colors
  # complementary colours
  comp_colours <- c("#98CA32", "#B8143A")   # red, green
  comp_colours <- c("#AE0D7A", "#A2B814")   # purple, green
  # compound colous
  comp_colours <- c("#B2D732","#347B98")    # ligher and dark green
  comp_colours <- c("#347B98","#B2D732")    # ligher and dark green
  # ----------------------------- end color coding
  
  p1 <- tmp %>%
    ggplot() + 
    geom_segment(
      mapping = aes( x = reorder(AIRPORT, ARR_CAP), xend = reorder(AIRPORT, ARR_CAP)
                     ,y = ARR_CAP, yend = PEAK_ARR_THRU
                     ,colour = DIFF), size = 1) + 
    scale_colour_manual(values = comp_colours) +
    geom_point(mapping = aes(x = reorder(AIRPORT, ARR_CAP), y = ARR_CAP), size = 4, colour = comp_colours[1]) + 
    geom_point(mapping = aes(x = reorder(AIRPORT, ARR_CAP), y = PEAK_ARR_THRU), size = 3
               , shape = 24, colour = comp_colours[2], fill=comp_colours[2]) +
    #facet_grid(.~REGION, scales = "free_x") +
    bra_eur_theme_minimal +
    theme(legend.position = c(0.2, 0.9)) +
    labs(x = NULL, y="movements per hour", colour = NULL) +
    coord_flip()  
  
  return(p1)
}


############################################################################
## Bli Pli
############################################################################

# load data
bli_pli_bra <- read_csv("./data/BRA-BLI-PLI.csv") |> mutate(REG = "BRA")
bli_pli_eur <- read_csv("./data/EUR-BLI-PLI.csv") |> mutate(REG = "EUR")


bli_pli_plot <- function(.years){
  bli_pli <- bind_rows(bli_pli_bra, bli_pli_eur) |> 
    filter(YEAR %in% .years) |> mutate(YEAR = as.factor(YEAR))
  
  p <- bli_pli |> 
    mutate(LABEL = paste(ICAO, YEAR)) |> 
    ggplot(aes(x = BLI, y = PLI, color = YEAR)) + 
    geom_point() + 
    geom_text_repel(
      aes(label = LABEL )
      ,max.overlaps = 5 #Inf
      ,vjust = .5
      , force_pull = 10
      ,size = 3
    ) +
    scale_color_brewer(palette = "Dark2") +
    facet_wrap(. ~ REG) +
    theme(legend.position = "top") +
    stat_smooth(aes(group = REG),method = "lm", formula = y ~ I(x^5), size = 0.5, linetype = "dashed", color = "grey50" , se = FALSE) +
    geom_hline(yintercept = 0.3, color = "grey80") +
    geom_vline(xintercept = 0.75, color= "grey80") +
    labs(color = NULL) +
    theme(panel.spacing.x = unit(1, "cm", data = NULL)) 
  
  return(p)
}



























