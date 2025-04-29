#' script to plot scope map
#' 
#' 

# load airport
source("_chapter-setup.R")

# get airport LAT/LON --------------------------------------------------------
# our_airports <- readr::read_csv(
#   "https://davidmegginson.github.io/ourairports-data/airports.csv"
#   , show_col_types = FALSE)
# 
# this_airports <- our_airports |> 
#   dplyr::filter(ident %in% c(bra_apts, eur_apts)) |> 
#   dplyr::select(ICAO = ident, LAT = latitude_deg, LON = longitude_deg) |> 
#   dplyr::inner_join( dplyr::bind_rows(bra_apts_names, eur_apts_names)
#                     ,dplyr::join_by(ICAO))
# 
# readr::write_csv(this_airports, "./data/airport-LAT-LON-NAME.csv")

# read-in look-up
this_airports <- readr::read_csv(here::here("data", "airport-LAT-LON-NAME.csv"), show_col_types = FALSE)
this_airports <- this_airports |> dplyr::filter(ICAO %in% c(bra_apts, eur_apts))

worldmap <- ggplot2::borders("world2", colour="lightblue", fill="lightblue")
ggplot2::ggplot() + worldmap + theme_void()

library(ggplot2)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(ggrepel)

world   <- ne_countries(scale = "medium", returnclass = "sf")
bra_map <- world |> dplyr::filter(admin == "Brazil")
#eur_map <- world |> dplyr::filter(admin %in% c("United Kingdom","Netherlands","Germany","Italy","Spain"))
eur_map <- ne_countries(
  country = c("Spain","Portugal","France"
              ,"United Kingdom"
              ,"Germany","Belgium","Netherlands","Luxembourg"
              ,"Austria","Switzerland", "Italy"), scale = "medium")

bra_apts_coord <- this_airports |> 
  filter(grepl(pattern = "^SB", x = ICAO)) |> 
  mutate(NUDGE_X = case_when(
    ICAO %in% c("SBSV") ~ -20
    ,.default = -10)
  )

bra_chart <- ggplot2::ggplot() +
  geom_sf(data = bra_map) +
  geom_point(data = this_airports |> filter(grepl(pattern = "^SB", x = ICAO))
             , aes(x = LON, y = LAT)) +
  geom_label_repel(data = this_airports |> filter(grepl(pattern = "^SB", x = ICAO))
                   , aes(x = LON, y = LAT
                         , label = stringr::str_wrap(paste(ICAO, NAME), 8)
                   )
                   
                   ,position = ggpp::position_nudge_center(x = -2, y = 2,
                                                    center_x = 0, center_y = 0),
                  # label.size = NA,
                   label.padding = 0.2
                   
                   , max.overlaps = Inf
                   # , force = 1
                   # , nudge_x = 5
  ) +
  theme_void()

eur_chart <- ggplot2::ggplot() +
  geom_sf(data = eur_map) + 
  coord_sf( xlim = c(-12, NA)
           ,ylim = c(35, NA)
           , expand = FALSE
           ) +
  geom_label_repel(data = this_airports |> filter(grepl(pattern = "^(E|L)", x = ICAO))
                   , aes(x = LON, y = LAT
                         , label = paste(ICAO, NAME)
                   )
                   , max.overlaps = 0
                   , force = 50
                   # , nudge_x = 5
  ) +
  theme_void()

bra_chart

eur_chart


