# read in all apt traffic counts, c.f. apdf-to-pbwg

pth_eur <- here::here("data-prep-eur")

eur_all_apt_tfc <- list.files(pth_eur, pattern = "apt-tfc-[EL]{1}", full.names = TRUE) |> 
  purrr::map(.f = ~ readr::read_csv(.x, show_col_types = FALSE)) |> 
  dplyr::bind_rows() |> 
  dplyr::filter(ICAO %in% eur_apts)    # removes LTFM for BRA-EUR

eur_apt_monthly_tfc <- eur_all_apt_tfc |> 
  dplyr::mutate(YEAR = lubridate::year(DATE), MONTH = lubridate::month(DATE)) |> 
  dplyr::group_by(ICAO, YEAR, MONTH) |> 
  dplyr::reframe(TOT_FLTS = sum(ARRS) + sum(DEPS)
                 ,N = dplyr::n()    # check days
                 )

eur_apt_monthly_tfc |> readr::write_csv(here::here("data", "EUR-study-apt-lvl-month.csv"))
