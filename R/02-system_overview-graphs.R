source("_chapter-setup.R")

# set colors
bra_col <- getElement(bra_eur_colours, "BRA")
eur_col <- getElement(bra_eur_colours, "EUR")

##################################################################################################

plot_cumulative_percentage_by_apt_rank <- function(){
  cols <- c(
    "sg_empresa_icao",
    "nm_mes_partida_real",
    "nr_mes_partida_real",
    "sg_icao_origem",
    "sg_icao_destino",
    "lt_combustivel",
    "sg_equipamento_icao",
    "nm_pais_origem",
    "nm_pais_destino",
    NULL
  )
  
  flights_ANAC_2024 <- read_csv("./data_proc/flights_ANAC_2024.csv", col_select = cols) |> 
    rename(
      ADEP = sg_icao_origem,
      ADES = sg_icao_destino,
      FUEL_BURN = lt_combustivel,
      TYPE = sg_equipamento_icao,
      PAIS_ORIGEM = nm_pais_origem,
      PAIS_DESTINO = nm_pais_destino,
      NULL
    )
  
  adep_counts <- flights_ANAC_2024 |> 
    group_by(ADEP, PAIS_ORIGEM) |>  # Group by ADEP and PAIS_ORIGEM
    summarise(count = n(), .groups = "drop") |>  
    arrange(desc(count))  
  
  ades_counts <- flights_ANAC_2024 |> 
    group_by(ADES, PAIS_DESTINO) |>  # # Group by ADES and PAIS_ORIGEM
    summarise(count = n(), .groups = "drop") |>  
    arrange(desc(count))  
  
  adep_counts_brasil <- adep_counts |> 
    filter(PAIS_ORIGEM == "BRASIL")
  
  ades_counts_brasil <- ades_counts |> 
    filter(PAIS_DESTINO == "BRASIL")
  
  
  # Merge the ADEP and ADES tables into a single table #### appears 2x airports due one is ADEP and other is ADES
  combined_counts <- bind_rows(
    adep_counts_brasil |> rename(airport = ADEP) |> select(airport, count),
    ades_counts_brasil |> rename(airport = ADES) |> select(airport, count)
  )
  
  # Group by airport and sum counts
  summed_counts <- combined_counts |> 
    group_by(airport) |> 
    summarise(total_count = sum(count, na.rm = TRUE)) |> 
    arrange(desc(total_count))
  
  "Prepare the data with cumulative sum and percentages"
  flights_data <- summed_counts |> 
    arrange(desc(total_count)) |>  
    mutate(
      cumulative_percent = cumsum(total_count) / sum(total_count) * 100,  
      rank = row_number()  
    )
  
  "Start the rank with 0 airport and 0 percent - plot will begin in 0,0"
  flights_data_with_zero <- flights_data |> 
    bind_rows(data.frame(rank = 0, cumulative_percent = 0)) |>  # Adicionar o ponto inicial
    arrange(rank)
  
  # Points that I want: x=10 (top 10 airports), y= 80, 90, 95 and 99%
  point_x10 <- flights_data_with_zero |> filter(rank == 10)  # Ponto no eixo X = 10
  point_y80 <- flights_data_with_zero |> filter(cumulative_percent >= 80) |> slice(1)  # Primeiro ponto no eixo Y >= 80
  point_y90 <- flights_data_with_zero |> filter(cumulative_percent >= 90) |> slice(1)  # Primeiro ponto no eixo Y >= 90
  point_y95 <- flights_data_with_zero |> filter(cumulative_percent >= 95) |> slice(1)  # Primeiro ponto no eixo Y >= 95
  point_y99 <- flights_data_with_zero |> filter(cumulative_percent >= 99) |> slice(1)  # Primeiro ponto no eixo Y >= 99
  
  # Max Rank - total number of airports
  max_rank <- max(flights_data_with_zero$rank, na.rm = TRUE)
  
  p_cumsum_bra <- ggplot(flights_data_with_zero, aes(x = rank, y = cumulative_percent)) +
    # cum line
    geom_path(color = "blue") +
    # cont and fixed lines - x and y
    geom_hline(yintercept = 0, color = "black", size = 0.5) +  # Linha no eixo X (y = 0)
    geom_vline(xintercept = 0, color = "black", size = 0.5) +  # Linha no eixo Y (x = 0)
    # Dotted lines limited to the points (x,y)
    geom_segment(aes(x = point_x10$rank, xend = point_x10$rank, y = 0, yend = point_x10$cumulative_percent), 
                 linetype = "dotted", color = "gray40", size = 0.8) +
    geom_segment(aes(x = 1, xend = point_y80$rank, y = 80, yend = 80), linetype = "dotted", color = "gray40", size = 0.8) +
    geom_segment(aes(x = point_y80$rank, xend = point_y80$rank, y = 0, yend = 80), linetype = "dotted", color = "gray40", size = 0.8) +
    geom_segment(aes(x = 1, xend = point_y90$rank, y = 90, yend = 90), linetype = "dotted", color = "gray40", size = 0.8) +
    geom_segment(aes(x = point_y90$rank, xend = point_y90$rank, y = 0, yend = 90), linetype = "dotted", color = "gray40", size = 0.8) +
    geom_segment(aes(x = 1, xend = point_y95$rank, y = 95, yend = 95), linetype = "dotted", color = "gray40", size = 0.8) +
    geom_segment(aes(x = point_y95$rank, xend = point_y95$rank, y = 0, yend = 95), linetype = "dotted", color = "gray40", size = 0.8) +
    geom_segment(aes(x = 1, xend = point_y99$rank, y = 99, yend = 99), linetype = "dotted", color = "gray40", size = 0.8) +
    geom_segment(aes(x = point_y99$rank, xend = point_y99$rank, y = 0, yend = 99), linetype = "dotted", color = "gray40", size = 0.8) +
    # Highlighting points
    geom_point(data = point_x10, aes(x = rank, y = cumulative_percent), color = "orange", size = 3) +
    geom_point(data = point_y80, aes(x = rank, y = cumulative_percent), color = "blue", size = 3) +
    geom_point(data = point_y90, aes(x = rank, y = cumulative_percent), color = "blue", size = 3) +
    geom_point(data = point_y95, aes(x = rank, y = cumulative_percent), color = "blue", size = 3) +
    geom_point(data = point_y99, aes(x = rank, y = cumulative_percent), color = "blue", size = 3) +
    # Labels
    geom_label(aes(x = point_x10$rank, y = point_x10$cumulative_percent, label = "TOP 10"), 
               fill = "orange", color = "white", fontface = "bold", size = 4) +
    geom_label(aes(x = point_y80$rank, y = point_y80$cumulative_percent, label = "80%"), 
               fill = "blue", color = "white", fontface = "bold", size = 4) +
    geom_label(aes(x = point_y90$rank, y = point_y90$cumulative_percent, label = "90%"), 
               fill = "blue", color = "white", fontface = "bold", size = 4) +
    geom_label(aes(x = point_y95$rank, y = point_y95$cumulative_percent, label = "95%"), 
               fill = "blue", color = "white", fontface = "bold", size = 4) +
    geom_label(aes(x = point_y99$rank, y = point_y99$cumulative_percent, label = "99%"), 
               fill = "blue", color = "white", fontface = "bold", size = 4) +
    # Axis customization
    scale_x_continuous(
      limits = c(0, max_rank),
      breaks = c(10, 150, point_y80$rank, point_y90$rank, point_y95$rank, point_y99$rank, max_rank),
      labels = c(10, 150, point_y80$rank, point_y90$rank, point_y95$rank, point_y99$rank, max_rank)
    ) +
    scale_y_continuous(
      breaks = c(0, 20, 50, 80, 90, 95, 100, point_x10$cumulative_percent),
      labels = c(0, 20, 50, 80, 90, 95, 100, round(point_x10$cumulative_percent, 2))
    ) +
    labs(
      x = "Number of airports - Rank",
      y = "Cumulative Percentage (%)",
      title = "Cumulative Percentage by Airport Rank"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12),  # Increase the size of the numbers on axis
      axis.title = element_text(size = 12)  # Increase the size of the axis titles
    )
  
  p_cumsum_bra # Graph visualization
}
