save_lateral_yards <- function(s) {
  future::plan("multisession")
  games <- nflreadr::load_schedules(s) |>
    dplyr::filter(!is.na(result))
  
  load <- furrr::future_map_dfr(games$game_id, check_lateral_yards)
  all <-
    dplyr::bind_rows(
      readRDS("data/lateral_yards/multiple_lateral_yards.rds"),
      load |>
        nflfastR::decode_player_ids() |>
        dplyr::group_by(game_id, play_id) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::ungroup()
    ) |> 
    dplyr::distinct()
  
  saveRDS(all, "data/lateral_yards/multiple_lateral_yards.rds")
  readr::write_csv(all, "data/lateral_yards/multiple_lateral_yards.csv")
  closeAllConnections()
}

check_lateral_yards <- function(id){
  season <- substr(id, 1, 4)
  path <- "https://github.com/nflverse/nflfastR-raw/raw/master/raw"
  # raw_data <- nflreadr::rds_from_url(sprintf("%s/%s/%s.rds", path, season, id))
  con <- url(sprintf("%s/%s/%s.rds", path, season, id))
  raw_data <- readRDS(con)
  close(con)
  plays <- janitor::clean_names(raw_data$data$viewer$gameDetail$plays) |>
    dplyr::select(play_id, play_stats)
  stats <- tidyr::unnest(plays, cols = c("play_stats")) |>
    janitor::clean_names() |>
    dplyr::filter(stat_id %in% c(12, 13, 23, 24)) |>
    dplyr::mutate(
      game_id = as.character(id),
      type = dplyr::if_else(stat_id %in% 12:13, "lateral_rushing", "lateral_receiving", missing = NA_character_)
    ) |>
    dplyr::select(
      "game_id",
      "play_id",
      "stat_id",
      "type",
      "yards",
      "team_abbr" = "team_abbreviation",
      "player_name",
      "gsis_player_id"
    )
  
  return(stats)
}

save_lateral_yards(nflreadr:::most_recent_season())
