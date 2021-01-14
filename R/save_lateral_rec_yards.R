`%>%` <- magrittr::`%>%`

save_lateral_rec_yards <- function(s){
  future::plan("multisession")
  games <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
    dplyr::filter(!is.na(result), season == s)

  load <- furrr::future_map_dfr(games$game_id, check_lateral_rec_yards)
  all <- load %>%
    nflfastR::decode_player_ids() %>%
    dplyr::arrange(player_name) %>%
    dplyr::filter(lateral_rec_yards != 0)
  
  saveRDS(all, glue::glue("data/lateral_rec_yards/lateral_receiving_yards_{s}.rds"))
  readr::write_csv(all, glue::glue("data/lateral_rec_yards/lateral_receiving_yards_{s}.csv"))
  closeAllConnections()
}

check_lateral_rec_yards <- function(id){
  # usethis::ui_todo("{id}")
  season <- substr(id, 1, 4)
  path <- "https://github.com/guga31bb/nflfastR-raw/blob/master/raw"
  raw_data <- readRDS(url(glue::glue("{path}/{season}/{id}.rds?raw=true")))
  plays <- janitor::clean_names(raw_data$data$viewer$gameDetail$plays) %>%
    dplyr::select(play_id, play_stats)
  stats <- tidyr::unnest(plays, cols = c("play_stats")) %>%
    janitor::clean_names() %>%
    dplyr::filter(stat_id == 23) %>%
    dplyr::mutate(game_id = as.character(id)) %>%
    dplyr::select(
      "game_id",
      "play_id",
      "stat_id",
      "lateral_rec_yards" = "yards",
      "team_abbr" = "team_abbreviation",
      "player_name",
      "gsis_player_id"
    )
  
  return(stats)
}
