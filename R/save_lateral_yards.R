`%>%` <- magrittr::`%>%`

save_lateral_yards <- function(s){
  # future::plan("multisession")
  games <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
    dplyr::filter(!is.na(result), season == s)

  load <- furrr::future_map_dfr(games$game_id, check_lateral_yards)
  all <- load %>%
    nflfastR::decode_player_ids()
  
  saveRDS(all, glue::glue("data/lateral_yards/lateral_yards_{s}.rds"))
  readr::write_csv(all, glue::glue("data/lateral_yards/lateral_yards_{s}.csv"))
  usethis::ui_done("{Sys.time()}: Saved lateral yards for {s}")
  # closeAllConnections()
  # Sys.sleep(150)
}

check_lateral_yards <- function(id){
  # usethis::ui_todo("{id}")
  season <- substr(id, 1, 4)
  path <- "https://github.com/guga31bb/nflfastR-raw/blob/master/raw"
  raw_data <- readRDS(url(glue::glue("{path}/{season}/{id}.rds?raw=true")))
  # raw_data <- readRDS(glue::glue("../nflfastR-raw/raw/{season}/{id}.rds"))
  plays <- janitor::clean_names(raw_data$data$viewer$gameDetail$plays) %>%
    dplyr::select(play_id, play_stats)
  stats <- tidyr::unnest(plays, cols = c("play_stats")) %>%
    janitor::clean_names() %>%
    dplyr::filter(stat_id %in% c(12, 13, 23, 24)) %>%
    dplyr::mutate(
      game_id = as.character(id),
      type = dplyr::if_else(stat_id %in% 12:13, "lateral_rushing", "lateral_receiving", missing = NA_character_)
    ) %>%
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
