`%>%` <- magrittr::`%>%`

load_week_ngs <- function(season, week, type, session) {
  # for testing
  # season <- 2020
  # week <- 5
  # type <- "passing"
  # session <- rvest::html_session("https://nextgenstats.nfl.com/stats/top-plays/fastest-ball-carriers")
  
  if (week == 0) {
    usethis::ui_todo("Loading {season} overall {type} stats...")
  } else {
    usethis::ui_todo("Loading {season} week {week} {type} stats...")
  }

  if (week == 0) {
    path <- glue::glue("https://appapi.ngs.nfl.com/statboard/{type}?season={season}&seasonType=REG")
  } else if (week %in% 1:17) {
    path <- glue::glue("https://appapi.ngs.nfl.com/statboard/{type}?season={season}&seasonType=REG&week={week}")
  } else if (week %in% 18:22) {
    path <- glue::glue("https://appapi.ngs.nfl.com/statboard/{type}?season={season}&seasonType=POST&week={week}")
  }

  response <- httr::POST(
    url = path,
    session$config,
    httr::config(referer = session$url),
    httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.86 Safari/537.36"),
    handle = session$handle
  ) %>%
    httr::content()

  info <- response %>%
    purrr::discard(is.list) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::mutate(week = week)

  teams <- nflfastR::teams_colors_logos %>%
    dplyr::select(team_id, team_abbr) %>%
    dplyr::filter(!team_abbr %in% c("LA", "OAK", "STL", "SD"))
  
  if (!is.null(response %>% purrr::pluck("stats"))) {
    stats <- response %>%
      purrr::pluck("stats") %>%
      purrr::map_dfr(function(x) {
        st <- x %>%
          purrr::discard(is.list) %>%
          tibble::as_tibble_row()
        pl <- x %>% purrr::pluck("player")
        pl <- pl %>% purrr::set_names(paste0("player_", names(pl)))
        return(dplyr::bind_cols(st, pl))
      }) %>%
      janitor::clean_names() %>%
      dplyr::rename(
        team_id = dplyr::first(tidyselect::contains("team_id"))
      ) %>%
      dplyr::left_join(teams, by = "team_id") %>%
      dplyr::select(-tidyselect::any_of(c("season", "season_type", "week")))
    
    out <- dplyr::bind_cols(info, stats) %>%
      dplyr::select(
        tidyselect::any_of(c("season", "season_type", "week")),
        tidyselect::any_of(c("player_display_name", "player_position", "team_abbr")),
        tidyselect::any_of(get(paste0(type, "_stats"))),
        tidyselect::any_of(player_info)
      ) %>%
      dplyr::select(-tidyselect::any_of(c(
        "player_nfl_id", "player_esb_id", "player_current_team_id",
        "player_season", "player_football_name", "player_headshot",
        "player_gsis_it_id", "player_ngs_position"
      )))
  } else {
    out <- tibble::tibble()
  }

  return(out)
}

save_ngs_data <- function(seasons) {
  
  if (!all(seasons %in% 2016:most_recent_season())) {
    usethis::ui_stop("Please pass valid seasons between 2016 and {most_recent}")
  }
  
  session <- rvest::html_session("https://nextgenstats.nfl.com/stats/top-plays/fastest-ball-carriers")
  
  todo <- expand.grid(season = seasons, type = c("passing", "rushing", "receiving"))
  
  purrr::walk2(todo$season, todo$type, save_ngs_type, session)
  
  usethis::ui_done("{usethis::ui_field('DONE')}")

}

save_ngs_type <- function(season, type, session) {
  if (!type %in% c("passing", "rushing", "receiving")) usethis::ui_stop("Please pass valid type!")
  # overall <- load_week_ngs(season, 0, type, session)
  # weekly <- purrr::map2_dfr(season, 1:22, load_week_ngs, type, session)
  ngs <- purrr::map2_dfr(season, 0:22, load_week_ngs, type, session)
  
  # saveRDS(overall, glue::glue("data/ngs/ngs_{season}_{type}_overall.rds"))
  # saveRDS(weekly, glue::glue("data/ngs/ngs_{season}_{type}_weekly.rds"))
  # readr::write_csv(overall, glue::glue("data/ngs/ngs_{season}_{type}_overall.csv.gz"))
  # readr::write_csv(weekly, glue::glue("data/ngs/ngs_{season}_{type}_weekly.csv.gz"))
  saveRDS(ngs, glue::glue("data/ngs/ngs_{season}_{type}.rds"))
  readr::write_csv(ngs, glue::glue("data/ngs/ngs_{season}_{type}.csv.gz"))
}

most_recent_season <- function() {
  dplyr::if_else(
    lubridate::month(lubridate::today("America/New_York")) >= 9,
    lubridate::year(lubridate::today("America/New_York")),
    lubridate::year(lubridate::today("America/New_York")) - 1
  )
}

passing_stats <- c(
  "avg_time_to_throw",
  "avg_completed_air_yards",
  "avg_intended_air_yards",
  "avg_air_yards_differential",
  "aggressiveness",
  "max_completed_air_distance",
  "avg_air_yards_to_sticks",
  "attempts",
  "pass_yards",
  "pass_touchdowns",
  "interceptions",
  "passer_rating",
  "completions",
  "completion_percentage",
  "expected_completion_percentage",
  "completion_percentage_above_expectation",
  "avg_air_distance",
  "max_air_distance"
)

rushing_stats <- c(
  "efficiency", 
  "percent_attempts_gte_eight_defenders",
  "avg_time_to_los",
  "rush_attempts",
  "rush_yards",
  "expected_rush_yards",
  "rush_yards_over_expected",
  "avg_rush_yards",
  "rush_yards_over_expected_per_att",
  "rush_pct_over_expected",
  "rush_touchdowns"
)

receiving_stats <- c(
  "avg_cushion",
  "avg_separation",
  "avg_intended_air_yards",
  "percent_share_of_intended_air_yards",
  "receptions",
  "targets",
  "catch_percentage",
  "yards",
  "rec_touchdowns",
  "avg_yac",
  "avg_expected_yac",
  "avg_yac_above_expectation"
)

player_info <- c(
  # "player_nfl_id",
  # "player_esb_id",
  "player_gsis_id",
  "player_first_name",
  "player_last_name",
  # "player_position_group",
  # "player_current_team_id",
  "player_jersey_number",
  # "player_status",
  # "player_season",
  # "player_football_name",
  # "player_headshot",
  # "player_gsis_it_id",
  # "player_ngs_position",
  # "player_ngs_position_group",
  # "player_suffix",
  "player_short_name"
)
