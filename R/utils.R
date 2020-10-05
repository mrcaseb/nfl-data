save_empty_stuff <- function(season, type) {
  saveRDS(tibble::tibble(), glue::glue("data/ngs/ngs_{season}_{type}.rds"))
  readr::write_csv(tibble::tibble(), glue::glue("data/ngs/ngs_{season}_{type}.csv.gz"))
}
todo <- expand.grid(season = 2016:2020, type = c("passing", "rushing", "receiving"))
purrr::walk2(todo$season, todo$type, save_empty_stuff)