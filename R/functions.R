#' Function to clean raw data from missing values in columns with biological
#' data.
#' @param data raw fish (NSSH) data.
#' @return A `data.frame`
#' @importFrom rlang .data
#' @export
cleaning_herring <- function(data) {
  data |>
    dplyr::filter(
      .data$weight > 0
    ) |>
    tidyr::drop_na("length", "age", "weight")
}

#' Counts fish per year using unique (id, indno) pairs.
#' @param data A data frame with `year`, `id` and `indno`.
#' @return A tibble with columns year and n_ids.
#' @export
count_per_year <- function(data) {
  data |>
    dplyr::summarise(
      n_ids = dplyr::n_distinct(paste(.data$id, .data$indno, sep = "_")),
      .by = "year"
    ) |>
    dplyr::arrange(.data$year)
}

#' Counts total weight (tonnes) of fish per year.
#' @param data A data frame with `year` and individual `weight`.
#' @return A tibble with columns year and total_weight.
#' @export
weight_per_year <- function(data) {
  data |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      total_weight = sum(.data$weight, na.rm = TRUE)/1e6,
      .groups = "drop"
    )
}

#' Counts number of fish per age for all years.
#' @param data A data frame with at least columns `year` and `age`.
#' @return A tibble with columns: year, age, n
#' @export
age_count_for_year <- function(data) {
  data |>
    dplyr::count(.data$year, .data$age, name = "n") |>
    dplyr::arrange(.data$year, .data$age)
}

#' Makes a summary of max and mean ages for all years.
#' @param data A data frame with at least columns `year` and `age`.
#' @return A tibble with columns: year, age, n
#' @export
age_summary_for_year <- function(data) {
  data |>
    dplyr::summarise(
      n_fish = dplyr::n(),
      mean_age = round(mean(.data$age, na.rm = TRUE), 2),
      max_age  = max(.data$age, na.rm = TRUE),
      .by = "year"
    ) |>
    dplyr::arrange(.data$year)
}

#' Counts number of fish per year per location (lat/lon), and calculates
#' mean age and weight. Additionally adding a filter for area of
#' interest of NSSH.
#' @param data A data frame with at least columns `year`, `month`, `lat`, `lon`, `age` and `weight`.
#' @return A tibble with columns year, month, lat, lon, n_fish, mean_age and mean_weight.
#' @export
location_catches_summary <- function(data) {
  data |>
    dplyr::summarise(
      n_fish = dplyr::n(),
      mean_age = round(mean(.data$age, na.rm = TRUE), 1),
      mean_weight = round(mean(.data$weight, na.rm = TRUE), 1),
      .by = c("year", "month", "lon", "lat")
    ) |>
    dplyr::filter(
      .data$lon > -30, .data$lon < 30,   # longitude range
      .data$lat > 30,  .data$lat < 80    # latitude range
    )
}
