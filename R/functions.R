#' Cleans the NSSH file.
#' @description
#' This function is used to clean raw data from missing values, and converting
#' to numerical values.
#' @param data raw NSSH data.
#' @return A `data.frame`
#' @export
cleaning_herring <- function(data) {
  data |>
    dplyr::filter(
      weight > 0) |>
    tidyr::drop_na(length, age, weight)
}

#' Count fish per year using unique (id, indno) pairs.
#' @param data A cleaned herring dataset.
#' @return A tibble with columns year and n_ids.
#' @export
count_per_year <- function(data) {
  data |>
    dplyr::summarise(
      n_ids = dplyr::n_distinct(paste(id, indno, sep = "_")),
      .by = year
    ) |>
    dplyr::arrange(year)
}

#' Count total weight (tonnes) of fish per year.
#'
#' @param data Cleaned herring dataset.
#' @return A tibble with columns year and total_weight.
#' @export
weight_per_year <- function(data) {
  data$weight <- suppressWarnings(as.numeric(data$weight))
  data |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      total_weight = sum(weight, na.rm = TRUE)/1e6,
      .groups = "drop"
      )
}

#' Count ages for all years
#' @param data A data frame with at least columns `year` and `age`.
#' @return A tibble with columns: year, age, n
#' @export
age_count_for_year <- function(data) {
  data |>
    dplyr::count(year, age, name = "n") |>
    dplyr::arrange(year, age)
}

#' Makes a summary of max and mean ages for all years.
#' @param data A data frame with at least columns `year` and `age`.
#' @return A tibble with columns: year, age, n
#' @export
age_summary_for_year <- function(data) {
  data |>
    dplyr::summarise(
      n_fish = dplyr::n(),
      mean_age = round(mean(age, na.rm = TRUE), 2),
      max_age  = max(age, na.rm = TRUE),
      .by = year) |>
    dplyr::arrange(year)
}

#' Count number of unique fish IDs per year per location (lat/lon) and adding
#' filter for area of interest.
#'
#' @param data Cleaned herring dataset.
#' @return A tibble with columns year, lon, lat, n_fish and ids.
#' @export
location_catches_summary <- function(data) {
  data |>
    dplyr::summarise(
      n_fish = dplyr::n(),
      mean_age = round(mean(age, na.rm = TRUE), 1),
      mean_weight = round(mean(weight, na.rm = TRUE), 1),
      .by = c(year, month, lon, lat)
    ) |>
    dplyr::filter(
      lon > -30, lon < 30,   # longitude range
      lat > 30,  lat < 80    # latitude range
    )
}
