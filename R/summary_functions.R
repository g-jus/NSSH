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

#' Count number of fish at each age per year.
#'
#' @param data Cleaned herring dataset.
#' @return A tibble with columns age and n.
#' @export
age_count_for_year <- function(data, year_value) {
  data |>
    dplyr::filter(year == year_value) |>
    dplyr::count(age, name = "n") |>
    dplyr::arrange(age)
}




