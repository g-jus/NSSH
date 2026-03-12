count_per_year <- function(data) {
  data |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      n_ids = dplyr::n_distinct(id, indno),
      .groups = "drop"
      )
}

weight_per_year <- function(data) {
  data$weight <- suppressWarnings(as.numeric(data$weight))
  data |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      total_weight = sum(weight, na.rm = TRUE)/1e6,
      .groups = "drop"
      )
}

age_count_for_year <- function(data, year_value) {
  data |>
    dplyr::filter(year == year_value) |>
    dplyr::count(age, name = "n") |>
    dplyr::arrange(age)
}

location_catches_summary <- function(data) {
  data |>
    dplyr::group_by(year, lon, lat) |>
    dplyr::summarise(
      n_fish = dplyr::n(),      # how many fish at this point
      ids = paste(unique(id), collapse = ", "),  # optional metadata about IDs
      .groups = "drop"
    )
}

filter_ocean_points <- function(data) {
  data |>
    dplyr::filter(
      lon > -30, lon < 30,   # longitude range
      lat > 30,  lat < 80    # latitude range
    )
}


