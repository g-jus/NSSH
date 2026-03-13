#' Count number of unique fish IDs per year per location (lat/lon).
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
    )
}

#' Filters out herring data which is within the normal NSSH distribution area.
#'
#' @param data The map ready dataset with colums lat and lon.
#' @return A tibble with same columns as input, filtered for correct lat/lon.
#' @export
filter_ocean_points <- function(data) {
  data |>
    dplyr::filter(
      lon > -30, lon < 30,   # longitude range
      lat > 30,  lat < 80    # latitude range
    )
}
