#' Process NSSH Data for Shiny App
#'
#' Loads raw data and applies all processing steps
#'
#' @return A list with processed datasets
#'
#' @keywords internal
#' @import dplyr
process_nssh_data <- function() {
  # Load raw data
  herring_raw <- herring_read()

  # Apply cleaning
  clean_herring <- cleaning_herring(herring_raw)

  # Create derived datasets
  growth_data_small <- clean_herring |>
    dplyr::sample_n(min(5000, nrow(clean_herring)), .seed = 123)

  max_age <- max(clean_herring$age, na.rm = TRUE)

  counts_per_year <- count_per_year(clean_herring)

  weights_per_year <- weight_per_year(clean_herring)

  age_counts <- age_count_for_year(clean_herring)

  age_summary <- age_summary_for_year(clean_herring)

  map_summary <- location_catches_summary(clean_herring)

  # Return as list
  list(
    clean_herring = clean_herring,
    growth_data_small = growth_data_small,
    max_age = max_age,
    counts_per_year = counts_per_year,
    weights_per_year = weights_per_year,
    age_counts = age_counts,
    age_summary = age_summary,
    map_summary = map_summary
  )
}
