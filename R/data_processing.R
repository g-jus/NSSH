#' This function is used to process NSSH Data for Shiny App.
#' Loads raw data and applies all processing steps provided by
#' underlying NSSH package functions.
#' @return A list with processed datasets.
#' @export

process_nssh_data <- function() {
  # Load raw data.
  herring_raw <- herring_read()

  # Apply cleaning.
  clean_herring <- cleaning_herring(herring_raw)

  # Create derived datasets.
  growth_data_small <- clean_herring |>
    dplyr::sample_n(min(5000, nrow(clean_herring)), .seed = 123)

  # Maximum age.
  max_age <- max(clean_herring$age, na.rm = TRUE)

  # Calculates number of unique fish per year.
  counts_per_year <- count_per_year(clean_herring)

  # Calculates total weight (tonnes) per year.
  weights_per_year <- weight_per_year(clean_herring)

  # Counts number of fish per age per year.
  age_counts <- age_count_for_year(clean_herring)

  # Makes a summary of mean age and max age of fish per year.
  age_summary <- age_summary_for_year(clean_herring)

  # Makes a summary for map lables, with biological data.
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
