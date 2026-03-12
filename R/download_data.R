#' Reads IMR NSSH file
#' @description
#' This function is to import the dataset from IMR's database.
#' @param path Path to the csv file.
#' @return A `data.frame`
#' @export
herring_read <- function() {
  data <- read.csv("https://ftp.nmdc.no/nmdc/IMR/Herring/HerringData.csv")
  return(data)
}

#' Cleans the NSSH file.
#' @description
#' This function is used to clean raw data from missing values, and converting
#' to numerical values.
#' @param data raw NSSH data.
#' @return A `data.frame`
#' @export
cleaning_herring <- function(data) {
  data |>
    dplyr::mutate(
      weight = suppressWarnings(as.numeric(weight)),
      age    = suppressWarnings(as.numeric(age)),
      lon    = suppressWarnings(as.numeric(lon)),
      lat    = suppressWarnings(as.numeric(lat))
    ) |>
    dplyr::filter(
      !is.na(weight),
      !is.na(age),
      !is.na(lon),
      !is.na(lat),
      age > 0,
      weight > 0)
}
