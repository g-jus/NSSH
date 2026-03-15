#' Reads IMR NSSH file
#' @description
#' This function is to import the dataset from IMR's database.
#' @return A `data.frame`
#' @export
herring_read <- function() {
  data <- read.csv("https://ftp.nmdc.no/nmdc/IMR/Herring/HerringData.csv", na.strings = ".")
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
    dplyr::filter(
      weight > 0) |>
    tidyr::drop_na(length, age, weight)
}
