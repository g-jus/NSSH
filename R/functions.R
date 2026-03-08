#' Reads IMR NSSH file
#' @description
#' This function is to import the dataset from IMR's database.
#' @param path Path to the csv file.
#' @return A `data.frame`
#' @export

get_herring_data <- function() {
  url <- "https://ftp.nmdc.no/nmdc/IMR/Herring/HerringData.csv"
  tmp <- tempfile(fileext = ".csv")
  download.file(url, tmp, mode = "wb")
  data <- read.csv(tmp)
  return(data)
}
