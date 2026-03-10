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

