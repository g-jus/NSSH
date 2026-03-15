#' Reads IMR NSSH file
#' @description
#' Function to import the raw NSSH dataset from the
#' Institute of Marine Research database.
#' @return A `data.frame`
#' @export
herring_read <- function() {
  data <- read.csv("https://ftp.nmdc.no/nmdc/IMR/Herring/HerringData.csv", na.strings = ".")
  return(data)
}

