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

clean_data <-

fit_vbgm <-
  library(FSA)
  ## VBGM starting values.
  vb_start <- findGrowthStarts(length ~ age, data = herring)
  ## VBGM equation.
  vb_equation <- length~Linf*(1-exp(-K*(age-t0)))
  ## Fit VBGM.
  vb_mod <- nls(vb_equation, data = herring, start = vb_start)

max_age <-

max_length <-

landings <-

plot_growth <-

shiny <-
