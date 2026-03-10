#' Fit VBGM to data
#'
#' @param data A tibble with columns age, length
#'
#' @return nls model
#' @export
fit_vbgm <- function(data) {
  nls(length ~ L_inf * (1 - exp(-k * (age - t0))),
      data = data,
      start = list(L_inf = max(data$length, na.rm = TRUE),
                   k = 0.2,
                   t0 = 0))
}

#' Fit Gompertz growth model
#'
#' Fits the Gompertz growth function:
#'   L(t) = L_inf * exp( -a * exp(-k * t) )
#'
#' @param data A tibble/data.frame with columns `age` and `length`
#'
#' @return An nls fitted model object
#' @export
fit_gompertz <- function(data) {
  nls(length ~ L_inf * exp(-a * exp(-k * age)),
      data = data,
      start = list(L_inf = max(data$length, na.rm = TRUE),
                 k = 0.2,
                 a = 1))
}

