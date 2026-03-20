#' Von Bertalanffy Growth Model.
#'
#' Fits a VBGM to the selected data.
#' @param t Age.
#' @param L_inf Asymptotic length.
#' @param k Growth coefficient.
#' @param t0 Theoretical age at length zero.
#'
#' @return Predicted length of NSSH.
#' @export
vbgm <- function(t, L_inf, k, t0) {
  L_inf * (1 - exp(-k * (t - t0)))
}

#' Gompertz growth model.
#'
#' Fits a Gompertz growth model to the selected data.
#' @param t Age.
#' @param L_inf Asymptotic length.
#' @param k Growth rate.
#' @param a Shape parameter.
#'
#' @return Predicted length of NSSH.
#' @export
gompertz_model <- function(t, L_inf, k, a) {
  L_inf * exp(-a * exp(-k * t))
}

#' Logistic growth model
#'
#' Fits a logistic growth model to the selected data.
#' @param t Age (numeric vector)
#' @param Linf Asymptotic length
#' @param k Growth rate
#' @param t0 Inflection parameter
#' @return Length-at-age
#' @export
logistic_model <- function(t, Linf, k, t0) {
  Linf / (1 + exp(-k * (t - t0)))
}
