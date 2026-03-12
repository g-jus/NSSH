#' Von Bertalanffy Growth Model.
#'
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

