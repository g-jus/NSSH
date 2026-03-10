vbgm <- function(age, Linf, k, t0) {
  Linf * (1 - exp(-k * (age - t0)))
}


  library(FSA)
## VBGM starting values.
vb_start <- findGrowthStarts(length ~ age, data = herring)
## VBGM equation.
vb_equation <- length~Linf*(1-exp(-K*(age-t0)))
## Fit VBGM.
vb_mod <- nls(vb_equation, data = herring, start = vb_start)
