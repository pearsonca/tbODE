#' A function that matches the signature required for ode solvers in deSolve
#' 
#' @param ... who knows?
#' @seealso \code{\link[deSolve]{ode}}.
#' @export
TBfunc <- function(t, y, parms) with(parms, {
  S <- y[1]; I <- y[2]; R <- y[3]
  N <- S + I + R
  
  dS <- -Beta*S*I/N
  dI <- Beta*S*I/N - Gmma*I
  dR <- Gmma*I
  
  return(list(c(dS, dI, dR, dS+dI+dR)))
  # ...?
})