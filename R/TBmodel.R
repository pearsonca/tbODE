#' A function that matches the signature required for ode solvers in deSolve
#'
#' @param t see \code{\link[deSolve]{ode}}
#' @param y see \code{\link[deSolve]{ode}}
#' @param parameters see \code{\link[deSolve]{ode}}.
#' @seealso \code{\link[deSolve]{ode}}.
#' @export
TBmodel <- function(t, y, parameters) with(c(as.list(y), parameters), {
  infection <- Beta*S*I/N
  recovery <- Gmma*I
  mortality <- c(S=S,I=I,R=R)*Mu
  disease_mortality <- Delta*I
  birth <- sum(mortality, disease_mortality)
  
  dS <- -infection - mortality["S"] + birth
  dI <- infection - recovery - mortality["I"] - disease_mortality
  dR <- recovery - mortality["R"]
  
  return(list(
    c(dS, dI, dR, 0),
    c(infection=infection, recovery=recovery,
      mortality=mortality, disease=disease_mortality,
      birth=birth)
  ))
})