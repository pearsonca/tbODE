#' Run a TB model function
#' 
#' @param func a function with signature function(t, y, parms, ...); see 
#'   \code{\link[deSolve]{ode}}
#' @param parms a list of replacement parameters for the default model
#'   parameters; see \code{\link[deSolve]{ode}}.
#' @param y0 see \code{\link[deSolve]{ode}}
#' @param times see \code{\link[deSolve]{ode}}
#' @export
oderun <- function(
  func,
  parms = list(),
  y0 = c(S=1000, I=10, R=10, N=1020),
  times = 1:1000
) data.table(ode(
  y0, times, func,
  modifyList(list(Beta=0.1, Gmma=0.05), parms)
), key="time")

formatted_results <- function(run) setkey(melt(run,
                                               id.vars = "time",
                                               value.name = "count",
                                               variable.name = "compartment"
), time, compartment)

#' Plot TB ODE run results
#'
#' @param run a result from \code{\link{oderun}}
#' @export
plot_run <- function(run) print(
  ggplot(formatted_results(run)[compartment %in% c("S","I","R")]) + theme_bw() +
    aes(x=time, y=count, color=compartment) +
    geom_line()
)