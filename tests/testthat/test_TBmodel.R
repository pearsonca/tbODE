library(drTB)
require(deSolve)

context("TBModel behavior")

solve_TB_model <- function(y0, parameters, times=seq(from=0,to=10,by=0.1), tol = 1e-8) {
  lsoda(y0, times, TBmodel, parms=parameters, rtol=tol, atol=tol)
}

nullList <- list(Beta = 0, Gmma = 0, Mu = 0, Delta = 0)

paramCombos <- list(
  null = nullList,
  only_transmission = modifyList(nullList, list(Beta=0.5))
)

test_that("population (N) is constant", {
  n0 <- 1000
  i0 <- 1
  y0 <- c(S=n0-i0, I=i0, R=0, N=n0)
  lapply(paramCombos, function(ps) {
    result <- solve_TB_model(y0, ps)
    expect_equal(rowSums(result[,c("S","I","R")]), rep(n0, dim(result)[1]))
  })
})

# test_that("with only transmission, S/(N-S) = S_0/(N-S_0)*exp(-Beta*t)", {
#   n0 <- 1000
#   i0 <- 1
#   y0 <- c(S=n0-i0, I=i0, R=0)
#   lapply(Filter(function(ps) (ps$Gmma == 0) && (ps$Mu == 0), paramCombos), function(ps) {
#     result <- solve_TB_model(y0, ps)
#     S <- result[,"S"]; t <- result[,"time"]; S0 <- y0["S"]
#     expect_equal(S/(n0-S), S0/(n0-S0)*exp(-ps$Beta*t), tol=1e-6)
#   })
# })
# 
# test_that("with only recovery, I = I_0*exp(-Gmma*t), R = R_0 + I_0*(1-exp(-Gmma*t))", {
#   n0 <- 1000
#   i0 <- 100
#   r0 <- 10
#   y0 <- c(S=n0-i0-r0, I=i0, R=r0)
#   lapply(Filter(function(ps) (ps$Beta == 0) && (ps$Mu == 0), paramCombos), function(ps) {
#     result <- solve_TB_model(y0, ps)
#     I <- result[,"I"]; R <- result[,"R"]; t <- result[,"time"]
#     expect_equal(I, i0*exp(-ps$Gmma*t), tol=1e-6)
#     expect_equal(R, r0+i0*(1-exp(-ps$Gmma*t)), tol=1e-6)
#   })
# })
# 
# test_that("with only disease induced mortality, S = S_0 + I_0*(1-exp(-Delta*t))", {
#   n0 <- 1000
#   i0 <- 100
#   r0 <- 10
#   y0 <- c(S=n0-i0-r0, I=i0, R=r0)
#   ps <- 
#     result <- solve_TB_model(y0, ps)
#     I <- result[,"I"]; R <- result[,"R"]; t <- result[,"time"]
#     expect_equal(I, i0*exp(-ps$Gmma*t), tol=1e-6)
#     expect_equal(R, r0+i0*(1-exp(-ps$Gmma*t)), tol=1e-6)
#   })
# })