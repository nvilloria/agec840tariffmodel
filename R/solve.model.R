#' Solve the equilibrium for a given policy configuration
#'
#' `solve.model()` finds the equilibrium given a policy configuration.
#' It uses base R's `uniroot()` to solve the market-clearing condition.
#'
#' @details
#' The model operates in two modes:
#'
#' **Mode A: Elastic import supply (\code{quota = NULL}, default)**
#' Solves one equation in one unknown (\eqn{P_d}):
#'
#' \eqn{K_d(P_d(1-sc))^{\eta_d} - K_s(P_d(1+sp))^{\epsilon_s} - K_{ms}(P_d/(1+tau))^{\epsilon_{ms}} = 0}
#'
#' **Mode B: Binding import quota (\code{quota = positive number})**
#' Import quantity is fixed at \eqn{M_q}. Two equations solved sequentially:
#' 1. \eqn{K_d(P_d(1-sc))^{\eta_d} - K_s(P_d(1+sp))^{\epsilon_s} = M_q}
#'    => domestic price \eqn{P_d} (domestic market clears at quota level)
#' 2. \eqn{K_{ms} P_w^{\epsilon_{ms}} = M_q}
#'    => world price \eqn{P_w} (foreign supply meets the quota level)
#'
#' The quota rent per unit = \eqn{P_d - P_w} (the scarcity wedge).
#' Welfare impact depends on who captures the rent (see `rent.domestic`).
#'
#' @param cal calibrated model list of class \code{model} from [calibrate.model()]
#' @param tau new tariff (default: calibration benchmark tau)
#' @param sp new production subsidy rate (default: calibration sp)
#' @param sc new consumption subsidy rate (default: calibration sc)
#' @param quota binding import quota in quantities (default NULL = no quota)
#' @param rent.domestic share in \code{[0,1]} of quota rents captured by domestic agents
#'                      (default 1; set to 0 for a VER-type foreign-rent scenario)
#'
#' @return A named list of class \code{solution} with equilibrium prices, quantities, and fiscal items.
#' @importFrom stats uniroot
#' @rawNamespace export(solve.model)
solve.model <- function(cal,
                        tau           = cal$tau,
                        sp            = cal$sp,
                        sc            = cal$sc,
                        quota         = NULL,
                        rent.domestic = 1) {

  k.d    <- cal$k.d
  k.s    <- cal$k.s
  k.ms   <- cal$k.ms
  eta.d  <- cal$eta.d
  eps.s  <- cal$eps.s
  eps.ms <- cal$eps.ms

  if (is.null(quota)) {
    excess <- function(p.d) {
      p.c <- p.d * (1 - sc)
      p.p <- p.d * (1 + sp)
      p.w <- p.d / (1 + tau)
      q.d <- k.d  * p.c ^ eta.d
      q.s <- k.s  * p.p ^ eps.s
      m.d <- q.d - q.s
      m.s <- k.ms * p.w  ^ eps.ms
      m.d - m.s
    }

    p.d.sol <- uniroot(excess,
                       interval = c(0.001, 1000),
                       tol      = .Machine$double.eps^0.5)$root

    p.c  <- p.d.sol * (1 - sc)
    p.p  <- p.d.sol * (1 + sp)
    p.w  <- p.d.sol / (1 + tau)
    q.d  <- k.d  * p.c  ^ eta.d
    q.s  <- k.s  * p.p  ^ eps.s
    m.d  <- q.d - q.s

    quota.rent.unit  <- 0
    quota.rent.total <- 0
    mode             <- "tariff-subsidy"

  } else {
    if (quota <= 0) stop("quota must be a strictly positive quantity.")

    dom.residual <- function(p.d) {
      p.c <- p.d * (1 - sc)
      p.p <- p.d * (1 + sp)
      k.d * p.c ^ eta.d - k.s * p.p ^ eps.s - quota
    }
    p.d.sol <- uniroot(dom.residual,
                       interval = c(0.001, 1000),
                       tol      = .Machine$double.eps^0.5)$root

    p.w  <- (quota / k.ms) ^ (1 / eps.ms)
    p.c  <- p.d.sol * (1 - sc)
    p.p  <- p.d.sol * (1 + sp)
    q.d  <- k.d * p.c ^ eta.d
    q.s  <- k.s * p.p ^ eps.s
    m.d  <- quota

    quota.rent.unit  <- p.d.sol - p.w
    quota.rent.total <- quota.rent.unit * quota
    mode             <- "quota"
  }

  tariff.rev    <- if (is.null(quota)) m.d * tau * p.w else 0
  prod.sub.cost <- q.s * sp * p.d.sol
  cons.sub.cost <- q.d * sc * p.d.sol

  res <- list(
    mode          = mode,
    tau           = tau,
    sp            = sp,
    sc            = sc,
    quota         = quota,
    rent.domestic = rent.domestic,
    p.d           = p.d.sol,
    p.w           = p.w,
    p.c           = p.c,
    p.p           = p.p,
    q.d           = q.d,
    q.s           = q.s,
    m.d           = m.d,
    tariff.rev        = tariff.rev,
    prod.sub.cost     = prod.sub.cost,
    cons.sub.cost     = cons.sub.cost,
    quota.rent.unit   = quota.rent.unit,
    quota.rent.total  = quota.rent.total
  )
  class(res) <- "solution"
  res
}
