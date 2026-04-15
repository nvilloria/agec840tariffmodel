#' Solve the equilibrium for price-based policies (tariffs and subsidies)
#'
#' `solve_taxes()` finds the equilibrium given a configuration of tariffs,
#' production subsidies, and consumption subsidies.
#' It uses base R's `uniroot()` to solve the market-clearing condition.
#'
#' @details
#' The function solves one equation in one unknown (\eqn{P_d}):
#'
#' \eqn{K_d(P_d(1-sc))^{\eta_d} - K_s(P_d(1+sp))^{\epsilon_s} - K_{ms}(P_d/(1+tau))^{\epsilon_{ms}} = 0}
#'
#' **Total rates, not increments.** \code{tau}, \code{sp}, and \code{sc} are
#' always the \emph{total} policy rates in the scenario being simulated, not
#' changes relative to the benchmark. The intercepts in \code{cal} are already
#' calibrated to account for whatever distortions existed at the benchmark
#' period. To simulate raising a 10\% benchmark tariff by 10 percentage
#' points, pass \code{tau = 0.20}, not \code{tau = 0.10}.
#'
#' @param cal calibrated model list of class \code{model} from [calibrate_model()]
#' @param tau total tariff rate in the scenario being simulated (default:
#'   calibration benchmark \code{tau}, which reproduces the benchmark
#'   equilibrium exactly)
#' @param sp new production subsidy rate (default: calibration sp)
#' @param sc new consumption subsidy rate (default: calibration sc)
#'
#' @return A named list of class \code{solution} with:
#' \describe{
#'   \item{p.d}{Equilibrium domestic price (normalized to 1 at the calibration benchmark).}
#'   \item{p.w}{World price: \eqn{P_w = P_d / (1 + tau)}.}
#'   \item{p.c}{Consumer price: \eqn{P_c = P_d (1 - sc)}.}
#'   \item{p.p}{Producer price: \eqn{P_p = P_d (1 + sp)}.}
#'   \item{q.d}{Domestic demand quantity: \eqn{Q_d = K_d P_c^{\eta_d}}.}
#'   \item{q.s}{Domestic supply quantity: \eqn{Q_s = K_s P_p^{\epsilon_s}}.}
#'   \item{m.d}{Net imports: \eqn{M_d = Q_d - Q_s} (always equals import supply at equilibrium).}
#'   \item{tariff.rev}{Tariff revenue: \eqn{M_d \times tau \times P_w}.}
#'   \item{prod.sub.cost}{Production subsidy expenditure: \eqn{Q_s \times sp \times P_d}.}
#'   \item{cons.sub.cost}{Consumption subsidy expenditure: \eqn{Q_d \times sc \times P_d}.}
#'   \item{quota, quota.rent.unit, quota.rent.total}{Set to \code{NULL} and 0 (quota not active in this solver).}
#'   \item{mode, tau, sp, sc, rent.domestic}{Policy parameters and mode identifier.}
#' }
#' @importFrom stats uniroot
#' @export
solve_taxes <- function(cal,
                        tau           = cal$tau,
                        sp            = cal$sp,
                        sc            = cal$sc) {

  k.d    <- cal$k.d
  k.s    <- cal$k.s
  k.ms   <- cal$k.ms
  eta.d  <- cal$eta.d
  eps.s  <- cal$eps.s
  eps.ms <- cal$eps.ms

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

  tariff.rev    <- m.d * tau * p.w
  prod.sub.cost <- q.s * sp * p.d.sol
  cons.sub.cost <- q.d * sc * p.d.sol

  res <- list(
    mode          = mode,
    tau           = tau,
    sp            = sp,
    sc            = sc,
    quota         = NULL,
    rent.domestic = 1,
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
