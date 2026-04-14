#' Solve the equilibrium for a binding import quota
#'
#' `solve_quota()` finds the equilibrium given a fixed import quantity.
#'
#' @details
#' Import quantity is fixed at \eqn{M_q}. Two equations solved sequentially:
#' 1. \eqn{K_d(P_d(1-sc))^{\eta_d} - K_s(P_d(1+sp))^{\epsilon_s} = M_q}
#'    => domestic price \eqn{P_d} (domestic market clears at quota level)
#' 2. \eqn{K_{ms} P_w^{\epsilon_{ms}} = M_q}
#'    => world price \eqn{P_w} (foreign supply meets the quota level)
#'
#' The quota rent per unit = \eqn{P_d - P_w} (the scarcity wedge).
#' Welfare impact depends on who captures the rent (see `rent.domestic`).
#'
#' @param cal calibrated model list of class \code{model} from [calibrate_model()]
#' @param quota binding import quota in quantities (strictly positive)
#' @param sp new production subsidy rate (default: calibration sp)
#' @param sc new consumption subsidy rate (default: calibration sc)
#' @param rent.domestic share in \code{[0,1]} of quota rents captured by domestic agents
#'                      (default 1; set to 0 for a VER-type foreign-rent scenario)
#'
#' @return A named list of class \code{solution} with:
#' \describe{
#'   \item{p.d}{Domestic price: the level at which domestic demand minus domestic supply equals the quota.}
#'   \item{p.w}{World price: \eqn{P_w = (M_q / K_{ms})^{1/\epsilon_{ms}}}, the price at which
#'     foreign export supply exactly equals the quota.}
#'   \item{p.c}{Consumer price: \eqn{P_c = P_d (1 - sc)}.}
#'   \item{p.p}{Producer price: \eqn{P_p = P_d (1 + sp)}.}
#'   \item{q.d}{Domestic demand quantity.}
#'   \item{q.s}{Domestic supply quantity.}
#'   \item{m.d}{Net imports, equal to \code{quota} by construction.}
#'   \item{quota.rent.unit}{Price wedge per unit: \eqn{P_d - P_w}.}
#'   \item{quota.rent.total}{Total quota rents: \eqn{(P_d - P_w) \times M_q}.}
#'   \item{tariff.rev}{Set to 0 (tariffs are inactive when a binding quota is in effect).}
#'   \item{prod.sub.cost, cons.sub.cost}{Subsidy expenditures.}
#'   \item{mode, quota, sp, sc, rent.domestic}{Policy parameters and mode identifier.}
#' }
#' @importFrom stats uniroot
#' @export
solve_quota <- function(cal,
                        quota,
                        sp            = cal$sp,
                        sc            = cal$sc,
                        rent.domestic = 1) {

  k.d    <- cal$k.d
  k.s    <- cal$k.s
  k.ms   <- cal$k.ms
  eta.d  <- cal$eta.d
  eps.s  <- cal$eps.s
  eps.ms <- cal$eps.ms

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

  tariff.rev    <- 0
  prod.sub.cost <- q.s * sp * p.d.sol
  cons.sub.cost <- q.d * sc * p.d.sol

  res <- list(
    mode          = mode,
    tau           = 0, # Tariffs are typically zero or irrelevant when quota binds
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
