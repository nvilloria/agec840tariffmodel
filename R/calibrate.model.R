#' Calibrate the partial equilibrium model from benchmark data
#'
#' `calibrate.model()` recovers the structural constants \eqn{K_d, K_s, K_{ms}} and the
#' derived excess-demand elasticity \eqn{\eta_{md}} from benchmark (observed) data and
#' assumed parameter values.
#'
#' @details
#' We normalize the benchmark domestic price to \eqn{P_d = 1} so that benchmark
#' quantities equal benchmark values (expenditures = quantities when \eqn{P = 1}).
#'
#' * \eqn{K_d}: set so that \eqn{Q_d(P_{c0}) = kd}
#'   \eqn{K_d = kd / P_{c0}^{\eta_d}} where \eqn{P_{c0} = (1 - sc)} at \eqn{P_d = 1}
#' * \eqn{K_s}: set so that \eqn{Q_s(P_{p0}) = ks}
#'   \eqn{K_s = ks / P_{p0}^{\epsilon_s}} where \eqn{P_{p0} = (1 + sp)} at \eqn{P_d = 1}
#' * \eqn{K_{ms}}: set so that \eqn{M_s(P_{w0}) = md} (market clearing at benchmark)
#'   \eqn{P_{w0} = 1/(1+tau)}, \eqn{K_{ms} = md / P_{w0}^{\epsilon_{ms}} = md \times (1+tau)^{\epsilon_{ms}}}
#' * \eqn{\eta_{md}} (excess demand elasticity):
#'   \eqn{\eta_{md} = (kd \times \eta_d - ks \times \epsilon_s) / md}
#'   Derived in Lecture 16 from differentiating \eqn{M_d = Q_d - Q_s}.
#'
#' @param kd benchmark domestic consumption (positive quantity, e.g. mt)
#' @param ks benchmark domestic production  (positive quantity)
#' @param eta.d demand price elasticity (must be < 0)
#' @param eps.s supply price elasticity (must be > 0)
#' @param eps.ms import supply (excess supply) elasticity (must be > 0)
#' @param tau benchmark tariff, ad valorem rate (e.g. 0.10 = 10%; default 0)
#' @param sp benchmark production subsidy rate (ad valorem; default 0)
#' @param sc benchmark consumption subsidy rate (ad valorem; default 0)
#'
#' @return A named list of class \code{model} -- the "calibrated model" -- to be passed to
#'          [solve.model()] and [welfare.change()].
#' @export
calibrate.model <- function(kd, ks,
                             eta.d, eps.s, eps.ms,
                             tau = 0, sp = 0, sc = 0) {

  md <- kd - ks

  if (kd <= 0)    stop("kd must be positive.")
  if (ks <= 0)    stop("ks must be positive.")
  if (md <= 0)    stop("md (kd - ks) must be positive (country must be a net importer).")
  if (eta.d >= 0) stop("eta.d must be negative.")
  if (eps.s <= 0) stop("eps.s must be positive.")
  if (eps.ms <= 0)stop("eps.ms must be positive.")
  if (tau < 0)    stop("tau must be non-negative.")
  if (sp < 0)     stop("sp must be non-negative.")
  if (sc < 0)     stop("sc must be non-negative.")
  if (sc >= 1)    stop("sc must be less than 1.")

  p.d0 <- 1
  p.w0 <- p.d0 / (1 + tau)
  p.c0 <- p.d0 * (1 - sc)
  p.p0 <- p.d0 * (1 + sp)

  k.d  <- kd / p.c0^eta.d
  k.s  <- ks / p.p0^eps.s
  k.ms <- md / p.w0^eps.ms

  eta.md <- (kd * eta.d - ks * eps.s) / md

  res <- list(
    kd = kd, ks = ks, md = md,
    eta.d = eta.d, eps.s = eps.s, eps.ms = eps.ms,
    tau = tau, sp = sp, sc = sc,
    k.d = k.d, k.s = k.s, k.ms = k.ms,
    eta.md = eta.md,
    p.d0 = p.d0, p.w0 = p.w0, p.c0 = p.c0, p.p0 = p.p0
  )
  class(res) <- "model"
  res
}
