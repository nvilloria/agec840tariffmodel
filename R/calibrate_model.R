#' Calibrate the partial equilibrium model from benchmark data
#'
#' `calibrate_model()` recovers the intercepts \eqn{K_d, K_s, K_{ms}} of the
#' behavioral functions and the derived excess-demand elasticity \eqn{\eta_{md}}
#' from benchmark (observed) data and assumed parameter values.
#'
#' @details
#' **Units.** \code{kd} and \code{ks} may be physical quantities (e.g., metric
#' tons) or expenditure values (e.g., millions of USD), provided they use the
#' same units.  Normalizing \eqn{P_d = 1} makes values and quantities numerically
#' identical at the benchmark (\eqn{P \times Q = Q} when \eqn{P = 1}), so the
#' unit choice simply determines the units of all subsequent welfare results.
#'
#' **Structural constants** (all evaluated at \eqn{P_{d0} = 1}):
#' * \eqn{K_d = kd / P_{c0}^{\eta_d}}, where \eqn{P_{c0} = 1 - sc}.
#' * \eqn{K_s = ks / P_{p0}^{\varepsilon_s}}, where \eqn{P_{p0} = 1 + sp}.
#' * \eqn{K_{ms} = md \cdot (1+\tau)^{\varepsilon_{ms}}}, where \eqn{P_{w0} = 1/(1+\tau)}.
#' * \eqn{\eta_{md} = (kd \cdot \eta_d - ks \cdot \varepsilon_s) / md}
#'   (excess demand elasticity; always negative).
#'
#' **Why \eqn{K_{ms} \neq md} when \eqn{tau > 0}**
#'
#' \eqn{K_d}, \eqn{K_s}, and \eqn{K_{ms}} are intercepts of functions
#' anchored to *different* benchmark prices.  Domestic demand and supply are
#' evaluated at the domestic price \eqn{P_{d0} = 1}, so \eqn{K_d = kd} and
#' \eqn{K_s = ks}.  Import supply is evaluated at the *world* price
#' \eqn{P_{w0} = 1/(1+tau) < 1} when there is a tariff.  To deliver the same
#' benchmark import quantity \eqn{md} at that lower world price, \eqn{K_{ms}}
#' must be larger: \eqn{K_{ms} = md \times (1+tau)^{\epsilon_{ms}}}.
#' Consequently, \eqn{K_{ms} = K_d - K_s} only when \eqn{tau = 0} (and
#' \eqn{sp = sc = 0}), when all prices coincide at 1.  The relation that always
#' holds is between benchmark *quantities*: \eqn{kd - ks = md}.
#'
#' @param kd benchmark domestic consumption: a physical quantity (e.g., metric
#'   tons) or an expenditure value (e.g., millions of USD). Must be positive
#'   and strictly greater than \code{ks}.
#' @param ks benchmark domestic production, in the same units as \code{kd}.
#'   Must be positive.
#' @param eta.d demand price elasticity (must be < 0)
#' @param eps.s supply price elasticity (must be > 0)
#' @param eps.ms import supply (excess supply) elasticity (must be > 0)
#' @param tau benchmark tariff rate present in the observed data (ad valorem;
#'   e.g. 0.10 = 10\%; default 0); must be >= 0. Used solely to recover the
#'   correct intercepts from benchmark data — particularly \eqn{K_{ms}}, whose
#'   value depends on the world price \eqn{P_{w0} = 1/(1+\tau_0)}. It does
#'   \strong{not} set a floor on counterfactual tariff rates: in
#'   \code{solve_taxes()}, \code{tau} always means the \emph{total} rate in
#'   the scenario being simulated.
#' @param sp benchmark production subsidy rate (ad valorem; default 0); must be >= 0.
#' @param sc benchmark consumption subsidy rate (ad valorem; default 0); must be >=0, < 1.
#'
#' @return A named list of class \code{model} with the following elements:
#' \describe{
#'   \item{kd, ks, md}{Benchmark quantities: total domestic demand, domestic supply,
#'     and net imports (\eqn{md = kd - ks}).}
#'   \item{eta.d, eps.s, eps.ms}{Elasticities as supplied by the user.}
#'   \item{tau, sp, sc}{Benchmark policy rates as supplied.}
#'   \item{k.d, k.s, k.ms}{Intercepts of the demand, domestic supply,
#'     and import supply functions respectively.}
#'   \item{eta.md}{Derived excess-demand (import demand) elasticity with respect to
#'     the domestic price; negative by construction.}
#'   \item{p.d0, p.w0, p.c0, p.p0}{Benchmark prices: domestic, world, consumer, and
#'     producer prices (all equal to 1 except \eqn{P_{w0} = 1/(1+tau)} when
#'     \eqn{tau > 0}).}
#' }
#' @export
calibrate_model <- function(kd, ks,
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
