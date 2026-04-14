#' Compute welfare changes between a baseline and counterfactual equilibrium
#'
#' `welfare_change()` computes the welfare effects of moving from a base
#' equilibrium (`base`) to a counterfactual equilibrium (`cf`).
#'
#' @details
#' **Approximation Strategy**
#' All surplus areas use the trapezoidal rule, which equals the sum of the
#' standard triangles and rectangles in the partial equilibrium diagrams.
#' * \eqn{\Delta CS = -((Q_{d0} + Q_{d1})/2) \times (P_{c1} - P_{c0})}
#' * \eqn{\Delta PS = +((Q_{s0} + Q_{s1})/2) \times (P_{p1} - P_{p0})}
#'
#' The trapezoidal rule is exact for linear supply/demand and a very good
#' approximation for the constant-elasticity functions used here.
#'
#' **Welfare Components**
#' * `delta.cs`: consumer surplus change
#' * `delta.ps`: producer surplus change
#' * `tariff.rev`: change in tariff revenues collected by government
#' * `prod.sub`: change in production subsidy cost (negative = gov. expenditure)
#' * `cons.sub`: change in consumption subsidy cost (negative = gov. expenditure)
#' * `quota.rents`: quota rents accruing to domestic agents (= 0 if foreign VER)
#' * `tot.gain`: terms-of-trade gain (\eqn{M_{d1} \times (P_{w0} - P_{w1})}); positive if
#'   the tariff depresses world prices (large-country effect)
#' * `dw.loss`: deadweight welfare triangle B+D (always non-negative loss)
#' * `net.welfare`: algebraic sum of all domestic welfare components
#'
#' The relationship between these is:
#' `net.welfare = delta.cs + delta.ps + tariff.rev + prod.sub + cons.sub + quota.rents`
#'
#' The `tot.gain` component is already embedded in `delta.cs`, `delta.ps`, and
#' `tariff.rev`, but it is reported separately for teaching purposes.
#'
#' @param base baseline equilibrium returned by [solve_taxes()]
#' @param cf counterfactual equilibrium returned by [solve_taxes()] or [solve_quota()]
#' @param cal calibrated model returned by [calibrate_model()]
#'
#' @return A named list of class \code{welfare} with:
#' \describe{
#'   \item{delta.cs}{Change in consumer surplus (negative when domestic price rises).}
#'   \item{delta.ps}{Change in producer surplus (positive when domestic price rises).}
#'   \item{tariff.rev}{Change in tariff revenue collected by the government.}
#'   \item{prod.sub}{Change in production subsidy cost (negative = increased expenditure).}
#'   \item{cons.sub}{Change in consumption subsidy cost (negative = increased expenditure).}
#'   \item{quota.rents}{Quota rents accruing to domestic agents
#'     (\eqn{(P_d - P_w) \times M_q \times} \code{rent.domestic}); zero under tariffs.}
#'   \item{net.welfare}{\eqn{\Delta CS + \Delta PS + \Delta} tariff revenue
#'     \eqn{+ \Delta} subsidy costs \eqn{+} quota rents. This is the authoritative
#'     national welfare measure.}
#'   \item{tot.gain}{Memo item: terms-of-trade gain
#'     \eqn{M_{cf} \times (P_{w,base} - P_{w,cf})}. Positive when the tariff
#'     depresses the world price (large-country effect). Already embedded in
#'     \code{net.welfare}; reported separately for decomposition.}
#'   \item{dw.loss}{Memo item: deadweight loss triangles B+D,
#'     \eqn{0.5 \times \Delta M \times \Delta P_d}. Always non-negative.
#'     Already embedded in \code{net.welfare}; reported separately for decomposition.
#'     Note: \code{net.welfare} \eqn{\approx} \code{tot.gain} \eqn{-} \code{dw.loss}
#'     exactly when the baseline is free trade (\eqn{tau_{base} = 0}); for
#'     non-zero baseline tariffs a small residual arises from the trapezoidal
#'     approximation of surplus areas.}
#' }
#' @export
welfare_change <- function(base, cf, cal) {
  delta.cs <- -((base$q.d + cf$q.d) / 2) * (cf$p.c - base$p.c)
  delta.ps <- +((base$q.s + cf$q.s) / 2) * (cf$p.p - base$p.p)
  tariff.rev <- cf$tariff.rev - base$tariff.rev
  prod.sub <- -(cf$prod.sub.cost - base$prod.sub.cost)
  cons.sub <- -(cf$cons.sub.cost - base$cons.sub.cost)
  quota.rents <- cf$quota.rent.total * cf$rent.domestic
  tot.gain <- cf$m.d * (base$p.w - cf$p.w)
  dw.loss <- 0.5 * (base$m.d - cf$m.d) * (cf$p.d - base$p.d)
  net.welfare <- delta.cs + delta.ps + tariff.rev + prod.sub + cons.sub + quota.rents

  res <- list(
    delta.cs    = delta.cs,
    delta.ps    = delta.ps,
    tariff.rev  = tariff.rev,
    prod.sub    = prod.sub,
    cons.sub    = cons.sub,
    quota.rents = quota.rents,
    tot.gain    = tot.gain,
    dw.loss     = dw.loss,
    net.welfare = net.welfare
  )
  class(res) <- "welfare"
  res
}
