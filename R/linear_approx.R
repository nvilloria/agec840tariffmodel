#' Log-linear approximation for the domestic and world prices
#'
#' `linear_approx()` implements the closed-form domestic price formula obtained
#' by log-linearizing the market-clearing condition and solving for \eqn{\log(P_d)}.
#'
#' @details
#' **Derivation Recap**
#' Starting from market clearing in logs:
#' \eqn{\log(K_{md}) + \eta_{md} \log(P_d) = \log(K_{ms}) + \epsilon_{ms} \log(P_w)}
#'
#' Using \eqn{\log(P_w) = \log(P_d) - \log(1+tau)} to eliminate \eqn{P_w}:
#' \eqn{\log(K_{md}) + \eta_{md} \log(P_d) = \log(K_{ms}) + \epsilon_{ms} \log(P_d) - \epsilon_{ms} \log(1+tau)}
#'
#' Solving for \eqn{\log(P_d)}:
#' \eqn{\log(P_d) = (\log(K_{md}) - \log(K_{ms}) + \epsilon_{ms} \log(1+tau)) / (\epsilon_{ms} - \eta_{md})}
#'
#' **Price Transmission Elasticity**
#' Differentiating w.r.t. \eqn{\log(1+tau)}:
#' * \eqn{d \log(P_d) / d \log(1+tau) = \epsilon_{ms} / (\epsilon_{ms} - \eta_{md})}
#' * \eqn{d \log(P_w) / d \log(1+tau) = \eta_{md} / (\epsilon_{ms} - \eta_{md})}
#'
#' Interpretation:
#' * Small country (\eqn{\epsilon_{ms} \rightarrow \infty}): ratio \eqn{\rightarrow 1} (full pass-through)
#' * Large country (finite \eqn{\epsilon_{ms}}): ratio \eqn{< 1} (partial pass-through)
#'
#' @param cal calibrated model returned by [calibrate_model()]
#' @param tau tariff rate for the approximation
#'
#' @return A named list with:
#' \describe{
#'   \item{p.d.approx}{Approximated domestic price at the given \code{tau}.}
#'   \item{p.w.approx}{Approximated world price: \eqn{P_d / (1 + tau)}.}
#'   \item{transmission.domestic}{Price transmission elasticity to the domestic market:
#'     \eqn{\epsilon_{ms} / (\epsilon_{ms} - \eta_{md})}. Approaches 1 as
#'     \eqn{\epsilon_{ms} \to \infty} (small country); less than 1 for a large country.}
#'   \item{transmission.world}{Price transmission elasticity to the world market:
#'     \eqn{\eta_{md} / (\epsilon_{ms} - \eta_{md})}. Negative, reflecting the
#'     terms-of-trade effect: a tariff lowers the world price.}
#' }
#' @export
linear_approx <- function(cal, tau) {
  k.md   <- cal$md
  k.ms   <- cal$k.ms
  eta.md <- cal$eta.md
  eps.ms <- cal$eps.ms

  log.p.d <- (log(k.md) - log(k.ms) + eps.ms * log(1 + tau)) /
             (eps.ms - eta.md)
  p.d.approx <- exp(log.p.d)
  p.w.approx <- p.d.approx / (1 + tau)
  transmission.domestic <- eps.ms / (eps.ms - eta.md)
  transmission.world    <- eta.md  / (eps.ms - eta.md)

  list(
    p.d.approx            = p.d.approx,
    p.w.approx            = p.w.approx,
    transmission.domestic = transmission.domestic,
    transmission.world    = transmission.world
  )
}
