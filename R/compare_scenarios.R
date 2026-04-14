#' Compare multiple policy scenarios at once
#'
#' `compare_scenarios()` runs a list of policy scenarios and returns a
#' data frame with prices, quantities, and welfare for each.
#'
#' @details
#' Each scenario is a named list with any subset of:
#' `tau`, `sp`, `sc`, `quota`, `rent.domestic`.
#' Omitted fields default to the calibration benchmark values.
#'
#' @param cal calibrated model returned by [calibrate_model()]
#' @param ... named lists of scenario parameters
#'
#' @return A data frame with one row per scenario and columns:
#' \describe{
#'   \item{scenario}{Scenario name.}
#'   \item{tau.pct, sp.pct, sc.pct}{Policy rates in percent.}
#'   \item{quota}{Quota level, or \code{NA} for tariff/subsidy scenarios.}
#'   \item{p.d, p.w}{Equilibrium domestic and world prices.}
#'   \item{q.d, q.s, m.d}{Equilibrium demand, domestic supply, and net imports.}
#'   \item{delta.cs, delta.ps}{Changes in consumer and producer surplus relative to
#'     the calibration benchmark.}
#'   \item{tariff.rev}{Change in tariff revenue.}
#'   \item{prod.sub, cons.sub}{Changes in production and consumption subsidy costs.}
#'   \item{quota.rents}{Quota rents accruing to domestic agents.}
#'   \item{tot.gain}{Terms-of-trade gain (memo item).}
#'   \item{net.welfare}{Net national welfare change relative to the calibration benchmark.}
#' }
#' @export
compare_scenarios <- function(cal, ...) {
  scenarios <- list(...)
  if (is.null(names(scenarios)))
    names(scenarios) <- paste0("S", seq_along(scenarios))

  base <- solve_taxes(cal)

  results <- lapply(names(scenarios), function(nm) {
    s   <- scenarios[[nm]]
    tau <- if (!is.null(s$tau))           s$tau           else cal$tau
    sp  <- if (!is.null(s$sp))            s$sp            else cal$sp
    sc  <- if (!is.null(s$sc))            s$sc            else cal$sc
    q   <- s$quota
    rd  <- if (!is.null(s$rent.domestic)) s$rent.domestic else 1

    if (is.null(q)) {
      cf  <- solve_taxes(cal, tau = tau, sp = sp, sc = sc)
    } else {
      cf  <- solve_quota(cal, quota = q, sp = sp, sc = sc, rent.domestic = rd)
    }
    wf  <- welfare_change(base, cf, cal)

    data.frame(
      scenario    = nm,
      tau.pct     = tau * 100,
      sp.pct      = sp  * 100,
      sc.pct      = sc  * 100,
      quota       = if (is.null(q)) NA_real_ else q,
      p.d         = round(cf$p.d, 4),
      p.w         = round(cf$p.w, 4),
      q.d         = round(cf$q.d, 2),
      q.s         = round(cf$q.s, 2),
      m.d         = round(cf$m.d, 2),
      delta.cs    = round(wf$delta.cs,   2),
      delta.ps    = round(wf$delta.ps,   2),
      tariff.rev  = round(wf$tariff.rev, 2),
      prod.sub    = round(wf$prod.sub,   2),
      cons.sub    = round(wf$cons.sub,   2),
      quota.rents = round(wf$quota.rents,2),
      tot.gain    = round(wf$tot.gain,   2),
      net.welfare = round(wf$net.welfare,2),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}
