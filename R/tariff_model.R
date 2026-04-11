#' AGEC 840 Tariff Model Functions
#'
#' Core functions for the AGEC 840 partial equilibrium tariff model.
#' These functions implement benchmark calibration, equilibrium solving,
#' welfare decomposition, and log-linear approximation.
#'
#' @name tariff-model
#' @keywords internal
NULL

#' Calibrate the partial equilibrium model from benchmark data
#'
#' @param kd Benchmark domestic consumption (positive numeric).
#' @param ks Benchmark domestic production (positive numeric).
#' @param md Benchmark net imports (= kd - ks; must be positive).
#' @param eta.d Domestic demand elasticity (negative numeric).
#' @param eps.s Domestic supply elasticity (positive numeric).
#' @param eps.ms Import supply elasticity (positive numeric).
#' @param tau Benchmark tariff rate (non-negative ad valorem, default 0).
#' @param sp Benchmark production subsidy rate (non-negative ad valorem, default 0).
#' @param sc Benchmark consumption subsidy rate (non-negative ad valorem, default 0).
#'
#' @return A named list containing calibrated constants, elasticities, benchmark prices,
#'   and policy parameters.
#' @export
calibrate.model <- function(kd, ks, md,
                             eta.d, eps.s, eps.ms,
                             tau = 0, sp = 0, sc = 0) {

  if (kd <= 0)    stop("kd must be positive.")
  if (ks <= 0)    stop("ks must be positive.")
  if (md <= 0)    stop("md must be positive (country must be a net importer).")
  if (eta.d >= 0) stop("eta.d must be negative.")
  if (eps.s <= 0) stop("eps.s must be positive.")
  if (eps.ms <= 0)stop("eps.ms must be positive.")
  if (tau < 0)    stop("tau must be non-negative.")
  if (sp < 0)     stop("sp must be non-negative.")
  if (sc < 0)     stop("sc must be non-negative.")
  if (sc >= 1)    stop("sc must be less than 1.")
  if (abs(kd - ks - md) > 1e-6 * kd)
    warning("kd - ks != md: check that benchmark data is internally consistent.")

  p.d0 <- 1
  p.w0 <- p.d0 / (1 + tau)
  p.c0 <- p.d0 * (1 - sc)
  p.p0 <- p.d0 * (1 + sp)

  k.d  <- kd / p.c0^eta.d
  k.s  <- ks / p.p0^eps.s
  k.ms <- md / p.w0^eps.ms

  eta.md <- (kd * eta.d - ks * eps.s) / md

  list(
    kd = kd, ks = ks, md = md,
    eta.d = eta.d, eps.s = eps.s, eps.ms = eps.ms,
    tau = tau, sp = sp, sc = sc,
    k.d = k.d, k.s = k.s, k.ms = k.ms,
    eta.md = eta.md,
    p.d0 = p.d0, p.w0 = p.w0, p.c0 = p.c0, p.p0 = p.p0
  )
}

#' Solve the equilibrium for a given policy configuration
#'
#' @param cal Calibrated model list returned by \code{calibrate.model()}.
#' @param tau Tariff rate for the counterfactual (default uses \code{cal$tau}).
#' @param sp Production subsidy rate for the counterfactual (default uses \code{cal$sp}).
#' @param sc Consumption subsidy rate for the counterfactual (default uses \code{cal$sc}).
#' @param quota Binding import quota in quantities (default \code{NULL} for elastic import supply).
#' @param rent.domestic Share of quota rents captured domestically, between 0 and 1.
#'
#' @return A named list containing equilibrium prices, quantities, and fiscal items.
#' @export solve.model
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

  list(
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
}

#' Compute welfare changes between a baseline and counterfactual equilibrium
#'
#' @param base Baseline equilibrium returned by \code{solve.model()}.
#' @param cf Counterfactual equilibrium returned by \code{solve.model()}.
#' @param cal Calibrated model returned by \code{calibrate.model()}.
#'
#' @return A named list with welfare components and net welfare.
#' @export
welfare.change <- function(base, cf, cal) {
  delta.cs <- -((base$q.d + cf$q.d) / 2) * (cf$p.c - base$p.c)
  delta.ps <- +((base$q.s + cf$q.s) / 2) * (cf$p.p - base$p.p)
  tariff.rev <- cf$tariff.rev - base$tariff.rev
  prod.sub <- -(cf$prod.sub.cost - base$prod.sub.cost)
  cons.sub <- -(cf$cons.sub.cost - base$cons.sub.cost)
  quota.rents <- cf$quota.rent.total * cf$rent.domestic
  tot.gain <- cf$m.d * (base$p.w - cf$p.w)
  dw.loss <- 0.5 * (base$m.d - cf$m.d) * (cf$p.d - cal$p.w0)
  net.welfare <- delta.cs + delta.ps + tariff.rev + prod.sub + cons.sub + quota.rents

  list(
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
}

#' Log-linear approximation for the domestic and world prices
#'
#' @param cal Calibrated model returned by \code{calibrate.model()}.
#' @param tau Tariff rate used in the approximation.
#'
#' @return A named list with approximated prices and transmission elasticities.
#' @export
linear.approx <- function(cal, tau) {
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

#' Print a formatted equilibrium solution
#'
#' @param sol A solution list returned by \code{solve.model()}.
#' @param label Optional label to display.
#' @export print.solution
print.solution <- function(sol, label = "Solution") {
  cat("\n===", label, "===\n")
  cat(sprintf("  Mode          : %s\n", sol$mode))
  cat(sprintf("  Tariff (tau)  : %6.1f%%\n", sol$tau * 100))
  cat(sprintf("  Prod. subsidy : %6.1f%%\n", sol$sp  * 100))
  cat(sprintf("  Cons. subsidy : %6.1f%%\n", sol$sc  * 100))
  if (!is.null(sol$quota))
    cat(sprintf("  Quota level   : %10.2f\n", sol$quota))
  cat("\n  Prices:\n")
  cat(sprintf("    Domestic (P_d)  : %.4f\n", sol$p.d))
  cat(sprintf("    World    (P_w)  : %.4f\n", sol$p.w))
  cat(sprintf("    Consumer (P_c)  : %.4f\n", sol$p.c))
  cat(sprintf("    Producer (P_p)  : %.4f\n", sol$p.p))
  cat("\n  Quantities:\n")
  cat(sprintf("    Domestic demand  (Q_d) : %10.2f\n", sol$q.d))
  cat(sprintf("    Domestic supply  (Q_s) : %10.2f\n", sol$q.s))
  cat(sprintf("    Net imports      (M_d) : %10.2f\n", sol$m.d))
  cat("\n  Government accounts:\n")
  cat(sprintf("    Tariff revenue      : %10.2f\n", sol$tariff.rev))
  cat(sprintf("    Prod. subsidy cost  : %10.2f\n", sol$prod.sub.cost))
  cat(sprintf("    Cons. subsidy cost  : %10.2f\n", sol$cons.sub.cost))
  if (sol$mode == "quota")
    cat(sprintf("    Quota rent (total)  : %10.2f  [domestic share: %.0f%%]\n",
                sol$quota.rent.total, sol$rent.domestic * 100))
  invisible(sol)
}

#' Print a formatted welfare decomposition
#'
#' @param wf A welfare list returned by \code{welfare.change()}.
#' @param label Optional label to display.
#' @export print.welfare
print.welfare <- function(wf, label = "Welfare Change") {
  cat("\n===", label, "===\n")
  cat(sprintf("  Delta CS (consumer surplus)   : %+10.2f\n", wf$delta.cs))
  cat(sprintf("  Delta PS (producer surplus)   : %+10.2f\n", wf$delta.ps))
  cat(sprintf("  Tariff revenue  (change)      : %+10.2f\n", wf$tariff.rev))
  cat(sprintf("  Prod. subsidy cost (change)   : %+10.2f\n", wf$prod.sub))
  cat(sprintf("  Cons. subsidy cost (change)   : %+10.2f\n", wf$cons.sub))
  cat(sprintf("  Quota rents (domestic share)  : %+10.2f\n", wf$quota.rents))
  cat("  ------------------------------------------------\n")
  cat(sprintf("  Net welfare                   : %+10.2f\n", wf$net.welfare))
  cat(sprintf("  [memo] Terms-of-trade gain    : %+10.2f\n", wf$tot.gain))
  cat(sprintf("  [memo] DW loss triangle B+D   : %+10.2f\n", wf$dw.loss))
  invisible(wf)
}

#' Compare multiple policy scenarios at once
#'
#' @param cal Calibrated model returned by \code{calibrate.model()}.
#' @param ... Named lists of scenario parameters (tau, sp, sc, quota, rent.domestic).
#'
#' @return A data frame comparing prices, quantities, and welfare across scenarios.
#' @export
compare.scenarios <- function(cal, ...) {
  scenarios <- list(...)
  if (is.null(names(scenarios)))
    names(scenarios) <- paste0("S", seq_along(scenarios))

  base <- solve.model(cal)

  results <- lapply(names(scenarios), function(nm) {
    s   <- scenarios[[nm]]
    tau <- if (!is.null(s$tau))           s$tau           else cal$tau
    sp  <- if (!is.null(s$sp))            s$sp            else cal$sp
    sc  <- if (!is.null(s$sc))            s$sc            else cal$sc
    q   <- s$quota
    rd  <- if (!is.null(s$rent.domestic)) s$rent.domestic else 1

    cf  <- solve.model(cal, tau = tau, sp = sp, sc = sc,
                       quota = q, rent.domestic = rd)
    wf  <- welfare.change(base, cf, cal)

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
