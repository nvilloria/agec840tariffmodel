#' Print a formatted equilibrium solution
#'
#' `print.solution()` provides a formatted summary of one equilibrium solution,
#' including prices, quantities, and fiscal items.
#'
#' @param x a solution list of class \code{solution} returned by [solve.model()]
#' @param label optional label to display
#' @param ... further arguments passed to or from other methods
#'
#' @return The solution list (invisibly).
#' @rawNamespace export(print.solution)
print.solution <- function(x, label = "Solution", ...) {
  cat("\n===", label, "===\n")
  cat(sprintf("  Mode          : %s\n", x$mode))
  cat(sprintf("  Tariff (tau)  : %6.1f%%\n", x$tau * 100))
  cat(sprintf("  Prod. subsidy : %6.1f%%\n", x$sp  * 100))
  cat(sprintf("  Cons. subsidy : %6.1f%%\n", x$sc  * 100))
  if (!is.null(x$quota))
    cat(sprintf("  Quota level   : %10.2f\n", x$quota))
  cat("\n  Prices:\n")
  cat(sprintf("    Domestic (P_d)  : %.4f\n", x$p.d))
  cat(sprintf("    World    (P_w)  : %.4f\n", x$p.w))
  cat(sprintf("    Consumer (P_c)  : %.4f\n", x$p.c))
  cat(sprintf("    Producer (P_p)  : %.4f\n", x$p.p))
  cat("\n  Quantities:\n")
  cat(sprintf("    Domestic demand  (Q_d) : %10.2f\n", x$q.d))
  cat(sprintf("    Domestic supply  (Q_s) : %10.2f\n", x$q.s))
  cat(sprintf("    Net imports      (M_d) : %10.2f\n", x$m.d))
  cat("\n  Government accounts:\n")
  cat(sprintf("    Tariff revenue      : %10.2f\n", x$tariff.rev))
  cat(sprintf("    Prod. subsidy cost  : %10.2f\n", x$prod.sub.cost))
  cat(sprintf("    Cons. subsidy cost  : %10.2f\n", x$cons.sub.cost))
  if (x$mode == "quota")
    cat(sprintf("    Quota rent (total)  : %10.2f  [domestic share: %.0f%%]\n",
                x$quota.rent.total, x$rent.domestic * 100))
  invisible(x)
}
