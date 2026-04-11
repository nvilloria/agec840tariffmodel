#' Print a formatted welfare decomposition
#'
#' `print.welfare()` provides a formatted welfare decomposition, showing
#' changes in consumer surplus, producer surplus, government revenue, and more.
#'
#' @param x a welfare list of class \code{welfare} returned by [welfare.change()]
#' @param label optional label to display
#' @param ... further arguments passed to or from other methods
#'
#' @return The welfare list (invisibly).
#' @rawNamespace export(print.welfare)
print.welfare <- function(x, label = "Welfare Change", ...) {
  cat("\n===", label, "===\n")
  cat(sprintf("  Delta CS (consumer surplus)   : %+10.2f\n", x$delta.cs))
  cat(sprintf("  Delta PS (producer surplus)   : %+10.2f\n", x$delta.ps))
  cat(sprintf("  Tariff revenue  (change)      : %+10.2f\n", x$tariff.rev))
  cat(sprintf("  Prod. subsidy cost (change)   : %+10.2f\n", x$prod.sub))
  cat(sprintf("  Cons. subsidy cost (change)   : %+10.2f\n", x$cons.sub))
  cat(sprintf("  Quota rents (domestic share)  : %+10.2f\n", x$quota.rents))
  cat("  ------------------------------------------------\n")
  cat(sprintf("  Net welfare                   : %+10.2f\n", x$net.welfare))
  cat(sprintf("  [memo] Terms-of-trade gain    : %+10.2f\n", x$tot.gain))
  cat(sprintf("  [memo] DW loss triangle B+D   : %+10.2f\n", x$dw.loss))
  invisible(x)
}
