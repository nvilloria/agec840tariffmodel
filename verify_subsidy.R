library(agec840tariffmodel)

# ── Benchmark & elasticities (same as homework) ──────────────────────────────
qs.bench <- 13456
md.bench <- 1467
qd.bench <- qs.bench + md.bench   # 14923

eta.d  <- -0.30
eps.s  <-  0.50
eps.ms <- 10.17

# ── Average tau 2020-2024 ─────────────────────────────────────────────────────
oecd <- data.frame(
  year = 2000:2024,
  Pw = c(220.68, 234.13, 206.79, 198.63, 222.89, 283.29, 374.76, 288.82,
         300.05, 427.91, 528.88, 633.16, 511.25, 417.99, 393.30, 322.79,
         432.76, 381.51, 303.19, 305.56, 317.24, 426.59, 447.97, 563.28, 490.74),
  MPD = c(120.11, 138.76, 151.99, 164.15, 136.91, 111.51, 68.00, 104.32,
          101.69,  73.03, 158.47, 124.34,  75.53,  19.84,  83.47, 135.44,
          106.09, 140.39, 153.66, 162.70, 166.40, 187.83, 204.10, 199.87, 211.77)
)
oecd$P   <- oecd$Pw + oecd$MPD
oecd$tau <- oecd$P / oecd$Pw - 1
tau.avg  <- mean(subset(oecd, year >= 2020)$tau)
cat("tau.avg:", round(tau.avg, 4), "\n\n")

# ── Calibrate at distorted benchmark ─────────────────────────────────────────
cal.dist <- calibrate_model(
  kd = qd.bench, ks = qs.bench,
  eta.d = eta.d, eps.s = eps.s, eps.ms = eps.ms,
  tau = tau.avg
)
cat("eta.md:", round(cal.dist$eta.md, 4), "\n\n")

# ── Baseline (reproduces benchmark) ──────────────────────────────────────────
base.dist <- solve_taxes(cal.dist)
cat("=== DISTORTED BASELINE (tariff = tau.avg) ===\n")
print_solution(base.dist, "Baseline")

# ── Free trade reference ──────────────────────────────────────────────────────
cft <- solve_taxes(cal.dist, tau = 0)
cat("\n=== FREE TRADE ===\n")
print_solution(cft, "Free trade")
wf.ft <- welfare_change(cft, base.dist)
print_welfare(wf.ft, "Welfare: Free trade -> Tariff baseline")

# ── Find production subsidy that matches Q_s of base.dist ────────────────────
target.qs <- base.dist$q.s
cat("\n=== PRODUCTION SUBSIDY SEARCH ===\n")
cat("Target Q_s (from tariff baseline):", round(target.qs, 2), "\n")
cat("Producer price under tariff (= P_d since sp=0):", round(base.dist$p.d, 6), "\n\n")

# Search over a fine grid
sp.grid <- seq(0, 0.80, by = 0.001)
qs.grid <- sapply(sp.grid, function(sp) {
  sol <- solve_taxes(cal.dist, tau = 0, sp = sp)
  sol$q.s
})

# Find sp that minimizes |Q_s - target|
idx <- which.min(abs(qs.grid - target.qs))
sp.approx <- sp.grid[idx]
cat("Grid search sp:", round(sp.approx, 4), "=", round(sp.approx*100, 2), "%\n")

# Refine with uniroot
sp.opt <- uniroot(function(sp) {
  sol <- solve_taxes(cal.dist, tau = 0, sp = sp)
  sol$q.s - target.qs
}, lower = 0.20, upper = 0.60)$root
cat("Refined sp:", round(sp.opt, 6), "=", round(sp.opt*100, 4), "%\n\n")

# ── Solution under production subsidy ────────────────────────────────────────
sub <- solve_taxes(cal.dist, tau = 0, sp = sp.opt)
cat("=== PRODUCTION SUBSIDY SOLUTION ===\n")
print_solution(sub, paste0("Production subsidy (sp = ", round(sp.opt*100,1), "%)"))

cat("\nProducer price under subsidy:", round(sub$p.d * (1 + sp.opt), 6), "\n")
cat("Producer price under tariff: ", round(base.dist$p.d, 6), "\n")
cat("Q_s under subsidy:", round(sub$q.s, 2), "| Target:", round(target.qs, 2), "\n\n")

# ── Welfare comparison (all vs free trade) ────────────────────────────────────
wf.tariff  <- welfare_change(cft, base.dist)
wf.subsidy <- welfare_change(cft, sub)

cat("=== WELFARE: Free trade -> Tariff ===\n")
print_welfare(wf.tariff, "Tariff program")

cat("\n=== WELFARE: Free trade -> Production subsidy ===\n")
print_welfare(wf.subsidy, "Production subsidy")

# ── Comparison table ─────────────────────────────────────────────────────────
cat("\n=== COMPARISON TABLE ===\n")
cat(sprintf("%-30s %12s %12s %12s\n", "Item", "Free Trade", "Tariff", "Prod. Subsidy"))
cat(strrep("-", 66), "\n")
cat(sprintf("%-30s %12s %12s %12s\n", "Tariff rate",
            "0%", paste0(round(tau.avg*100,1),"%"), "0%"))
cat(sprintf("%-30s %12s %12s %12s\n", "Subsidy rate",
            "0%", "0%", paste0(round(sp.opt*100,1),"%")))
cat(sprintf("%-30s %12.4f %12.4f %12.4f\n", "Domestic price P_d",
            cft$p.d, base.dist$p.d, sub$p.d))
cat(sprintf("%-30s %12.4f %12.4f %12.4f\n", "World price P_w",
            cft$p.w, base.dist$p.w, sub$p.w))
cat(sprintf("%-30s %12.4f %12.4f %12.4f\n", "Producer price",
            cft$p.d, base.dist$p.d * 1, sub$p.d * (1 + sp.opt)))
cat(sprintf("%-30s %12.0f %12.0f %12.0f\n", "Q_s",
            cft$q.s, base.dist$q.s, sub$q.s))
cat(sprintf("%-30s %12.0f %12.0f %12.0f\n", "Q_d",
            cft$q.d, base.dist$q.d, sub$q.d))
cat(sprintf("%-30s %12.0f %12.0f %12.0f\n", "M_d",
            cft$m.d, base.dist$m.d, sub$m.d))
cat(sprintf("%-30s %12s %12.2f %12.2f\n", "Delta CS (vs FT)",
            "—", wf.tariff$d.cs, wf.subsidy$d.cs))
cat(sprintf("%-30s %12s %12.2f %12.2f\n", "Delta PS (vs FT)",
            "—", wf.tariff$d.ps, wf.subsidy$d.ps))
cat(sprintf("%-30s %12s %12.2f %12.2f\n", "Tariff revenue",
            "—", wf.tariff$tariff.rev, wf.subsidy$tariff.rev))
cat(sprintf("%-30s %12s %12.2f %12.2f\n", "Subsidy cost",
            "—", wf.tariff$subsidy.cost, wf.subsidy$subsidy.cost))
cat(sprintf("%-30s %12s %12.2f %12.2f\n", "Terms-of-trade gain",
            "—", wf.tariff$tot.gain, wf.subsidy$tot.gain))
cat(sprintf("%-30s %12s %12.2f %12.2f\n", "Net welfare",
            "—", wf.tariff$net.welfare, wf.subsidy$net.welfare))

# ── Check sp vs tau.avg ───────────────────────────────────────────────────────
cat("\n=== KEY COMPARISONS ===\n")
cat("sp.opt:", round(sp.opt*100, 2), "%\n")
cat("tau.avg:", round(tau.avg*100, 2), "%\n")
cat("sp < tau? ", sp.opt < tau.avg, "\n")
cat("NW subsidy > NW tariff?", wf.subsidy$net.welfare > wf.tariff$net.welfare, "\n")

# ── Revenue-max formula check ─────────────────────────────────────────────────
cat("\n=== REVENUE-MAX TARIFF FORMULA CHECK ===\n")
eta.md <- cal.dist$eta.md
tau.rev.formula <- (eps.ms + abs(eta.md)) / (eps.ms * (abs(eta.md) - 1))
cat("eta.md:", round(eta.md, 4), "\n")
cat("tau_rev (formula):", round(tau.rev.formula*100, 2), "%\n")
cat("tau* (welfare max):", round(1/eps.ms*100, 2), "%\n")
