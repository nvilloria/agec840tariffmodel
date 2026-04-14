test_that("calibrate_model returns expected values for simple benchmark", {
  cal <- calibrate_model(kd = 1000, ks = 700,
                         eta.d = -0.30, eps.s = 0.50, eps.ms = 5.00,
                         tau = 0.10)
  expect_equal(round(cal$k.d, 3), 1000)
  expect_equal(round(cal$k.s, 3), 700)
  expect_equal(round(cal$eta.md, 3), -2.167)
})

test_that("solve_taxes returns the benchmark equilibrium at calibration", {
  cal <- calibrate_model(kd = 1000, ks = 700,
                         eta.d = -0.30, eps.s = 0.50, eps.ms = 5.00,
                         tau = 0.10)
  base <- solve_taxes(cal)
  expect_equal(round(base$p.d, 3), 1.000)
  expect_equal(round(base$m.d, 3), 300.000)
})

test_that("solve_quota returns expected equilibrium", {
  cal <- calibrate_model(kd = 1000, ks = 700,
                         eta.d = -0.30, eps.s = 0.50, eps.ms = 5.00,
                         tau = 0.10)
  # Benchmark imports are 300. Restrict to 240.
  cf <- solve_quota(cal, quota = 240)
  expect_equal(round(cf$m.d, 3), 240.000)
  expect_gt(cf$p.d, 1.0) # Price should rise
})
