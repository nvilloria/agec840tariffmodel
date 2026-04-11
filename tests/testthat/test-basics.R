test_that("calibrate.model returns expected values for simple benchmark", {
  cal <- calibrate.model(kd = 1000, ks = 700, md = 300,
                         eta.d = -0.30, eps.s = 0.50, eps.ms = 5.00,
                         tau = 0.10)
  expect_equal(round(cal$k.d, 3), 1000)
  expect_equal(round(cal$k.s, 3), 700)
  expect_equal(round(cal$eta.md, 3), -2.167)
})

test_that("solve.model returns the benchmark equilibrium at calibration", {
  cal <- calibrate.model(kd = 1000, ks = 700, md = 300,
                         eta.d = -0.30, eps.s = 0.50, eps.ms = 5.00,
                         tau = 0.10)
  base <- solve.model(cal)
  expect_equal(round(base$p.d, 3), 1.000)
  expect_equal(round(base$m.d, 3), 300.000)
})
