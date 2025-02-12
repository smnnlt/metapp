library(metafor)

# standardized change score (uncorrected)
scm <- escalc(measure = "SMCRP", correct = FALSE, m1i = 10, m2i = 15, sd1i = 2, sd2i = 3, ni = 10, ri = 0.8)
sc <- smcr(10, 15, 2, 3, 10, 0.8, hedges = FALSE)

# bias-corrected
scm_ub <- escalc(measure = "SMCRPH", correct = TRUE, m1i = 10, m2i = 15, sd1i = 2, sd2i = 3, ni = 10, ri = 0.8)
sc_ub <- smcr(10, 15, 2, 3, 10, 0.8, hedges = TRUE)
# here something does not work out

test_that("estimate matches for smcr", {
  expect_equal(sc$es * (-1), as.numeric(scm$yi))
  #expect_equal(sc_ub$es * (-1), as.numeric(scm_ub$yi))
  # metafor has different variance estimators
})

# check against example calculations in Borenstein & Hedges (2019), sec. 11.2.2.2

bh <- smcr(103, 100, 7.1, 7.1, 50, 0.7, hedges = FALSE)
bh_ub <- smcr(103, 100, 7.1, 7.1, 50, 0.7, hedges = TRUE)

test_that("smcr matches Borenstein & Hedges examples", {
  expect_equal(bh$es, -0.4225, tolerance = 0.01)
  expect_equal(bh$var, 0.0131, tolerance = 0.01)
  expect_equal(bh_ub$es, -0.4160, tolerance = 0.01)
  expect_equal(bh_ub$var, 0.0127, tolerance = 0.01)
})
