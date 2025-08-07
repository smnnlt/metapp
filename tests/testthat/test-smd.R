library(metafor)

# Cohen's d
cdm <- escalc(measure = "SMD", correct = FALSE, m1i = 10, m2i = 15, sd1i = 2, sd2i = 3, n1i = 11, n2i = 9)
cd <- smd(10, 15, 2, 3, 11, 9, hedges = FALSE)

test_that("Cohen's d matches metafor results", {
  expect_equal(cd$es, as.numeric(cdm$yi))
  expect_equal(cd$var, as.numeric(cdm$vi))
})

# Hedges' g (default metafor)
hgm <- escalc(measure = "SMD", m1i = 10, m2i = 15, sd1i = 2, sd2i = 3, n1i = 11, n2i = 9)
hg <- smd(10, 15, 2, 3, 11, 9)

test_that("Hedges' g matches metafor results", {
  expect_equal(hg$es, as.numeric(hgm$yi))
  expect_equal(hg$var, as.numeric(hgm$vi))
})

# Hedges' g (vartype 2)
hgm2 <- escalc(measure = "SMD", vtype = "LS2", m1i = 10, m2i = 15, sd1i = 2, sd2i = 3, n1i = 11, n2i = 9)
hg2 <- smd(10, 15, 2, 3, 11, 9, vartype = 2)

test_that("Hedges' g with different variance estimator matches metafor results", {
  expect_equal(hg2$es, as.numeric(hgm2$yi))
  expect_equal(hg2$var, as.numeric(hgm2$vi))
})
