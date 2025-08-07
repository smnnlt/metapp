library(metafor)

# mean difference (heteroscedastic)
mm <- escalc(measure = "MD", m1i = 10, m2i = 15, sd1i = 2, sd2i = 3, n1i = 11, n2i = 9)
m <- md(10, 15, 2, 3, 11, 9)

test_that("Cohen's d matches metafor results", {
  expect_equal(m$es, as.numeric(mm$yi))
  expect_equal(m$var, as.numeric(mm$vi))
})

# mean difference (homoscedastic)
mhm <- escalc(measure = "MD", vtype = "HO", m1i = 10, m2i = 15, sd1i = 2, sd2i = 3, n1i = 11, n2i = 9)
mh <- md(10, 15, 2, 3, 11, 9, var_homo = TRUE)

test_that("Cohen's d matches metafor results", {
  expect_equal(mh$es, as.numeric(mhm$yi))
  expect_equal(mh$var, as.numeric(mhm$vi))
})
