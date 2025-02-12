library(metafor)

# test ppc1 function: compare with metafor results

g1 <- ppc(0.7-0.5, 0.6-0.6, 0.1, 0.2, 9, 9, r = 0.77, type = 1, var_becker = FALSE) |> get_ci()
g1becker <- ppc(0.7-0.5, 0.6-0.6, 0.1, 0.2, 9, 9, r1 = 0.77, r2 = 0.77, type = 1, var_becker = TRUE) |> get_ci()
m1.1 <- escalc(measure="SMCR", m1i=0.7, m2i=0.5, sd1i=0.1, ni=9, ri=0.77)
m1.2 <- escalc(measure="SMCR", m1i=0.6, m2i=0.6, sd1i=0.2, ni=9, ri=0.77)

test_that("ppc1 gets same effect estimate regardless of variance estimation", {
  expect_equal(g1$es, g1becker$es)
})

test_that("ppc1 with variance approximation by Becker works", {
  expect_equal(g1becker$es, as.numeric(m1.1$yi - m1.2$yi))
  expect_equal(g1becker$var, as.numeric(m1.1$vi + m1.2$vi))
})

# test ppc2 function: compare with custom script
# modified from https://gist.github.com/mathijsdeen/8603cb55202ef7011013c4508de7b810

dPPC2 <- function(MpreT, MposT, MpreC, MposC, SpreT, SpreC, NT, NC, rho, correct = TRUE, CIlevel = .95){

  # inputs for d
  DF    <- NT + NC - 2
  #CP    <- ifelse(correct == TRUE, 1 - 3 / (4 * (DF) - 1), 1)
  CP <- metapp::j(DF)
  #CP <- ifelse(correct == TRUE, 1 - 3 / (4 * (DF) - 1), 1)
  SDpre <- sqrt(((NT - 1) * SpreT^2 + (NC - 1) * SpreC^2) / DF)

  # Morris (2008), Equation 8 (PPC2)
  d     <- CP * (((MposT - MpreT) - (MposC - MpreC)) / SDpre)

  # variance of d, chopped up because it's a long equation (Eq. 25 in Morris (2008))
  Vard1 <- 2 * CP^2 * (1 - rho)
  Vard2 <- ((NT + NC) / (NT * NC)) * (DF / (DF - 2))
  Vard3 <- 1 + d^2 / (2 * (1 - rho) * ((NT + NC) / (NT * NC)))
  Vard  <- Vard1 * Vard2 * Vard3 - d^2
  SEd   <- sqrt(Vard)
  CIlb  <- d - qnorm((1 + CIlevel) / 2) * SEd
  CIup  <- d + qnorm((1 + CIlevel) / 2) * SEd
  out   <- data.frame(d = d,
                      SE = SEd,
                      `lower bound` = CIlb,
                      `upper bound` = CIup,
                      N1 = NT,
                      N2 = NC)


  return(out)
}
m2 <- dPPC2(0.5, 0.7, 0.6, 0.6, 0.1, 0.2, 9, 9, 0.77)
g2 <- ppc(0.7-0.5, 0.6-0.6, 0.1, 0.2, 9, 9, r = 0.77, type = 2) |> get_ci()

test_that("ppc2 gets same estimate and variance as gist script", {
  expect_equal(g2$es, m2$d)
  expect_equal(g2$ci_low, m2$lower.bound)
  expect_equal(g2$ci_high, m2$upper.bound)
})
