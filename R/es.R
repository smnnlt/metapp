#' Calculating a mean difference
#'
#' Calculates a raw MD effect size for a simple group comparison.
#'
#' @param x1,x2 means of the first and seconds group.
#' @param sd1,sd2 standard deviations of the first and second group.
#' @param n1,n2 sample sizes of the first and second group.
#' @param var_homo whether an variance estimator with pooled standard deviation
#'   should be used, that assumes homoscedasticity for the population variance
#'   (defaults to TRUE).
#' @returns A data frame of the class mpp with the effect size (es) and its
#'   variance (var).
#' @examples
#' md(x1 = 10, x2 = 15, sd1 = 3, sd2 = 4, n1 = 11, n2 = 9)
#'
#' @export
md <- function(x1, x2, sd1, sd2, n1, n2, var_homo = TRUE) {
  md <- x1 - x2
  if (var_homo) { # homogeneous variances: pooled sd
    sd_pooled <- sd_pooled(sd1, sd2, n1, n2)
    var <- ((n1+n2)/(n1*n2))*(s_pooled^2)
  } else { # heterogeneous variances: average sd
    var <- (sd1^2/n1) + (sd2^2/n2)
  }
  out <- data.frame(
    es = md,
    var = var
  )
  class(out) <- c("mpp", "data.frame")
  out
}

#' Calculating a standardized mean difference
#'
#' Calculates a SMD effect size for a simple group comparison.
#'
#' @param hedges whether an bias correction for small sample sizes according to
#'   Hedges should be applied (defaults to TRUE).
#' @param homo whether the pooled sd should be used as the standardizer
#'   (assuming homogeneous population variances). Defaults to TRUE.
#' @param vartype whether to use the variance estimator proposed by Hedges (1,
#'   as is default in metafor), the variance estimator from Borenstein (2), or
#'   the default in RevMan (3)). Defaults to 1.
#' @param exact whether to use the exact formula for the small sample size
#'   correction (otherwise an approximation is used). This is only relevant when
#'   \code{hedges=TRUE}. Defaults to TRUE.
#' @returns A data frame of the class mpp with the effect size (es) and its
#'   variance (var).
#' @inheritParams md
#' @examples
#' smd(x1 = 10, x2 = 15, sd1 = 3, sd2 = 4, n1 = 11, n2 = 9)
#'
#' @export
smd <- function(x1, x2, sd1, sd2, n1, n2, hedges = TRUE, homo = TRUE, vartype = 1, exact = TRUE) {
  md <- x1 - x2
  if (homo) {
    s <- sd_pooled(sd1, sd2, n1, n2)
  } else {
    s <- sd_avg(sd1, sd2)
  }
  smd <- md/s
  var <- ((n1+n2)/(n1*n2))+(smd^2/(2*n1+2*n2))
  if (hedges) {
    j_res <- j(n1+n2-2, exact = exact)
    smd <- j_res * smd
    if (vartype == 1) {
      # Hedges 82 plugs g (j*d) into the var estimation
      var <- ((n1+n2)/(n1*n2))+(smd^2/(2*n1+2*n2))
    } else if (vartype == 2) {
      # Borenstein uses var(d) and then correct by j^2
      var <- j_res^2 * var
    } else if (vartype == 3) {
      # RevMan uses another variant
      var <- ((n1+n2)/(n1*n2))+(smd^2/(2*(n1+n2-3.94)))
    } else {
      stop("vartype must be one of: 1, 2, 3")
    }
  }
  out <- data.frame(
    es = smd,
    var = var
  )
  class(out) <- c("mpp", "data.frame")
  out
}

#' Calculating a standardized change score difference (raw score based
#' standardization)
#'
#' Calculates a standardized change score group comparison for pre-post designs
#' with a raw score based standardization (see Morris 2008).
#'
#' @param x1d,x2d mean changes of the first and second group
#' @param sd1pre,sd2pre pre-score standard deviations of the first and second
#'   group
#' @param r1,r2,r pre-post correlation of first and second group (for
#'   \code{type=1}), or for both groups (for \code{type=2}).
#' @param type Type of effect size, see Morris (2008). (1) for the difference of
#'   standardized mean changes of each group, (2) for a standardization using
#'   the pooled pre sd. Defaults to 2.
#' @param var_becker For \code{type=1}, whether the approximate variance
#'   estimator by Becker (1988) should be used (calculating variances for each
#'   group and then adding them up). Otherwise uses the variance estimation by
#'   Morris (2008, Typ ppc1). See Morris 2000 for a comparison. Defaults to
#'   FALSE.
#' @returns A data frame of the class mpp with the effect size (es) and its
#'   variance (var).
#' @inheritParams md
#' @examples
#' ppc(x1d = 5, x2d = 2, sd1pre = 3, sd2pre = 4, n1 = 20, n2 = 24, r = 0.8)
#'
#' @export
ppc <- function(x1d, x2d, sd1pre, sd2pre, n1, n2, r = NA, r1 = NA, r2 = NA, type = 2, var_becker = FALSE) {
  if (type == 1) {
    g1 <- j(n1-1) * x1d / sd1pre
    g2 <- j(n2-1) * x2d / sd2pre
    g <- g1 - g2
    if (var_becker) {
      var <- ((2*(1-r1))/n1)+g1^2/(2*n1)+((2*(1-r2))/n2)+g2^2/(2*n2)
    } else {
      # Morris assumes one correlation
      var <- j(n1-1)^2*((2*(1-r))/n1)*((n1-1)/(n1-3))*(1+(n1*g1^2)/(2*(1-r)))-g1^2+j(n2-1)^2*((2*(1-r))/n2)*((n2-1)/(n2-3))*(1+(n2*g2^2)/(2*(1-r)))-g2^2
    }
  } else if (type == 2) {
    df <- n1 + n2 - 2
    s_pooled <- sd_pooled(sd1pre, sd2pre, n1, n2)
    g <- j(df)*((x1d-x2d)/s_pooled)
    var <- 2*j(df)^2*(1-r)*((n1+n2)/(n1*n2))*(df/(df-2))*(1+(g^2/(2*(1-r)*((n1+n2)/(n1*n2)))))-g^2
  }
  out <- data.frame(
    es = g,
    var = var
  )
  class(out) <- c("mpp", "data.frame")
  out
}

#' Calculating a standardized change score (raw score based standardization)
#'
#' Calculates the standardized change score with raw score standardization for a
#' single group.
#'
#' @param xpre,xpost mean of the pre and post score
#' @param sdpre,sdpost standard deviation of the pre and post score
#' @param n sample size
#' @param r pre-post correlation
#'
#' @inheritParams smd
#' @examples
#' smcr(10, 12, 2, 3, 20, 0.8)
#'
#' @export
smcr <- function(xpre, xpost, sdpre, sdpost, n, r, hedges = TRUE, exact = TRUE) {
  s_within <- sd_avg(sdpre, sdpost)
  d <- (xpost - xpre) / s_within
  var <- 2*(1-r)*((1/n)+(d^2/2*n))
  if (hedges) {
    j <- j(n-1, exact = exact)
    d <- j * d
    var <- j^2 * var
  }
  out <- data.frame(
    es = d,
    var = var
  )
  class(out) <- c("mpp", "data.frame")
  out
}
