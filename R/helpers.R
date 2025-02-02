#' Calculating the Hedges correction factor
#'
#' Calculates the Hedges correction factor for small sample bias
#'
#' @param x numeric, the degrees of freedom.
#' @param exact Whether to use the exact formula or an approximation. Defaults
#'   to true.
#' @returns A numeric, the bias correction factor.
#' @examples
#' j(20)
#'
#' @export
j <- function(x, exact = TRUE) {
  if (exact) {
    gamma(x/2) / (sqrt(x/2)*gamma((x-1)/2))
  } else {
    1 - (3 / (4 * x - 1))
  }
}

#' Get change score standard deviation from pre-post correlation
#'
#' Calculates the change score standard deviation of a group from the pre and
#' post standard deviations and the pre-post correlation.
#'
#' @param sd_pre,sd_post pre and post standard deviations.
#' @param r pre-post correlation.
#' @returns A numeric, the standard deviation of change scores.
#' @examples
#' r_to_sdd(sd_pre = 5, sd_post = 7, r = 0.9)
#'
#' @export
r_to_sdd <- function(sd_pre, sd_post, r) {
  sqrt(sd_pre^2+sd_post^2-2*r*sd_pre*sd_post)
}

#' Get pre-post correlation from change score standard deviation
#'
#' Calculates the pre-post correlation for a group from the pre, post and change
#' score standard deviation.
#'
#' @param sd_d standard deviation of change scores
#' @inheritParams r_to_sdd
#' @returns A numeric, the pre-post correlation.
#' @examples
#' sdd_to_r(sd_pre = 3, sd_post = 4, sd_d = 2)
#'
#' @export
sdd_to_r <- function(sd_pre, sd_post, sd_d) {
  (sd_pre^2 + sd_post^2 - sd_d^2)/(2*sd_pre*sd_post)
}

#' Get the confidence interval for a given effect size
#'
#' Calculates the confidence interval for
#'
#' @param es A data frame of the class mpp with an effect size and its variance.
#'   For example, the output of \code{\link{md}}, \code{\link{smd}},
#'   \code{\link{scr}}.
#' @param level the confidence level. Defaults to 0.95.
#' @param df The degrees of freedom. Defaults to NA, which assumes a normal
#'   distribution. Otherwise a t-distribution is assumed.
#' @returns A data.frame with the effect size (es), its variance (var) and the
#'   lower and upper limits of the confidence interval (ci_low, ci_high).
#' @examples
#' # calculate a SMD first
#' g <- smd(x1 = 10, x2 = 15, sd1 = 3, sd2 = 4, n1 = 11, n2 = 9)
#'
#' # calculate confidence interval
#' get_ci(g)
#'
#' @export
get_ci <- function(es, level = 0.95, df = NA) {
  if (is.na(df)) { # z distribution
    q <- stats::qnorm((1-level)/2)
  } else { # t-distribution
    q <- stats::qt((1-level)/2, df)
  }
  data.frame(
    es = es$es,
    var = es$var,
    ci_low = es$es + q * sqrt(es$var),
    ci_high = es$es - q * sqrt(es$var)
  )
}

#' Pool groups
#'
#' Pools the mean, standard deviation and sample size for two groups.
#'
#' @inheritParams md
#' @returns a data frame with the group mean (mean), standard deviation (sd) and sample size (n).
#' @examples
#' pool_groups(x1 = 10, x2 = 12, sd1 = 4, sd2 = 5, n1 = 10, n2 = 11)
#'
#' @export
pool_groups <- function(x1, x2, sd1, sd2, n1, n2) {
  n <- n1 + n2
  x <- (x1 * n1 + x2 * n2) / n
  sd <- sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2+(n1*n2/n)*(x1^2+x2^2-2*x1*x2))/(n-1))
  data.frame(
    x = x,
    sd = sd,
    n = n
  )
}
